#!/usr/bin/env Rscript
# run_full_pipeline_and_render.R
#
# Single-entry R script to:
#  1) run nested cross-validation tuning for a Random Forest,
#  2) refit final tuned model on full training set and evaluate on held-out test set,
#  3) compute PDPs and SHAP summaries for top predictors,
#  4) run Moran's I spatial residual check (if centroids available),
#  5) save artifacts to models/ and figures/,
#  6) render the R Markdown report so the PDF includes tuned results and plots.
#
# Usage:
#   Rscript run_full_pipeline_and_render.R
#
# Notes:
#  - Expects preprocessed baked CSVs at models/train_baked.csv and models/test_baked.csv
#    (the Rmd preprocessing step writes these).
#  - Writes tuned model to models/rf_tuned_m05.rds, PDP PNGs to figures/, SHAP CSV to models/.
#  - Adjust parallel workers and tuning grid size for your machine.
#  - Set environment variable R_FUTURE_WORKERS or edit `workers` below to control parallelism.

options(warn = 1)
suppressPackageStartupMessages({
  library(tidymodels)
  library(dplyr)
  library(purrr)
  library(readr)
  library(furrr)
  library(vip)
  library(fastshap)
  library(pdp)
  library(sf)
  library(spdep)
  library(rlang)
  library(glue)
  library(stringr)
})

set.seed(2026)

# Paths and filenames
train_path <- "models/train_baked.csv"
test_path  <- "models/test_baked.csv"
tuned_model_path <- "models/rf_tuned_m05.rds"
tuned_test_metrics_path <- "models/rf_tuned_test_metrics.csv"
nested_cv_summary_path <- "models/rf_nested_cv_outer_summary.csv"
shap_values_path <- "models/shap_values_sample.csv"
shap_summary_path <- "models/shap_summary.csv"
pdp_fig_dir <- "figures"
moran_results_path <- "models/moran_results.rds"
rmd_file <- "M05-Initial-Models-Report-—-County-Childhood-Vaccination-Uptake_GBruder.Rmd"
output_pdf <- "M05-Initial-Models-Report-—-County-Childhood-Vaccination-Uptake_GBruder.pdf"

dir.create("models", showWarnings = FALSE, recursive = TRUE)
dir.create(pdp_fig_dir, showWarnings = FALSE, recursive = TRUE)

# Helper: safe read CSV with informative error
safe_read_csv <- function(path) {
  if (!file.exists(path)) stop(glue::glue("Required file not found: {path}"))
  readr::read_csv(path, show_col_types = FALSE)
}

# Load baked datasets
cat("Loading baked train/test sets...\n")
train_baked <- safe_read_csv(train_path)
test_baked  <- safe_read_csv(test_path)

# Identify outcome and predictors
outcome <- "coverage_estimate"
if (!outcome %in% names(train_baked)) stop(glue::glue("Outcome column '{outcome}' not found in train_baked.csv"))
predictors <- setdiff(names(train_baked), c(outcome, "fips", "outcome_q"))

cat("Predictors detected:", paste(predictors, collapse = ", "), "\n")

# ---------------------------------------------------------------------
# 1) Nested cross-validation (outer estimation) and final tuning on full training
# ---------------------------------------------------------------------
tryCatch({
  cat("Starting nested cross-validation tuning (outer folds)...\n")
  
  # Parallel plan: use available cores but be conservative
  workers <- as.integer(Sys.getenv("R_FUTURE_WORKERS", unset = "4"))
  if (is.na(workers) || workers < 1) workers <- 1
  future::plan(future::multisession, workers = workers)
  cat("Using", workers, "parallel workers for tuning.\n")
  
  # Outer folds
  outer_folds <- vfold_cv(train_baked, v = 5, strata = outcome)
  
  # Tunable RF spec
  rf_tune_spec <- rand_forest(mode = "regression", trees = 500) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_args(mtry = tune(), min_n = tune(), sample_size = tune())
  
  rf_wf <- workflow() %>%
    add_model(rf_tune_spec) %>%
    add_formula(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))))
  
  # Parameter ranges and grid (random)
  p <- length(predictors)
  rf_params <- parameters(dials::mtry(range = c(1, p)),
                          dials::min_n(range = c(1, 20)),
                          dials::sample_prop())
  # Use a moderate grid size; increase for final runs
  grid_size <- as.integer(Sys.getenv("RF_GRID_SIZE", unset = "50"))
  rf_grid <- dials::grid_random(rf_params, size = grid_size)
  
  # Function to run inner tuning and evaluate on outer assessment
  outer_results <- map(outer_folds$splits, function(split_i) {
    analysis_i <- analysis(split_i)
    assessment_i <- assessment(split_i)
    inner_folds <- vfold_cv(analysis_i, v = 5, strata = outcome)
    
    tune_res <- tune_grid(
      rf_wf,
      resamples = inner_folds,
      grid = rf_grid,
      metrics = metric_set(rmse),
      control = control_grid(save_pred = TRUE, verbose = TRUE)
    )
    
    best <- select_best(tune_res, "rmse")
    final_wf <- finalize_workflow(rf_wf, best)
    final_fit <- fit(final_wf, data = analysis_i)
    
    preds_i <- predict(final_fit, new_data = assessment_i) %>% bind_cols(assessment_i %>% select(all_of(outcome)))
    metrics_i <- metrics(preds_i, truth = !!sym(outcome), estimate = .pred)
    list(best = best, metrics = metrics_i)
  })
  
  # Aggregate outer metrics and save
  outer_metrics <- map_dfr(outer_results, "metrics")
  outer_summary <- outer_metrics %>% group_by(.metric) %>% summarise(mean = mean(.estimate, na.rm = TRUE), sd = sd(.estimate, na.rm = TRUE))
  readr::write_csv(outer_summary, nested_cv_summary_path)
  cat("Nested CV outer summary saved to:", nested_cv_summary_path, "\n")
  
  # Re-run tuning on full training to select final hyperparameters (practical approach)
  cat("Running tuning on full training set to select final hyperparameters...\n")
  tune_res_full <- tune_grid(
    rf_wf,
    resamples = vfold_cv(train_baked, v = 5, strata = outcome),
    grid = rf_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
  best_full <- select_best(tune_res_full, "rmse")
  final_wf_full <- finalize_workflow(rf_wf, best_full)
  final_fit_full <- fit(final_wf_full, data = train_baked)
  
  # Save tuned model
  saveRDS(final_fit_full, tuned_model_path)
  cat("Tuned model saved to:", tuned_model_path, "\n")
  
  # Evaluate on held-out test set and save metrics
  preds_test <- predict(final_fit_full, new_data = test_baked) %>% bind_cols(test_baked %>% select(all_of(outcome)))
  test_metrics <- metrics(preds_test, truth = !!sym(outcome), estimate = .pred)
  readr::write_csv(test_metrics, tuned_test_metrics_path)
  cat("Tuned model test metrics saved to:", tuned_test_metrics_path, "\n")
  
}, error = function(e) {
  future::plan(future::sequential)
  stop(glue::glue("Nested CV tuning failed: {conditionMessage(e)}"))
}, finally = {
  # reset plan to sequential to avoid background workers lingering
  future::plan(future::sequential)
})

# ---------------------------------------------------------------------
# 2) SHAP and PDP for top predictors (using tuned model)
# ---------------------------------------------------------------------
tryCatch({
  cat("Computing PDPs and SHAP summaries for top predictors...\n")
  if (!file.exists(tuned_model_path)) stop("Tuned model not found; aborting SHAP/PDP step.")
  rf_final <- readRDS(tuned_model_path)
  
  # Extract fitted parsnip model if available for variable importance
  rf_parsnip <- tryCatch(extract_fit_parsnip(rf_final$.workflow[[1]]), error = function(e) NULL)
  if (!is.null(rf_parsnip) && !is.null(rf_parsnip$fit)) {
    vi_tbl <- vip::vi(rf_parsnip$fit) %>% as_tibble() %>% rename(variable = Variable, importance = Importance) %>% arrange(desc(importance))
    top_preds <- vi_tbl$variable[1:min(3, nrow(vi_tbl))]
  } else {
    top_preds <- predictors[1:min(3, length(predictors))]
  }
  cat("Top predictors for interpretability:", paste(top_preds, collapse = ", "), "\n")
  
  # PDPs
  for (pred in top_preds) {
    pdp_obj <- pdp::partial(rf_final, pred.var = pred, train = train_baked, grid.resolution = 20, progress = "none")
    p <- autoplot(pdp_obj) + labs(title = paste("Partial dependence:", pred), x = pred, y = "Predicted coverage")
    fig_file <- file.path(pdp_fig_dir, paste0("pdp_", str_replace_all(pred, "[^A-Za-z0-9_]", "_"), ".png"))
    ggsave(filename = fig_file, plot = p, width = 7, height = 4, dpi = 300)
    cat("Saved PDP:", fig_file, "\n")
  }
  
  # SHAP explanations (sample for speed)
  predict_fun <- function(object, newdata) {
    predict(object, newdata = newdata)$.pred
  }
  X <- train_baked %>% select(all_of(top_preds))
  sample_n <- min(500, nrow(X))
  set.seed(2026)
  X_sample <- X %>% slice_sample(n = sample_n)
  
  # fastshap explain; nsim can be reduced for speed
  nsim <- as.integer(Sys.getenv("SHAP_NSIM", unset = "100"))
  shap_vals <- fastshap::explain(rf_final, X = X_sample, pred_wrapper = predict_fun, nsim = nsim)
  shap_df <- as.data.frame(shap_vals)
  readr::write_csv(as_tibble(shap_df), shap_values_path)
  shap_summary <- tibble::tibble(variable = names(shap_df), mean_abs_shap = colMeans(abs(shap_df)))
  readr::write_csv(shap_summary, shap_summary_path)
  cat("Saved SHAP values and summary to:", shap_values_path, "and", shap_summary_path, "\n")
  
}, error = function(e) {
  warning(glue::glue("SHAP/PDP step failed: {conditionMessage(e)}"))
})

# ---------------------------------------------------------------------
# 3) Moran's I spatial residual check (if centroids available)
# ---------------------------------------------------------------------
tryCatch({
  centroids_path <- "data_clean/county_centroids.csv"
  if (!file.exists(centroids_path)) {
    cat("County centroids not found at", centroids_path, "- skipping Moran's I.\n")
  } else {
    cat("Running Moran's I spatial residual check...\n")
    centroids <- readr::read_csv(centroids_path, show_col_types = FALSE) %>% mutate(fips = as.character(fips))
    # Ensure preds_test exists; if not, compute using tuned model
    if (!exists("preds_test")) {
      if (!exists("rf_final")) stop("No predictions available and tuned model missing.")
      preds_test <- predict(rf_final, new_data = test_baked) %>% bind_cols(test_baked %>% select(fips, all_of(outcome)))
      preds_test <- preds_test %>% rename(pred = .pred, truth = !!sym(outcome))
    }
    resid_df <- preds_test %>% mutate(residual = pred - truth) %>% left_join(centroids, by = "fips") %>% filter(!is.na(lon) & !is.na(lat))
    if (nrow(resid_df) < 10) {
      warning("Insufficient matched residuals with centroids to compute Moran's I; skipping.")
    } else {
      sf_pts <- sf::st_as_sf(resid_df, coords = c("lon", "lat"), crs = 4326)
      coords <- sf::st_coordinates(sf_pts)
      nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 5))
      lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      moran_res <- spdep::moran.test(sf_pts$residual, lw, zero.policy = TRUE)
      saveRDS(moran_res, moran_results_path)
      cat("Moran's I result saved to:", moran_results_path, "\n")
      print(moran_res)
    }
  }
}, error = function(e) {
  warning(glue::glue("Moran's I step failed: {conditionMessage(e)}"))
})

# ---------------------------------------------------------------------
# 4) Render the R Markdown report so the PDF includes tuned results and plots
# ---------------------------------------------------------------------
tryCatch({
  cat("Rendering R Markdown report to PDF...\n")
  if (!file.exists(rmd_file)) stop(glue::glue("Rmd file not found: {rmd_file}"))
  rmarkdown::render(rmd_file, output_file = output_pdf, envir = new.env())
  cat("Report rendered to:", output_pdf, "\n")
}, error = function(e) {
  stop(glue::glue("Rendering Rmd failed: {conditionMessage(e)}"))
})

cat("All steps complete.\n")

