# Rmds/artifacts/export_for_tableau_and_slides.R
# Run this AFTER your artifact generation chunk has saved project_artifacts.RData
# Produces: sanitized CSV for Tableau Public + canonical PNGs for slides and Tableau
library(here)
library(tidyverse)
library(ggplot2)
library(pROC)
library(digest)   # for hashing PseudoID if you prefer hashing
library(glue)

artifacts_dir <- normalizePath(here::here("Rmds/artifacts"), winslash = "/", mustWork = FALSE)
proj_file <- file.path(artifacts_dir, "project_artifacts.RData")
if (!file.exists(proj_file)) stop("project_artifacts.RData not found. Run artifact generation first.")

# load artifacts into env
env <- new.env()
load(proj_file, envir = env)

# helper: safe get
safe_get <- function(name) if (exists(name, envir = env)) get(name, envir = env) else NULL

# test_baked
tb <- safe_get("test_baked")
if (is.null(tb) || !is.data.frame(tb)) stop("test_baked not found in artifacts; cannot proceed.")

# Ensure pred_prob and target_attrit numeric
if ("pred_prob" %in% names(tb)) tb$pred_prob <- as.numeric(tb$pred_prob)
if ("target_attrit" %in% names(tb)) tb$target_attrit <- as.numeric(tb$target_attrit)

# Create output PNG paths
png_path <- function(name) file.path(artifacts_dir, paste0(name, ".png"))

# 1) Decile counts and observed attrition by decile (use decile in artifact or compute)
decile_mode <- (safe_get("artifact_provenance")$decile_mode %||% "ntile")
if (!"pred_decile" %in% names(tb) && "pred_prob" %in% names(tb)) {
  if (decile_mode == "ntile") tb <- tb %>% mutate(pred_decile = dplyr::ntile(pred_prob, 10)) else tb <- tb %>% mutate(pred_decile = as.integer(cut(pred_prob, breaks = seq(0,1,by=0.1), include.lowest = TRUE, labels = 1:10)))
}
tb$pred_decile <- factor(tb$pred_decile, levels = as.character(1:10))

# Decile counts plot
decile_counts_df <- tb %>% count(pred_decile) %>% arrange(as.integer(as.character(pred_decile)))
decile_counts_plot <- ggplot(decile_counts_df, aes(x = pred_decile, y = n, text = paste0("Decile: ", pred_decile, "<br>Count: ", n))) +
  geom_col(fill = "#2b8cbe") + geom_text(aes(label = n), vjust = -0.3, size = 3) +
  labs(x = "Decile", y = "Count", title = "Decile counts (test set)") + theme_minimal()
ggsave(png_path("decile_counts"), plot = decile_counts_plot, width = 8, height = 5, dpi = 300)

# Decile observed attrition plot
decile_obs_df <- tb %>% group_by(pred_decile) %>% summarise(obs = mean(as.numeric(target_attrit), na.rm = TRUE), n = n(), .groups = "drop")
decile_obs_plot <- ggplot(decile_obs_df, aes(x = pred_decile, y = obs, text = paste0("Decile: ", pred_decile, "<br>Observed: ", round(obs,4), "<br>Count: ", n))) +
  geom_col(fill = "#fdae61") + geom_text(aes(label = scales::percent(obs, accuracy = 0.1)), vjust = -0.3, size = 3) +
  labs(x = "Decile", y = "Observed attrition", title = "Observed attrition by decile (test set)") + theme_minimal()
ggsave(png_path("decile_obs"), plot = decile_obs_plot, width = 8, height = 5, dpi = 300)

# 2) Calibration plot: use cal_plot if present, else build
cal_plot <- safe_get("cal_plot")
if (!is.null(cal_plot) && inherits(cal_plot, "ggplot")) {
  ggsave(png_path("reliability"), plot = cal_plot, width = 8, height = 5, dpi = 300)
} else {
  if ("pred_prob" %in% names(tb) && "target_attrit" %in% names(tb)) {
    cal_df <- tb %>% mutate(bin = dplyr::ntile(pred_prob, 10)) %>% group_by(bin) %>% summarise(mean_prob = mean(pred_prob, na.rm = TRUE), obs = mean(target_attrit, na.rm = TRUE), n = n(), .groups = "drop")
    p_cal <- ggplot(cal_df, aes(x = mean_prob, y = obs)) + geom_point(size = 2, color = "#2b8cbe") + geom_line(color = "#2b8cbe") + geom_abline(slope = 1, intercept = 0, linetype = "dashed") + theme_minimal() + labs(x = "Mean predicted probability", y = "Observed event rate", title = "Reliability diagram (deciles)")
    ggsave(png_path("reliability"), plot = p_cal, width = 8, height = 5, dpi = 300)
  } else {
    message("Skipping reliability PNG: pred_prob or target_attrit missing.")
  }
}

# 3) ROC overlay: if roc_xgb or other roc objects exist, build overlay; else skip
roc_objs <- list()
if (exists("roc_xgb", envir = env)) roc_objs$roc_xgb <- get("roc_xgb", envir = env)
# also check for other named roc objects
for (nm in c("roc_base","roc_enh","roc_rf","roc_xgb")) {
  if (exists(nm, envir = env)) {
    obj <- get(nm, envir = env)
    if (inherits(obj, "roc")) roc_objs[[nm]] <- obj
  }
}
if (length(roc_objs) > 0) {
  p <- ggplot() + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70")
  cols <- c(roc_base="black", roc_enh="blue", roc_rf="darkgreen", roc_xgb="red")
  for (nm in names(roc_objs)) {
    roc_obj <- roc_objs[[nm]]
    df <- data.frame(tpr = rev(roc_obj$sensitivities), fpr = rev(1 - roc_obj$specificities))
    p <- p + geom_line(data = df, aes(x = fpr, y = tpr), color = cols[nm] %||% "black", size = 1)
  }
  p <- p + labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC overlay (test set)") + theme_minimal()
  ggsave(png_path("roc_overlay"), plot = p, width = 8, height = 6, dpi = 300)
} else {
  message("No ROC objects found; skipping ROC overlay PNG.")
}

# 4) Varimp plots: top20_rf and top20_xgb if present
top20_rf <- safe_get("top20_rf")
if (!is.null(top20_rf) && is.data.frame(top20_rf)) {
  rf_imp <- top20_rf
  if (!all(c("Feature","Overall") %in% names(rf_imp))) names(rf_imp)[1:2] <- c("Feature","Overall")
  p_rf <- ggplot(rf_imp, aes(x = reorder(Feature, Overall), y = Overall)) + geom_col(fill = "#2b8cbe") + coord_flip() + labs(title = "Random Forest top predictors", x = NULL, y = "Importance") + theme_minimal()
  ggsave(png_path("rf_varimp"), plot = p_rf, width = 8, height = 6, dpi = 300)
} else {
  message("top20_rf not found; skipping RF varimp PNG.")
}

top20_xgb <- safe_get("top20_xgb")
if (!is.null(top20_xgb) && is.data.frame(top20_xgb)) {
  xgb_imp <- top20_xgb
  # try to find a numeric importance column
  valcol <- intersect(c("Gain","Importance","Gain.1","Overall"), names(xgb_imp))[1]
  if (is.null(valcol)) valcol <- names(xgb_imp)[2]
  names(xgb_imp)[1:2] <- c("Feature", valcol)
  p_xgb <- ggplot(xgb_imp, aes(x = reorder(Feature, !!sym(valcol)), y = !!sym(valcol))) + geom_col(fill = "#d73027") + coord_flip() + labs(title = "XGBoost top predictors", x = NULL, y = valcol) + theme_minimal()
  ggsave(png_path("xgb_varimp"), plot = p_xgb, width = 8, height = 6, dpi = 300)
} else {
  message("top20_xgb not found; skipping XGB varimp PNG.")
}

# 5) SHAP example: if shap_vals exists, create a single-row bar chart for the top contributors
shap_vals <- safe_get("shap_vals")
if (!is.null(shap_vals) && (is.matrix(shap_vals) || is.data.frame(shap_vals))) {
  shap_df <- as.data.frame(shap_vals)
  idx <- 1
  if (nrow(shap_df) >= 1) {
    row_shap <- shap_df[idx, , drop = FALSE]
    shap_long <- tibble::enframe(as.numeric(row_shap[1,]), name = "feature", value = "shap") %>% arrange(desc(abs(shap)))
    shap_long_top <- head(shap_long, 20)
    p_shap <- ggplot(shap_long_top, aes(x = reorder(feature, shap), y = shap, fill = shap > 0)) + geom_col() + coord_flip() + scale_fill_manual(values = c("TRUE"="#d73027","FALSE"="#2b8cbe"), guide = "none") + labs(title = "Local SHAP example (top features)", x = NULL, y = "SHAP contribution") + theme_minimal()
    ggsave(png_path("shap_example"), plot = p_shap, width = 8, height = 6, dpi = 300)
  }
} else {
  message("shap_vals not found; skipping SHAP example PNG.")
}

# 6) Export sanitized CSV for Tableau Public
# Decide: drop PseudoID (recommended) or hash it. We'll drop by default for public.
tb_for_tableau <- tb

# Remove or hash PseudoID for Tableau Public
if ("PseudoID" %in% names(tb_for_tableau)) {
  # Option: drop PseudoID for public
  tb_for_tableau$PseudoID <- NULL
  # If you prefer hashing instead of dropping, uncomment:
  # tb_for_tableau$PseudoID <- sapply(as.character(tb$PseudoID), digest, algo = "md5")
}

# Remove any obvious internal file paths or system columns
sys_cols <- grep("proj_file|path|artifact", names(tb_for_tableau), ignore.case = TRUE, value = TRUE)
if (length(sys_cols) > 0) tb_for_tableau <- tb_for_tableau %>% select(-all_of(sys_cols))

# Small-cell suppression: if any group has < 5 rows, mask sensitive numeric values (example)
# (For this fake dataset it's not necessary, but code is here for reference)
# counts_by_decile <- tb_for_tableau %>% group_by(pred_decile) %>% summarise(n = n())
# small_deciles <- counts_by_decile %>% filter(n < 5) %>% pull(pred_decile)
# if (length(small_deciles) > 0) {
#   tb_for_tableau <- tb_for_tableau %>% mutate(pred_prob = ifelse(pred_decile %in% small_deciles, NA_real_, pred_prob))
# }

csv_out <- file.path(artifacts_dir, "test_baked_for_tableau_public.csv")
write.csv(tb_for_tableau, file = csv_out, row.names = FALSE)
message("Wrote sanitized CSV for Tableau Public: ", csv_out)

# 7) Summary message
message("Export complete. PNGs written to: ", artifacts_dir)
message("Files created (if available): decile_counts.png, decile_obs.png, roc_overlay.png, reliability.png, rf_varimp.png, xgb_varimp.png, shap_example.png")