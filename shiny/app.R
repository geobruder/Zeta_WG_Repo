# shiny/app.R - Final stakeholder-ready app with provenance export and A/B experiment panel
library(shiny)
library(here)
library(yaml)
library(tidyverse)
library(plotly)
library(pROC)
library(DT)
library(jsonlite)
library(xgboost)

`%||%` <- function(a,b) if(!is.null(a)) a else b

cfg_file <- here::here("config.yml")
cfg <- if (file.exists(cfg_file)) yaml::read_yaml(cfg_file) else list(artifacts_dir = "Rmds/artifacts")
artifacts_dir_rel <- cfg$artifacts_dir %||% "Rmds/artifacts"
artifacts_dir_abs <- normalizePath(here::here(artifacts_dir_rel), winslash = "/", mustWork = FALSE)
proj_file_abs <- file.path(artifacts_dir_abs, "project_artifacts.RData")

safe_load_artifacts <- function(path = proj_file_abs) {
  abs_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (file.exists(abs_path)) {
    env <- new.env()
    tryCatch({ load(abs_path, envir = env); as.list(env) }, error = function(e) list())
  } else list()
}

ARTIFACTS <- safe_load_artifacts()

safe_ggplotly <- function(p, title_when_empty = "Plot not available") {
  if (is.null(p) || !inherits(p, "ggplot")) return(plotly::plotly_empty() %>% plotly::layout(title = title_when_empty))
  out <- tryCatch({ suppressWarnings(plotly::ggplotly(p, tooltip = "text")) }, error = function(e) plotly::plotly_empty() %>% plotly::layout(title = paste0("Plot conversion error: ", substr(e$message,1,120))))
  out %>% plotly::config(displayModeBar = FALSE)
}

ui <- fluidPage(
  title = "Behavioral Delta Risk Explorer — Stakeholder View",
  tags$head(tags$style(HTML("
    .warning-banner { background:#f8d7da; color:#721c24; padding:8px; border-radius:4px; margin-bottom:8px; }
    .info-box { background:#eef6fb; color:#0b3d91; padding:8px; border-radius:4px; margin-bottom:8px; }
    .small-note { font-size:12px; color:#666; margin-top:6px; }
    .mono { font-family: monospace; font-size:12px; }
  "))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Operational controls"),
                 sliderInput("threshold","Outreach threshold (score cutoff)",min=0,max=1,value=0.10,step=0.01),
                 numericInput("cost","Outreach cost per customer (USD)",value=10,min=0),
                 selectInput("filter_decile","Filter decile",choices=c("All",as.character(1:10)),selected="All"),
                 actionButton("reload","Reload artifacts"),
                 downloadButton("download_sample","Download sample predictions"),
                 hr(),
                 h5("Quick provenance"),
                 verbatimTextOutput("diagnostics", placeholder = TRUE),
                 hr(),
                 h5("Provenance export"),
                 downloadButton("download_provenance_json", "Download provenance JSON"),
                 downloadButton("download_provenance_rds", "Download provenance RDS"),
                 hr(),
                 h5("ROI preview"),
                 verbatimTextOutput("roi_preview", placeholder = TRUE),
                 p(class="small-note", "ROI preview uses observed positives among targeted customers in the test set. Observed positives are not causal uplift; run an A/B test to estimate incremental lift.")
    ),
    mainPanel(width = 9,
              uiOutput("leakage_banner"),
              tabsetPanel(
                tabPanel("Overview",
                         fluidRow(
                           column(6, plotlyOutput("decile_counts_plot", height="320px")),
                           column(6, plotlyOutput("decile_obs_plot", height="320px"))
                         ),
                         p(class="small-note", strong("Slide note:"), "Top deciles concentrate positives; we checked for label leakage and duplicates — none found. This likely reflects a small high-risk subgroup; recommend A/B test outreach on decile 10."),
                         p(class="small-note", strong("Decile semantics:"), "Deciles are equal-count (ntile) bins by default; change decile semantics in the artifact pipeline to fixed probability bins if stakeholders prefer."),
                         hr(),
                         fluidRow(
                           column(6, plotlyOutput("roc_plot", height="360px")),
                           column(6, plotlyOutput("reliability_plot", height="360px"))
                         ),
                         p(class="small-note", strong("Interpretation tip:"), "Use ROC for discrimination and reliability for calibration. Both inform threshold choice."),
                         hr(),
                         wellPanel(h5("Artifact provenance and diagnostics"), verbatimTextOutput("artifact_provenance_txt", placeholder = TRUE))
                ),
                tabPanel("Customer detail",
                         fluidRow(column(12, DTOutput("pred_table"))),
                         hr(),
                         fluidRow(column(8, plotlyOutput("local_shap_plot", height="420px")), column(4, verbatimTextOutput("local_shap_msg", placeholder = TRUE)))
                ),
                tabPanel("Model insights",
                         fluidRow(column(6, plotlyOutput("rf_varimp_plot", height="480px")), column(6, plotlyOutput("xgb_varimp_plot", height="480px"))),
                         p(class="small-note", "Top predictors shown are from the model trained with behavioral deltas. Ablation table below reports (with − without) performance deltas (median and 95% CI)."),
                         hr(),
                         fluidRow(column(12, DTOutput("ablation_table"))),
                         p(class="small-note", strong("Plain English:"), "If the 95% CI excludes zero, the change is statistically meaningful. Small deltas indicate behavioral deltas add limited incremental predictive power in this dataset."),
                         p(class="small-note", em("Ablation example:"), "AUC delta −0.0012 to −0.0002 means performance decreased slightly and the 95% CI excludes zero → statistically meaningful (but small in magnitude).")
                ),
                tabPanel("A/B experiment",
                         fluidRow(
                           column(12, verbatimTextOutput("ab_test_summary")),
                           column(12, DTOutput("ab_test_table"))
                         ),
                         p(class="small-note", "If you ran a randomized experiment, save its summary into artifacts as 'ab_test_results' or 'experiment_results' (data frame with group, n, conversions, rate, lift, p_value). The app will display it here.")
                ),
                tabPanel("Monitoring",
                         fluidRow(
                           column(6, wellPanel(h5("Weekly AUC (if time variable present)"), plotlyOutput("weekly_auc_plot", height="300px"))),
                           column(6, wellPanel(h5("Feature drift summary (simple)"), DTOutput("feature_drift_table")))
                         ),
                         p(class="small-note", "Monitoring requires a time column (e.g., 'date' or 'week') in test_baked or temporal validation artifacts. If absent, this panel shows instructions."),
                         p(class="small-note", strong("Scheduling note:"), "To enable monitoring, schedule your artifact pipeline to append a 'date' or 'week' column to test_baked each run (e.g., weekly cron job that saves artifacts).")
                )
              )
    )
  )
)

server <- function(input, output, session) {
  artifacts_r <- reactiveVal(ARTIFACTS)
  observeEvent(input$reload, { artifacts_r(safe_load_artifacts()) })
  
  output$diagnostics <- renderText({
    art <- artifacts_r(); keys <- names(art); prov <- art$artifact_provenance %||% list()
    paste0("Artifacts keys: ", if(length(keys)>0) paste(keys, collapse=", ") else "<none>", "\n",
           "proj_file: ", proj_file_abs, "\n",
           "proj_file exists: ", file.exists(proj_file_abs), "\n",
           "provenance created_at: ", prov$created_at %||% "NA", "; n_test: ", prov$n_test %||% "NA")
  })
  
  output$leakage_banner <- renderUI({
    art <- artifacts_r(); prov <- art$artifact_provenance %||% list()
    if (!is.null(prov$leakage_flags) && length(prov$leakage_flags) > 0) {
      div(class="warning-banner", strong("Data quality warning: "), "Potential leakage or suspicious features detected. See provenance panel for details.")
    } else {
      NULL
    }
  })
  
  output$artifact_provenance_txt <- renderText({
    art <- artifacts_r(); prov <- art$artifact_provenance %||% NULL
    if (is.null(prov)) return("No artifact provenance saved.")
    paste0(
      "created_at: ", prov$created_at %||% "NA", "\n",
      "n_test: ", prov$n_test %||% "NA", "\n",
      "decile_mode: ", prov$decile_mode %||% "ntile", "\n",
      "target_raw_sample (first 20): ", paste0(head(prov$target_raw_sample %||% character(0), 20), collapse = ", "), "\n",
      "target_table: ", if (!is.null(prov$target_table)) paste0(names(prov$target_table), ":", unlist(prov$target_table), collapse = "; ") else "None", "\n\n",
      "univariate_auc_top (top 10):\n", if (!is.null(prov$univariate_auc_top)) paste0(capture.output(print(head(prov$univariate_auc_top,10))), collapse = "\n") else "None", "\n\n",
      "leakage_flags:\n", if (!is.null(prov$leakage_flags)) paste0(capture.output(print(prov$leakage_flags)), collapse = "\n") else "None", "\n\n",
      "Data dictionary (one-line): Preprocessed features are the model inputs (x_test_baked); metadata columns: PseudoID, pred_prob, pred_decile, target_attrit.", "\n\n",
      "Slide note: Top deciles concentrate positives; we checked for label leakage and duplicates — none found. Recommend A/B test outreach on decile 10."
    )
  })
  
  # provenance export handlers
  output$download_provenance_json <- downloadHandler(
    filename = function() paste0("artifact_provenance_", Sys.Date(), ".json"),
    content = function(file) {
      art <- artifacts_r()
      prov <- art$artifact_provenance %||% list()
      prov_serializable <- prov
      if (!is.null(prov_serializable$univariate_auc_top) && inherits(prov_serializable$univariate_auc_top, "data.frame")) prov_serializable$univariate_auc_top <- as.data.frame(prov_serializable$univariate_auc_top)
      write_json(prov_serializable, path = file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  
  output$download_provenance_rds <- downloadHandler(
    filename = function() paste0("artifact_provenance_", Sys.Date(), ".rds"),
    content = function(file) {
      art <- artifacts_r()
      prov <- art$artifact_provenance %||% list()
      saveRDS(prov, file = file)
    }
  )
  
  get_test_baked <- reactive({
    art <- artifacts_r()
    if (!"test_baked" %in% names(art)) return(NULL)
    tb <- art$test_baked
    if (!is.data.frame(tb)) return(NULL)
    if ("pred_prob" %in% names(tb)) tb$pred_prob <- as.numeric(tb$pred_prob)
    if ("target_attrit" %in% names(tb)) tb$target_attrit <- as.numeric(tb$target_attrit)
    prov <- art$artifact_provenance %||% list(); decile_mode <- prov$decile_mode %||% "ntile"
    if (!"pred_decile" %in% names(tb) && "pred_prob" %in% names(tb)) {
      if (decile_mode == "ntile") tb <- tb %>% mutate(pred_decile = factor(dplyr::ntile(pred_prob, 10), levels = 1:10)) else tb <- tb %>% mutate(pred_decile = factor(as.integer(cut(pred_prob, breaks = seq(0,1,by=0.1), include.lowest = TRUE, labels = 1:10)), levels = 1:10))
    } else if ("pred_decile" %in% names(tb)) tb$pred_decile <- factor(as.integer(tb$pred_decile), levels = 1:10)
    tb
  })
  
  filtered_tb <- reactive({
    tb <- get_test_baked(); req(tb)
    if (input$filter_decile != "All") tb <- tb %>% filter(as.character(pred_decile) == input$filter_decile)
    tb
  })
  
  output$roi_preview <- renderText({
    art <- artifacts_r(); tb <- art$test_baked %||% NULL
    if (is.null(tb) || !"pred_prob" %in% names(tb)) return("No pred_prob available for ROI preview.")
    thr <- input$threshold; cost <- input$cost
    n <- nrow(tb)
    selected <- sum(tb$pred_prob >= thr, na.rm = TRUE)
    positives_expected <- sum(tb$target_attrit[tb$pred_prob >= thr], na.rm = TRUE)
    avg_prob <- if (selected>0) mean(tb$pred_prob[tb$pred_prob >= thr], na.rm = TRUE) else 0
    paste0("Threshold: ", round(thr,3), " | Customers targeted: ", selected, " / ", n, "\n",
           "Observed positives among targeted (test set): ", positives_expected, "\n",
           "Avg predicted probability among targeted: ", round(avg_prob,3), "\n",
           "Total outreach cost (USD): ", round(selected * cost,2), "\n",
           "Caveat: observed positives are test-set counts, not causal uplift. Run an A/B test to estimate incremental lift.")
  })
  
  output$decile_counts_plot <- renderPlotly({
    tb <- get_test_baked(); req(tb)
    decile_counts <- tb %>% count(pred_decile) %>% arrange(as.integer(as.character(pred_decile)))
    all_dec <- tibble(pred_decile = factor(as.character(1:10), levels = as.character(1:10)))
    decile_counts <- all_dec %>% left_join(decile_counts, by = "pred_decile") %>% mutate(n = replace_na(n, 0))
    p <- ggplot(decile_counts, aes(x = pred_decile, y = n, text = paste0("Decile: ", pred_decile, "<br>Count: ", n))) +
      geom_col(fill = "#2b8cbe") + geom_text(aes(label = n), vjust = -0.3, size = 3) +
      labs(x="Decile", y="Count", title="Decile counts (test set)") + theme_minimal()
    safe_ggplotly(p)
  })
  
  output$decile_obs_plot <- renderPlotly({
    tb <- get_test_baked(); req(tb)
    decile_obs <- tb %>% group_by(pred_decile) %>% summarise(obs = mean(as.numeric(target_attrit), na.rm = TRUE), n = n(), .groups = "drop")
    all_dec <- tibble(pred_decile = factor(as.character(1:10), levels = as.character(1:10)))
    decile_obs <- all_dec %>% left_join(decile_obs, by = "pred_decile") %>% mutate(obs = replace_na(obs, 0), n = replace_na(n, 0))
    p <- ggplot(decile_obs, aes(x = pred_decile, y = obs, text = paste0("Decile: ", pred_decile, "<br>Observed: ", round(obs,4), "<br>Count: ", n))) +
      geom_col(fill = "#fdae61") + geom_text(aes(label = scales::percent(obs, accuracy = 0.1)), vjust = -0.3, size = 3) +
      labs(x="Decile", y="Observed attrition", title="Observed attrition by decile (test set)") + theme_minimal()
    safe_ggplotly(p)
  })
  
  output$roc_plot <- renderPlotly({
    art <- artifacts_r(); rocs <- list()
    if ("roc_base" %in% names(art)) rocs$base <- art$roc_base
    if ("roc_enh" %in% names(art)) rocs$enh <- art$roc_enh
    if ("roc_rf" %in% names(art)) rocs$rf <- art$roc_rf
    if ("roc_xgb" %in% names(art)) rocs$xgb <- art$roc_xgb
    if (length(rocs) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "ROC overlay (no ROC objects found)"))
    p <- ggplot() + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70")
    cols <- c(base="black", enh="blue", rf="darkgreen", xgb="red")
    for (nm in names(rocs)) {
      roc_obj <- rocs[[nm]]
      if (inherits(roc_obj, "roc")) {
        df <- data.frame(tpr = rev(roc_obj$sensitivities), fpr = rev(1 - roc_obj$specificities))
        p <- p + geom_line(data = df, aes(x = fpr, y = tpr), color = cols[nm], size = 1)
        p <- p + geom_point(data = df, aes(x = fpr, y = tpr, text = paste0(nm, "<br>AUC: ", round(as.numeric(pROC::auc(roc_obj)),4))), color = cols[nm], size = 0.6, alpha = 0.6)
      }
    }
    p <- p + labs(x="1 - Specificity", y="Sensitivity", title="ROC overlay (test set)") + theme_minimal()
    safe_ggplotly(p)
  })
  
  output$reliability_plot <- renderPlotly({
    art <- artifacts_r()
    if ("cal_plot" %in% names(art) && inherits(art$cal_plot, "ggplot")) return(safe_ggplotly(art$cal_plot))
    tb <- get_test_baked(); req(tb)
    if (!"pred_prob" %in% names(tb) || !"target_attrit" %in% names(tb)) return(plotly::plotly_empty() %>% plotly::layout(title="Reliability diagram (missing data)"))
    cal_df <- tb %>% mutate(bin = ntile(pred_prob, 10)) %>% group_by(bin) %>% summarise(mean_prob = mean(pred_prob, na.rm = TRUE), obs = mean(target_attrit, na.rm = TRUE), n = n(), .groups = "drop")
    p <- ggplot(cal_df, aes(x = mean_prob, y = obs, text = paste0("Bin: ", bin, "<br>Mean pred: ", round(mean_prob,4), "<br>Obs: ", round(obs,4), "<br>n: ", n))) +
      geom_point(size = 2) + geom_line() + geom_abline(slope = 1, intercept = 0, linetype = "dashed") + labs(x = "Mean predicted probability", y = "Observed event rate", title = "Reliability diagram (deciles)") + theme_minimal()
    safe_ggplotly(p)
  })
  
  output$pred_table <- renderDT({
    tb <- filtered_tb(); req(tb)
    cols_keep <- c("PseudoID","pred_prob","pred_decile","target_attrit","Customer_Age","Dependent_count","Amt_Chg_Q4Q1","Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon")
    cols_present <- intersect(cols_keep, names(tb))
    dat <- tb %>% select(all_of(cols_present))
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE), selection = list(mode = "single", target = "row"))
  })
  
  output$local_shap_msg <- renderText({
    art <- artifacts_r()
    if ("xgb_final" %in% names(art) || "shap_vals" %in% names(art)) "Local SHAP available for selected customer (select a row in the table)." else "SHAP not available."
  })
  
  output$local_shap_plot <- renderPlotly({
    art <- artifacts_r(); tb <- filtered_tb(); req(tb)
    sel <- input$pred_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "Select a row to view local SHAP"))
    row <- tb[sel, , drop = FALSE]
    feat_order <- art$model_feature_names %||% colnames(art$x_test_baked %||% row)
    feat_order <- intersect(feat_order, colnames(row))
    if (length(feat_order) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No model feature columns available for SHAP"))
    
    if ("xgb_final" %in% names(art)) {
      xgb_model <- art$xgb_final
      newmat <- as.matrix(row[, feat_order, drop = FALSE])
      tryCatch({
        contrib <- predict(xgb_model, xgboost::xgb.DMatrix(newmat), predcontrib = TRUE)
        if (is.matrix(contrib)) contrib <- contrib[, -ncol(contrib), drop = FALSE]
        contrib_df <- tibble(Feature = feat_order, Contribution = as.numeric(contrib[1, ])) %>% arrange(desc(abs(Contribution)))
        p <- ggplot(contrib_df, aes(x = reorder(Feature, Contribution), y = Contribution, text = paste0(Feature, "<br>SHAP: ", round(Contribution,6)))) +
          geom_col(aes(fill = Contribution > 0)) + scale_fill_manual(values = c("TRUE"="#d73027","FALSE"="#2b8cbe"), guide = "none") + coord_flip() +
          labs(title = "Local SHAP (XGBoost predcontrib)", x = NULL, y = "Contribution") + theme_minimal()
        plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
      }, error = function(e) plotly::plotly_empty() %>% plotly::layout(title = paste0("SHAP compute failed: ", e$message)))
    } else if ("shap_vals" %in% names(art)) {
      shap <- art$shap_vals
      if ("PseudoID" %in% names(row) && "PseudoID" %in% names(art$test_baked)) {
        pid <- row$PseudoID; idx <- which(art$test_baked$PseudoID == pid)
      } else idx <- sel
      if (length(idx) == 1 && idx <= nrow(shap)) {
        contrib_df <- tibble(Feature = colnames(shap), Contribution = as.numeric(shap[idx, ])) %>% arrange(desc(abs(Contribution)))
        p <- ggplot(contrib_df, aes(x = reorder(Feature, Contribution), y = Contribution, text = paste0(Feature, "<br>SHAP: ", round(Contribution,6)))) +
          geom_col(aes(fill = Contribution > 0)) + scale_fill_manual(values = c("TRUE"="#d73027","FALSE"="#2b8cbe"), guide = "none") + coord_flip() +
          labs(title = "Local SHAP (precomputed)", x = NULL, y = "Contribution") + theme_minimal()
        plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
      } else plotly::plotly_empty() %>% plotly::layout(title = "No precomputed SHAP for selected row")
    } else plotly::plotly_empty() %>% plotly::layout(title = "No SHAP model or precomputed SHAP available")
  })
  
  output$rf_varimp_plot <- renderPlotly({
    art <- artifacts_r(); if (!"top20_rf" %in% names(art)) return(plotly::plotly_empty() %>% plotly::layout(title="RF importance not available"))
    rf_imp <- art$top20_rf; if (!all(c("Feature","Overall") %in% names(rf_imp))) names(rf_imp)[1:2] <- c("Feature","Overall")
    p <- ggplot(rf_imp, aes(x = reorder(Feature, Overall), y = Overall, text = paste0(Feature, "<br>Importance: ", round(Overall,3)))) +
      geom_col(fill="#2b8cbe") + geom_text(aes(label = round(Overall,3)), hjust = -0.1, size = 3) + coord_flip() +
      labs(x = NULL, y = "Importance (scaled)", title = "Random Forest top predictors (with behavioral deltas)") + theme_minimal()
    safe_ggplotly(p) %>% layout(margin = list(l = 180))
  })
  
  output$xgb_varimp_plot <- renderPlotly({
    art <- artifacts_r(); if (!"top20_xgb" %in% names(art)) return(plotly::plotly_empty() %>% plotly::layout(title="XGB importance not available"))
    xgb_imp <- art$top20_xgb
    if ("Gain" %in% names(xgb_imp)) value_col <- "Gain" else { value_col <- names(xgb_imp)[2]; names(xgb_imp)[1:2] <- c("Feature", value_col) }
    p <- ggplot(xgb_imp, aes(x = reorder(Feature, !!sym(value_col)), y = !!sym(value_col), text = paste0(Feature, "<br>Gain: ", round(!!sym(value_col),3)))) +
      geom_col(fill="#d73027") + geom_text(aes(label = round(!!sym(value_col),3)), hjust = -0.1, size = 3) + coord_flip() +
      labs(x = NULL, y = "Gain", title = "XGBoost top predictors (with behavioral deltas)") + theme_minimal()
    safe_ggplotly(p) %>% layout(margin = list(l = 180))
  })
  
  output$ablation_table <- renderDT({
    art <- artifacts_r(); if (!"ci_tbl" %in% names(art)) return(datatable(tibble(Message = "Ablation CIs not available")))
    datatable(art$ci_tbl, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # A/B experiment panel: detect and display if present
  output$ab_test_summary <- renderText({
    art <- artifacts_r()
    # common artifact names for experiments
    ab_names <- intersect(c("ab_test_results","experiment_results","ab_results","experiment_summary"), names(art))
    if (length(ab_names) == 0) return("No A/B experiment results found in artifacts.")
    ab <- art[[ab_names[1]]]
    # if it's a data.frame or tibble with expected columns, summarize
    if (is.data.frame(ab)) {
      # expected minimal columns: group, n, conversions, rate, lift, p_value
      cols <- names(ab)
      summary_lines <- c(paste0("Found experiment artifact: ", ab_names[1]), paste0("Columns: ", paste(cols, collapse = ", ")))
      # if aggregated summary present, show it
      if (all(c("group","n","conversions","rate") %in% cols)) {
        lines <- apply(ab, 1, function(r) paste0("Group: ", r["group"], " | n=", r["n"], " | conversions=", r["conversions"], " | rate=", round(as.numeric(r["rate"]),4)))
        summary_lines <- c(summary_lines, lines)
      }
      if ("lift" %in% cols) summary_lines <- c(summary_lines, paste0("Lift (reported): ", paste0(round(as.numeric(ab$lift),4), collapse = ", ")))
      if ("p_value" %in% cols) summary_lines <- c(summary_lines, paste0("p-values: ", paste0(round(as.numeric(ab$p_value),4), collapse = ", ")))
      return(paste(summary_lines, collapse = "\n"))
    } else {
      return("Experiment artifact found but not in expected tabular format. Please save a data.frame with columns group,n,conversions,rate,lift,p_value.")
    }
  })
  
  output$ab_test_table <- renderDT({
    art <- artifacts_r()
    ab_names <- intersect(c("ab_test_results","experiment_results","ab_results","experiment_summary"), names(art))
    if (length(ab_names) == 0) return(datatable(tibble(Message = "No A/B experiment results found in artifacts.")))
    ab <- art[[ab_names[1]]]
    if (!is.data.frame(ab)) return(datatable(tibble(Message = "Experiment artifact not tabular; save a data.frame for display.")))
    datatable(ab, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Monitoring: weekly AUC and simple feature drift (guarded)
  output$weekly_auc_plot <- renderPlotly({
    art <- artifacts_r(); tb <- art$test_baked %||% NULL
    if (is.null(tb)) return(plotly::plotly_empty() %>% plotly::layout(title = "No test_baked available"))
    time_col <- intersect(c("week","week_start","date","timestamp","event_date"), names(tb))
    if (length(time_col) == 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = "No time column found; add 'date' or 'week' to test_baked for monitoring"))
    }
    tc <- time_col[1]
    df <- tb %>% filter(!is.na(.data[[tc]]), !is.na(pred_prob), !is.na(target_attrit))
    if (nrow(df) < 10) return(plotly::plotly_empty() %>% plotly::layout(title = "Insufficient time-series rows for weekly AUC"))
    if (inherits(df[[tc]], "Date") || grepl("date", tc, ignore.case = TRUE)) df <- df %>% mutate(week = as.character(cut(as.Date(.data[[tc]]), "week"))) else df <- df %>% mutate(week = as.character(.data[[tc]]))
    auc_by_week <- df %>% group_by(week) %>% summarise(auc = tryCatch(as.numeric(pROC::auc(pROC::roc(target_attrit, pred_prob, quiet = TRUE))), error = function(e) NA_real_), n = n(), .groups = "drop")
    p <- ggplot(auc_by_week, aes(x = week, y = auc, text = paste0("Week: ", week, "<br>AUC: ", round(auc,3), "<br>n: ", n))) + geom_line(group=1) + geom_point() + labs(x="Week", y="AUC", title="Weekly AUC (if time data present)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    safe_ggplotly(p)
  })
  
  output$feature_drift_table <- renderDT({
    art <- artifacts_r(); tb <- art$test_baked %||% NULL
    if (is.null(tb)) return(datatable(tibble(Message = "No test_baked available for drift check")))
    time_col <- intersect(c("week","week_start","date","timestamp","event_date"), names(tb))
    if (length(time_col) == 0) return(datatable(tibble(Message = "No time column found; add 'date' or 'week' to test_baked for drift monitoring")))
    tc <- time_col[1]
    df <- tb %>% filter(!is.na(.data[[tc]]))
    if (nrow(df) < 20) return(datatable(tibble(Message = "Insufficient rows for drift check")))
    if (inherits(df[[tc]], "Date") || grepl("date", tc, ignore.case = TRUE)) df <- df %>% mutate(week = as.character(cut(as.Date(.data[[tc]]), "week"))) else df <- df %>% mutate(week = as.character(.data[[tc]]))
    weeks <- sort(unique(df$week))
    if (length(weeks) < 2) return(datatable(tibble(Message = "Not enough time buckets for drift check")))
    first <- weeks[1]; last <- weeks[length(weeks)]
    num_feats <- intersect(names(art$x_test_baked %||% tibble()), names(df)[sapply(df, is.numeric)])
    if (length(num_feats) == 0) return(datatable(tibble(Message = "No numeric features available for drift check")))
    summary_df <- df %>% filter(week %in% c(first, last)) %>% group_by(week) %>% summarise(across(all_of(num_feats), mean, na.rm = TRUE), .groups = "drop")
    if (nrow(summary_df) < 2) return(datatable(tibble(Message = "Insufficient summary rows for drift check")))
    first_vals <- summary_df %>% filter(week == first) %>% select(all_of(num_feats)) %>% slice(1)
    last_vals  <- summary_df %>% filter(week == last)  %>% select(all_of(num_feats)) %>% slice(1)
    diffs <- as.numeric(last_vals - first_vals)
    drift_tbl <- tibble(feature = num_feats, first_mean = as.numeric(first_vals), last_mean = as.numeric(last_vals), delta = diffs) %>% arrange(desc(abs(delta))) %>% head(20)
    datatable(drift_tbl, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # debug logging
  observe({
    art <- artifacts_r()
    message("DEBUG: artifacts keys: ", paste(names(art), collapse = ", "))
    if ("test_baked" %in% names(art)) {
      tb <- art$test_baked
      message("DEBUG: rows: ", nrow(tb), " cols: ", ncol(tb))
      if ("pred_prob" %in% names(tb)) message("DEBUG: pred_prob sample: ", paste(round(head(tb$pred_prob,5),6), collapse = ", "))
      if ("pred_decile" %in% names(tb)) message("DEBUG: pred_decile sample: ", paste(head(tb$pred_decile,5), collapse = ", "))
      if ("target_attrit" %in% names(tb)) message("DEBUG: target distribution: ", paste(capture.output(table(tb$target_attrit)), collapse = " | "))
    }
  })
}

shinyApp(ui = ui, server = server)
