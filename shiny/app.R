# shiny/app.R
# Behavioral Delta Risk Explorer - updated full app.R
# Save this file as shiny/app.R and run with shiny::runApp("shiny") or open in RStudio and click Run App.

library(shiny)
library(tidyverse)
library(plotly)
library(pROC)
library(knitr)
library(DT)

# Helper: null coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Helper: safe loader for artifacts
safe_load_artifacts <- function(path = "artifacts/project_artifacts.RData") {
  env <- new.env()
  if (file.exists(path)) {
    load(path, envir = env)
    as.list(env)
  } else {
    list()
  }
}

# Load artifacts at startup (can be reloaded via UI)
artifacts <- safe_load_artifacts("artifacts/project_artifacts.RData")

# Extract objects with fallbacks
test_baked <- artifacts$test_baked %||% NULL
platt_pred  <- artifacts$platt_pred %||% NULL
raw_probs   <- artifacts$raw_probs %||% artifacts$pred_prob %||% artifacts$xgb_pred_prob %||% NULL
pred_prob   <- if (!is.null(platt_pred)) platt_pred else raw_probs
cal_plot    <- artifacts$cal_plot %||% (if (file.exists("artifacts/cal_plot.rds")) readRDS("artifacts/cal_plot.rds") else NULL)
ci_tbl      <- artifacts$ci_tbl %||% (if (file.exists("artifacts/ci_tbl.rds")) readRDS("artifacts/ci_tbl.rds") else NULL)
xgb_final   <- artifacts$xgb_final %||% (if (file.exists("artifacts/xgb_final.rds")) readRDS("artifacts/xgb_final.rds") else NULL)
top20_rf    <- artifacts$top20_rf %||% NULL
top20_xgb   <- artifacts$top20_xgb %||% NULL
roc_base    <- artifacts$roc_base %||% NULL
roc_enh     <- artifacts$roc_enh %||% NULL
roc_rf      <- artifacts$roc_rf %||% NULL
roc_xgb     <- artifacts$roc_xgb %||% NULL
brier_xgb   <- artifacts$brier_xgb %||% NULL
pred_with_behav  <- artifacts$pred_with_behav %||% NULL
pred_without_behav <- artifacts$pred_without_behav %||% NULL
top20_rf <- top20_rf
top20_xgb <- top20_xgb

# Prepare baked df if possible
if (!is.null(test_baked) && is.null(pred_prob) && "Predicted_Risk" %in% names(test_baked)) {
  pred_prob <- test_baked$Predicted_Risk
}
if (!is.null(test_baked) && !is.null(pred_prob)) {
  if (!"pred_prob" %in% names(test_baked)) {
    test_baked <- test_baked %>% mutate(pred_prob = pred_prob)
  } else {
    test_baked <- test_baked %>% mutate(pred_prob = ifelse(is.na(pred_prob), pred_prob, pred_prob))
  }
  if (!"pred_decile" %in% names(test_baked)) {
    test_baked <- test_baked %>% mutate(pred_decile = ntile(pred_prob, 10))
  }
}

ui <- fluidPage(
  titlePanel("Behavioral Delta Risk Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Interactive explorer for calibrated attrition scores."),
      sliderInput("threshold", "Outreach threshold", min = 0, max = 1, value = 0.10, step = 0.01),
      numericInput("outreach_cost", "Outreach cost per customer", value = 10, min = 0),
      selectInput("decile_select", "Filter decile", choices = c("All", as.character(1:10)), selected = "All"),
      actionButton("refresh", "Reload artifacts"),
      hr(),
      downloadButton("download_sample", "Download sample predictions")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(column(6, plotlyOutput("decile_plot")), column(6, plotlyOutput("obs_rate_plot"))),
                 fluidRow(column(6, plotlyOutput("roc_plot")), column(6, plotlyOutput("cal_plot"))),
                 br(),
                 DTOutput("threshold_table")
        ),
        tabPanel("Customer detail",
                 fluidRow(column(6, DTOutput("customer_table")), column(6, uiOutput("customer_info"))),
                 br(),
                 conditionalPanel(condition = "output.shapAvailable == true",
                                  h4("Local SHAP for selected customer"),
                                  plotlyOutput("shap_plot"))
        ),
        tabPanel("Model insights",
                 fluidRow(column(6, plotOutput("rf_imp_plot")), column(6, plotOutput("xgb_imp_plot"))),
                 br(),
                 conditionalPanel(condition = "output.ciAvailable == true",
                                  h4("Ablation bootstrap CIs"),
                                  DTOutput("ci_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive storage for artifacts
  artifacts_reactive <- reactiveVal(artifacts)
  
  observeEvent(input$refresh, {
    new_art <- safe_load_artifacts("artifacts/project_artifacts.RData")
    artifacts_reactive(new_art)
    # update local copies used by server
    test_baked <<- new_art$test_baked %||% test_baked
    platt_pred  <<- new_art$platt_pred %||% platt_pred
    raw_probs   <<- new_art$raw_probs %||% new_art$pred_prob %||% new_art$xgb_pred_prob %||% raw_probs
    pred_prob   <<- if (!is.null(platt_pred)) platt_pred else raw_probs
    cal_plot    <<- new_art$cal_plot %||% cal_plot
    ci_tbl      <<- new_art$ci_tbl %||% ci_tbl
    xgb_final   <<- new_art$xgb_final %||% xgb_final
    top20_rf    <<- new_art$top20_rf %||% top20_rf
    top20_xgb   <<- new_art$top20_xgb %||% top20_xgb
    roc_base    <<- new_art$roc_base %||% roc_base
    roc_enh     <<- new_art$roc_enh %||% roc_enh
    roc_rf      <<- new_art$roc_rf %||% roc_rf
    roc_xgb     <<- new_art$roc_xgb %||% roc_xgb
    showNotification("Artifacts reloaded", type = "message")
  })
  
  df <- reactive({
    art <- artifacts_reactive()
    tb <- art$test_baked %||% test_baked
    probs <- art$platt_pred %||% art$raw_probs %||% art$pred_prob %||% pred_prob
    if (!is.null(tb) && !is.null(probs)) {
      tb <- tb %>% mutate(pred_prob = probs)
      if (!"pred_decile" %in% names(tb)) tb <- tb %>% mutate(pred_decile = ntile(pred_prob, 10))
    }
    tb
  })
  
  # Decile distribution
  output$decile_plot <- renderPlotly({
    d <- df()
    req(d)
    dec <- d %>% group_by(pred_decile) %>% summarise(n = n(), .groups = "drop")
    p <- ggplot(dec, aes(x = factor(pred_decile), y = n)) +
      geom_col(fill = "#2b8cbe") +
      labs(x = "Decile", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Observed rate by decile
  output$obs_rate_plot <- renderPlotly({
    d <- df()
    req(d)
    dec <- d %>% group_by(pred_decile) %>% summarise(obs_rate = mean(target_attrit, na.rm = TRUE), .groups = "drop")
    p <- ggplot(dec, aes(x = factor(pred_decile), y = obs_rate)) +
      geom_col(fill = "#f03b20") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Decile", y = "Observed attrition") +
      theme_minimal()
    ggplotly(p)
  })
  
  # ROC overlay (if ROC objects exist)
  output$roc_plot <- renderPlotly({
    art <- artifacts_reactive()
    rocs <- list(art$roc_base %||% roc_base, art$roc_enh %||% roc_enh, art$roc_rf %||% roc_rf, art$roc_xgb %||% roc_xgb)
    names(rocs) <- c("base","enh","rf","xgb")
    rocs <- rocs[!sapply(rocs, is.null)]
    if (length(rocs) == 0) {
      return(plotly_empty() %>% layout(title = "ROC not available"))
    }
    plt <- ggplot() + labs(title = "ROC overlay (test set)")
    colors <- c("black","blue","darkgreen","red")
    i <- 1
    for (nm in names(rocs)) {
      roc_obj <- rocs[[nm]]
      if (is.null(roc_obj)) next
      df_roc <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
      plt <- plt + geom_line(data = df_roc, aes(x = fpr, y = tpr), color = colors[i], size = 1, alpha = 0.8)
      i <- i + 1
    }
    plt <- plt + geom_abline(linetype = "dashed") + xlab("1 - Specificity") + ylab("Sensitivity") + theme_minimal()
    ggplotly(plt)
  })
  
  # Calibration plot
  output$cal_plot <- renderPlotly({
    d <- df()
    if (!is.null(cal_plot)) {
      tryCatch({
        ggplotly(cal_plot)
      }, error = function(e) plotly_empty() %>% layout(title = "Calibration plot error"))
    } else if (!is.null(d)) {
      cal_df <- d %>% mutate(bin = ntile(pred_prob, 10)) %>% group_by(bin) %>% summarise(mean_prob = mean(pred_prob, na.rm = TRUE), obs = mean(target_attrit, na.rm = TRUE), .groups = "drop")
      p <- ggplot(cal_df, aes(x = mean_prob, y = obs)) +
        geom_point(size = 2) + geom_line() + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        labs(x = "Mean predicted probability", y = "Observed event rate", title = "Reliability diagram") +
        theme_minimal()
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Calibration not available")
    }
  })
  
  # Decision threshold table
  output$threshold_table <- renderDT({
    d <- df()
    req(d)
    use_probs <- d$pred_prob
    truth <- d$target_attrit
    thresholds <- c(0.01, 0.02, 0.05, 0.10, 0.20)
    threshold_table <- map_dfr(thresholds, function(t){
      preds_bin <- as.integer(use_probs >= t)
      tp <- sum(preds_bin == 1 & truth == 1, na.rm = TRUE)
      fp <- sum(preds_bin == 1 & truth == 0, na.rm = TRUE)
      tn <- sum(preds_bin == 0 & truth == 0, na.rm = TRUE)
      fn <- sum(preds_bin == 0 & truth == 1, na.rm = TRUE)
      precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
      recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
      tibble(threshold = t, TP = tp, FP = fp, TN = tn, FN = fn, Precision = precision, Recall = recall)
    })
    datatable(threshold_table, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # Customer table and selection
  output$customer_table <- renderDT({
    d <- df()
    req(d)
    sel_dec <- input$decile_select
    if (sel_dec != "All") d <- d %>% filter(pred_decile == as.integer(sel_dec))
    d_small <- d %>% select(PseudoID, pred_prob, pred_decile, target_attrit, everything()) %>% arrange(desc(pred_prob))
    datatable(d_small, selection = "single", options = list(pageLength = 10))
  })
  
  # Customer detail UI
  output$customer_info <- renderUI({
    d <- df()
    req(d)
    sel <- input$customer_table_rows_selected
    if (length(sel) == 0) {
      top <- d %>% arrange(desc(pred_prob)) %>% slice(1)
    } else {
      top <- d %>% arrange(desc(pred_prob)) %>% slice(sel[1])
    }
    if (nrow(top) == 0) return(HTML("<p>No customer selected</p>"))
    tagList(
      h4("Customer detail"),
      p(strong("PseudoID:"), top$PseudoID),
      p(strong("Calibrated probability:"), round(top$pred_prob, 3)),
      p(strong("Amt delta:"), if ("Amt_Chg_Q4Q1" %in% names(top)) round(top$Amt_Chg_Q4Q1, 3) else "NA"),
      p(strong("Ct delta:"), if ("Ct_Chg_Q4Q1" %in% names(top)) round(top$Ct_Chg_Q4Q1, 3) else "NA"),
      p(strong("Contacts last 12m:"), if ("Contacts_Count_12_mon" %in% names(top)) top$Contacts_Count_12_mon else "NA")
    )
  })
  
  # SHAP explainability for selected customer (approximate)
  output$shap_plot <- renderPlotly({
    # require xgb_final and fastshap
    req(xgb_final)
    if (!requireNamespace("fastshap", quietly = TRUE)) {
      return(plotly_empty() %>% layout(title = "fastshap not installed"))
    }
    d <- df()
    req(d)
    sel <- input$customer_table_rows_selected
    idx <- if (length(sel) == 0) 1 else sel[1]
    # prepare X sample (exclude id/target columns)
    X <- as.data.frame(d %>% select(-PseudoID, -target_attrit, -pred_prob, -pred_decile))
    # ensure numeric matrix for fastshap
    Xnum <- X %>% mutate(across(everything(), ~ as.numeric(as.character(.))))
    pred_fun <- function(object, newdata) {
      as.numeric(predict(object, newdata = xgboost::xgb.DMatrix(as.matrix(newdata))))
    }
    shap_vals <- tryCatch({
      fastshap::explain(xgb_final, X = Xnum, pred_wrapper = pred_fun, nsim = 50)
    }, error = function(e) NULL)
    if (is.null(shap_vals)) return(plotly_empty() %>% layout(title = "SHAP not available"))
    shap_row <- shap_vals[idx, , drop = FALSE] %>% pivot_longer(everything(), names_to = "feature", values_to = "shap")
    p <- ggplot(shap_row %>% arrange(desc(abs(shap))) %>% head(15), aes(x = reorder(feature, shap), y = shap, fill = shap > 0)) +
      geom_col() + coord_flip() + labs(x = "", y = "SHAP value", title = "Approx SHAP contributions (selected customer)") + theme_minimal()
    ggplotly(p)
  })
  
  # Importance plots
  output$rf_imp_plot <- renderPlot({
    if (!is.null(top20_rf)) {
      top20_rf %>% head(15) %>% ggplot(aes(x = reorder(Feature, Overall), y = Overall)) + geom_col(fill = "#4daf4a") + coord_flip() + labs(title = "Random Forest top predictors", x = "", y = "Importance") + theme_minimal()
    } else {
      plot.new(); text(0.5, 0.5, "RF importance not available")
    }
  })
  
  output$xgb_imp_plot <- renderPlot({
    if (!is.null(top20_xgb)) {
      top20_xgb %>% head(15) %>% ggplot(aes(x = reorder(Feature, Gain), y = Gain)) + geom_col(fill = "#984ea3") + coord_flip() + labs(title = "XGBoost top predictors", x = "", y = "Gain") + theme_minimal()
    } else {
      plot.new(); text(0.5, 0.5, "XGB importance not available")
    }
  })
  
  # CI table
  output$ci_table <- renderDT({
    req(ci_tbl)
    datatable(ci_tbl, options = list(pageLength = 10))
  })
  
  # Download sample predictions
  output$download_sample <- downloadHandler(
    filename = function() paste0("predictions_sample_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- df()
      req(d)
      write_csv(d %>% select(PseudoID, pred_prob, pred_decile, target_attrit), file)
    }
  )
  
  # Expose whether SHAP and CI are available to UI for conditionalPanel
  output$shapAvailable <- reactive({
    !is.null(xgb_final) && requireNamespace("fastshap", quietly = TRUE)
  })
  outputOptions(output, "shapAvailable", suspendWhenHidden = FALSE)
  
  output$ciAvailable <- reactive({
    !is.null(ci_tbl)
  })
  outputOptions(output, "ciAvailable", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)