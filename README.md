  Behavioral Delta Risk Calibrated Attrition Scoring for Targeted Retention

UPDATE 05/26

# Behavioral Delta Risk — Presentation and Shiny Explorer

This repository contains the tuned pipeline, stakeholder slides, and a local Shiny explorer for the
Behavioral Delta Risk project. The app and slides read precomputed artifacts saved by the tuned R Markdown.

## Quick overview of deliverables
- `Rmds/Behavioral-Delta-Risk_Tuned_BruderG.Rmd` — tuned pipeline (run to regenerate artifacts).
- `artifacts/project_artifacts.RData` — saved model artifacts and plotting objects used by slides and app.
- `Rmds/slides_template.Rmd` → `outputs/BehavioralDelta_StakeholderSlides.html` — revealjs slide deck.
- `shiny/app.R` — Shiny explorer (run locally; no deployment required -  deploy.R script framework for future deployment in shiny folder).
- `artifacts/demo.mp4` — short demo video embedded in slides.

## Prerequisites
- R (>= 4.0) and RStudio recommended.
- Install required packages once:
```r
install.packages(c(
  "tidyverse","rmarkdown","revealjs","shiny","plotly","DT","pROC",
  "caret","recipes","rsample","xgboost","randomForest","vip","fastshap"
))


## Project Background

This project evaluates whether short term behavioral changes in customer activity improve prediction of attrition beyond static demographic and credit features. The practical goal is to provide a calibrated scoring model that the retention team can use to prioritize outreach and maximize retained lifetime value.

## Goal and stakeholders

The goal is to deliver a reproducible scoring pipeline and a calibrated model suitable for a pilot retention campaign. The primary stakeholder is the retention manager who needs a reliable list of customers to contact and a probability score that can be used to set outreach thresholds. The model is intended to support operational decisions and not to automate account closure or other irreversible actions.

## Research question

Do engineered short term behavioral delta features that capture quarter to quarter changes in transaction amount and transaction count and reductions in contact frequency add predictive value beyond static demographic and credit features?

## Hypothesis and predictions

The hypothesis is that recent negative behavioral changes are associated with higher attrition risk. I predict that adding delta features to a static feature set will increase discrimination measured by AUC and will reduce the Brier score indicating better probability accuracy. I predict that tuned ensembles will outperform logistic baselines and that Platt scaling will modestly improve calibration for the selected model.

## Methods

The pipeline begins with deterministic ingestion and a targeted leakage audit that removes direct identifiers while preserving an anonymized PseudoID. I engineered behavioral delta features that measure relative quarter to quarter changes in transaction amount and transaction count and I created binary flags for large drops and for recent contact reduction. Preprocessing uses median imputation on the training partition rare level consolidation one hot encoding zero variance removal and optional winsorization for extreme transaction values. Modeling uses a stratified 70 30 train test split. Hyperparameter tuning uses fivefold cross validation repeated five times optimizing two class ROC. Candidate models include a static logistic an enhanced logistic a tuned random forest and an XGBoost workflow that falls back to direct xgboost cross validation when caret xgbTree produces missing ROC metrics. Final evaluation reports holdout AUC Brier and calibration diagnostics. Ablation experiments and bootstrap resampling quantify the incremental value of behavioral features. Minimal artifacts are exported so downstream reports can render figures and tables without re running the full training pipeline.

## Status update

The preprocessing recipe and feature engineering are implemented and saved as a reproducible recipe. Behavioral delta features were engineered and validated. Random forest tuning completed successfully and produced stable cross validated results. Caret xgbTree produced missing ROC metrics in this environment so a robust direct xgboost cross validation fallback was implemented and validated. Models were evaluated on a held out test set and Platt scaling was applied to the selected model. Ablation experiments and bootstrap confidence intervals were computed to quantify the incremental value of behavioral features. Minimal artifacts including probability vectors ROC objects calibration outputs and bootstrap tables were exported to an artifacts folder for downstream reporting.

## Outcome

In the current run the enhanced logistic improved AUC and Brier relative to the static logistic. Tuned ensembles substantially outperformed logistic baselines on the holdout. The direct xgboost final model produced the highest AUC and the lowest Brier in this run. Platt scaling produced a small improvement in Brier while preserving AUC. Bootstrap ablation indicates the Random Forest AUC delta from behavioral features is statistically robust and XGBoost shows a smaller positive contribution. These results support a staged pilot using the calibrated ensemble subject to a final leakage re audit and temporal validation.
