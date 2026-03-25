### Preliminary Project Proposal

#### Team roles

**Team Lead:** Geoffrey Bruder  
**Recorder:** Geoffrey Bruder  
**Spokesperson:** Geoffrey Bruder

#### Project title

**County Childhood Vaccination Uptake: Social Determinants, Broadband
Access, and Internet Influence**

#### Background and question

**Research question:** What county‑level social determinants of health,
broadband/internet access measures, and internet search activity predict
lower childhood vaccination uptake, and which counties are most at risk
of under‑vaccination?

This project addresses a clear public‑health need: state and county
health boards require actionable, county‑level risk profiles to
prioritize outreach and resource allocation for childhood immunizations.
Integrating SDOH, broadband availability, and internet search behavior
produces a richer signal than SDOH alone and supports targeted
interventions.

> **Quoted guidance from the course materials:** “Begin your plan by
> clearly defining what question you are trying to answer. This question
> should specify both your target variable (the main outcome you are
> trying to predict or explain) and the context in which it applies.”

#### Stakeholder

**Primary stakeholder:** State Health Board (or county public‑health
departments).  
**Secondary stakeholders:** CDC regional offices; public‑health NGOs.

#### Hypothesis and prediction

**Hypothesis:** County childhood vaccination rates are associated with
structural SDOH (income, education, insurance), broadband availability,
and local internet search interest in vaccine‑related misinformation.  
**Prediction:** After adjusting for SDOH, counties with higher broadband
access but elevated search interest in vaccine‑hesitancy keywords will
show lower vaccination uptake than comparable counties with similar SDOH
but lower misinformation search activity.

#### Data and sources

**Outcome (response):** County childhood vaccination coverage (e.g., MMR
completion for children 19–35 months). Source: CDC VaxView / Socrata
endpoints.  
**Predictors (candidate):** - **Census ACS API:** median household
income; educational attainment; % households with broadband
subscription; race/ethnicity; population density.  
- **AHRQ SDOH:** composite SDOH indicators by county.  
- **FCC Broadband / BDC:** provider counts; served/unserved
indicators.  
- **Google Trends (pytrends):** relative search interest for
vaccine‑related queries; rolling averages and lagged features.  
- **Optional:** state immunization registry extracts or BRFSS for
validation.

**Join key:** county FIPS.

#### Tentative analysis plan

**Preprocessing:** document vintages; align ACS 5‑yr structural features
with monthly/annual vaccination and Trends data; impute missingness
(MICE or state medians where appropriate); create codebook.  
**Feature engineering:** broadband penetration (% households with
broadband), provider density, SDOH composite indices, Google Trends
rolling 3‑month average and 6‑month lag, interaction terms (broadband ×
misinformation intensity).  
**Unsupervised component:** PCA for dimensionality reduction of SDOH
features; k‑means or hierarchical clustering to identify county
typologies.  
**Baseline model:** linear regression (continuous outcome) or logistic
regression (binary threshold).  
**Advanced models:** Random Forest; XGBoost/LightGBM; Elastic Net for
interpretability.  
**Validation:** 80/20 train/test split; 10‑fold cross‑validation for
tuning; subgroup fairness checks.  
**Metrics:** Regression — RMSE, MAE, $`R^2`$. Classification —
Precision, Recall, F1, ROC‑AUC. Operational metric — <precision@k> for
top‑risk counties.

#### Pitfalls and mitigations

- **Temporal misalignment:** ACS 5‑yr vs. monthly Trends. *Mitigation:*
  use ACS for structural features; use rolling windows and lags for
  Trends; document vintages.  
- **Google Trends normalization:** relative values and sampling
  variability. *Mitigation:* use consistent query lists, rolling
  averages, multiple keywords, and sensitivity checks.  
- **Small counts / suppression:** suppressed county counts.
  *Mitigation:* aggregate where necessary; flag and report
  uncertainty.  
- **API quotas:** caching, backoff, and token use.

#### Technical details

**Language / stack:** Python (requests, pandas, geopandas, scikit‑learn,
xgboost/lightgbm, shap, pytrends, folium).  
**Reproducibility:** GitHub repo with README, data acquisition scripts,
notebooks, and codebook.  
**Resources:** Census API key; optional FCC account; compute for model
tuning.

------------------------------------------------------------------------
