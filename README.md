---# County Childhood Vaccination Uptake

**Social Determinants, Broadband Access, and Internet Influence**  
**Author:** Geoffrey Bruder  
**Updated:** April 2026

---

## Project status — current, accurate snapshot

**Status:** Active / In progress — core ingestion, preprocessing, and modeling baseline completed; EDA was recently revisited. The repository now contains reproducible ingestion and preprocessing code, imputation and group-imputation audits, preprocessor artifacts, updated reports (R Markdown), and a working ElasticNet baseline with nested CV diagnostics. Spatial joins and advanced spatial modeling remain planned next steps.


---

## Summary

We build reproducible county‑level models to predict childhood vaccination coverage using CDC VaxView, ACS covariates, and optional COVIDcast survey signals; preprocessing artifacts, EDA, and an interpretable ElasticNet baseline are available in the repo.

---

## What has been completed:

- **Ingestion & provenance**
  - Raw files ingested with attempted FIPS padding and type coercion.
  - Provenance recorded: `outputs/run_provenance.json` and `data_vintages.csv` (when available).
- **Cleaned analysis table**
  - `data_clean/analysis_table.csv` produced (merged CDC + ACS + optional COVIDcast).
- **Preprocessing artifacts**
  - `models/preprocessor.joblib` (ColumnTransformer pipeline).
  - `models/simple_imputer_numeric_median.joblib`.
  - Preprocessed arrays exported to `outputs/` for reproducibility.
- **Imputation audits**
  - `outputs/data_imputation_and_cleaning_summary.json` (global imputation audit).
  - `outputs/group_imputation_audit.json` and `outputs/per_state_missing.csv` (per‑state missingness).
- **Exploratory Data Analysis**
  - Formal EDA report: `M03_EDA_Report.Rmd` (revised to address instructor feedback).
  - EDA includes: target summary, predictor descriptive tables, correlogram, identification of strong correlations, bivariate plots with Pearson r, missingness diagnostics, and basic spatial checks (presence/absence of lat/lon or FIPS).
- **Modeling baseline**
  - Nested cross‑validation pipeline implemented.
  - ElasticNet baseline trained and evaluated (CV RMSE ≈ 4.07).
  - Model artifacts and summaries saved to `outputs/` (e.g., `models_summary.json`, `elasticnet_coefficients.csv`, `elasticnet_preds_full.csv` when generated).
- **Notebooks**
  - `notebooks/analysis_updated_full.ipynb` — ingestion, preprocessing, audits, and artifact exports. Run top→bottom to reproduce artifacts.
- **Reproducibility**
  - Scripts and notebooks save artifacts atomically and include checks; README and Rmd include instructions to re-run.

---

## What changed since the original proposal

- Focused on robust ingestion and reproducible preprocessing rather than adding many external data sources immediately.
- Implemented group (state) median imputation and produced an audit of what was filled per state.
- Performed secondary EDA to address course feedback: added code, concrete interpretations tied to our data, and basic spatial diagnostics.
- Produced an interpretable ElasticNet baseline with nested CV and conformal-interval diagnostics (conformal outputs saved when available).

---

## Repository layout (key files)

```
.
├─ data_raw/                      # Place raw CDC, ACS, optional COVIDcast files here
├─ data_clean/
│  └─ analysis_table.csv          # merged, cleaned table used for EDA and modeling
├─ notebooks/
│  └─ analysis_updated_full.ipynb # ingestion, preprocessing, audits, artifact exports
├─ models/
│  ├─ preprocessor.joblib
│  └─ simple_imputer_numeric_median.joblib
├─ outputs/
│  ├─ data_imputation_and_cleaning_summary.json
│  ├─ group_imputation_audit.json
│  ├─ per_state_missing.csv
│  ├─ models_summary.json
│  ├─ elasticnet_coefficients.csv
│  └─ (figures, preds, CSV artifacts)
├─ figures/                       # generated figures used by reports
├─ M03_EDA_Report.Rmd             # Formal EDA report (revised)
├─ M08_Final_Written_Deliverables.Rmd
├─ README.md                      # (this file)
└─ tests/                         # minimal tests (suggested)
```

---

## How to reproduce the current artifacts (exact steps)

1. **Activate environment**
   - Example: `conda activate county-vax` (or the environment you use).
2. **Place raw data**
   - Put the CDC VaxView CSV and ACS CSV (and optional COVIDcast CSV) into `data_raw/`.
   - Expected filenames (not strict; notebook will try to detect coverage column):  
     - `cdc_vaxview_*.csv` (CDC coverage file)  
     - `acs_2021_counties_merged_validated.csv`  
     - optional `covidcast_*.csv`
3. **Run ingestion & preprocessing**
   - Open `notebooks/analysis_updated_full.ipynb` and run top→bottom. This will:
     - Create `data_clean/analysis_table.csv`
     - Save `models/preprocessor.joblib` and `models/simple_imputer_numeric_median.joblib`
     - Write imputation audits to `outputs/`
4. **Render the EDA report**
   - From R (project root):  
     ```r
     rmarkdown::render("M03_EDA_Report.Rmd")
     ```
   - Or open `M03_EDA_Report.Rmd` in RStudio and Knit to PDF/HTML.
5. **Run modeling cells**
   - The notebook contains nested CV and model training cells; run them to regenerate `outputs/models_summary.json` and model artifacts.

---

## Key findings so far (data‑specific, concise)

- **Target variability:** The target (`coverage_estimate`) has a standard deviation consistent with the ElasticNet baseline RMSE (~4.07), indicating baseline errors are on the same scale as observed variability.
- **Missingness:** The optional survey signal `vaccinate_children` has substantial missingness in many counties; group (state) median imputation was applied and audited. Treat this predictor cautiously and run sensitivity checks.
- **Collinearity:** The EDA identifies strong predictor pairs (|r| > 0.6) when present; these pairs are documented in the EDA and should be handled via regularization, variable selection, or dimensionality reduction.
- **Population density:** If not present in the ACS extract, computing population density (population / land area) is a high-priority next step because it plausibly mediates access and spatial clustering.
- **Spatial structure:** Basic checks are in place; formal spatial analysis (Moran’s I, LISA, hotspot mapping) is planned and prioritized.

---

## Remaining work and next steps (priority order)

1. **Compute population density** for all counties and re-run EDA checks and bivariate analyses.  
2. **Join county geometries** (TIGER/Line or equivalent) and compute Moran’s I and LISA; produce maps and hotspot analyses.  
3. **Expand predictor set** to ~20–25 key variables and produce a compact VIF/collinearity report to guide modeling.  
4. **Partial correlation and stratified analyses** for the strongest correlated pairs to assess confounding.  
5. **Heteroskedastic-aware uncertainty quantification** (improve conformal intervals or model residual variance).  
6. **Unit tests & CI** for ingestion and preprocessing; add minimal tests in `tests/`.  
7. **Stakeholder deliverables:** one‑page county summaries and a short slide deck for public‑health partners.

---

## Notes about the EDA revision

- The EDA was reworked to address instructor feedback: added code, concrete interpretations tied to our data, and spatial diagnostics. That revision was a focused effort to improve the course grade and to make the EDA formally suitable for stakeholders; it is integrated into the main project artifacts (Rmd + notebook + outputs).

---

## Contact

Open an issue in the repo.
---

## Data use and license

All data sources are public (CDC, ACS, COVIDcast). Users must comply with each data source’s terms of use. This repository contains derived, aggregate county‑level data and code for research and educational purposes.

---

*This README reflects the current, implemented state of the project (April 2026). 