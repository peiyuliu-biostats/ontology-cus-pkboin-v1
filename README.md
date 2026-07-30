# Oncology Early-Phase Adaptive Dose Optimization

[![View ShinyApp](https://img.shields.io/badge/View-ShinyApp-blue?style=for-the-badge&logo=R)](https://peiyuliu.shinyapps.io/oncology-cus-pkboin-v1/)

**Live demo:** https://peiyuliu.shinyapps.io/oncology-pkboin-ERcus/

An interactive R Shiny platform for **optimal biological dose (OBD)** selection in early-phase oncology trials. It brings three complementary methodologies under one interface, reflecting the shift — advanced by the FDA's **Project Optimus** — from finding the maximum tolerated dose (MTD) toward identifying the dose that best balances efficacy and toxicity.

---

## Methods

The app is organized into three independent dashboards, each self-contained with isolated state. Please turn to live app methods siderbar to view all methods formulas and details.

### 1. PK-BOIN12 Suite
A family of model-assisted, adaptive Phase I/II interval designs that select the OBD by jointly weighing toxicity and efficacy through a utility / rank-based desirability score (RDS). Three nested methods are available from a single Method selector:

- **BOIN12** — utility-based dose finding on binary toxicity and efficacy (Lin et al., 2020).
- **PKBOIN-12** — extends BOIN12 with a continuous pharmacokinetic (PK) exposure outcome: a PK cutoff (ζ₁) expands the admissible dose set, and a PK-based elimination rule removes under-exposed doses (Sun & Tu, 2024).
- **TITE-PKBOIN-12** — adds time-to-event handling of late-onset toxicity/efficacy via approximated-likelihood imputation, enabling decisions with pending outcomes and rapid accrual.

Each method degrades gracefully to the one below it, so PKBOIN-12 with a very low target PK recovers BOIN12 exactly.

### 2. STEIN
An independent simple toxicity–efficacy interval design for seamless Phase I/II dose finding, offered as an alternative adaptive design to the BOIN family (Lin & Yin, 2017).

### 3. ER–Clinical Utility Score (CUS)
A post-hoc, exposure–response scoring framework. It fits logistic/linear/log-linear/Exponential/Coxph exposure–response curves for efficacy and safety, maps them to utility scores, and aggregates them into a single Clinical Utility Score to locate the optimal therapeutic window (Cheng et al., 2024).

---

## Key Features

- **Two working modes** — *Simulate* to study a design's operating characteristics under user-specified true dose–response curves, or *Upload* to replay a design on real cohort- or patient-level trial data.
- **Full design workflow** — for each adaptive design: derived decision boundaries, an annotated decision flowchart, editable true-rate scenarios, operating characteristics over thousands of replications, trial-conduct replay, and OBD determination.
- **PK-aware dosing** — PKBOIN-12 integrates individual-level PK variability into dose escalation, elimination, and final OBD selection.
- **Curated case library** — pre-loaded real-world oncology datasets (e.g. *Loncastuximab tesirine*, *Polatuzumab vedotin*) for the CUS framework.
- **Interactive analysis** — Plotly visualizations, debounced sensitivity controls, and DT-based decision tables throughout.
- **Modular architecture** — pure computation functions are decoupled from Shiny modules, with each dashboard's reactive state fully isolated for reliability and maintainability.

---

## Getting Started

```r
# install dependencies
install.packages(c(
  "shiny", "shinydashboard", "shinyBS", "shinyFeedback", "shinyjs",
  "shinybusy", "DT", "plotly", "ggplot2", "dplyr", "tidyr", "purrr",
  "stringr", "readxl", "mvtnorm"
))

# run from the ontology-cus-pkboin-v1/ directory (the folder containing app.R)
shiny::runApp("ontology-cus-pkboin-v1")
```

---

## References

- Lin R, Zhou Y, Yan F, Li D, Yuan Y. *BOIN12: Bayesian Optimal Interval Phase I/II Trial Design for Utility-Based Dose Finding in Immunotherapy and Targeted Therapies.* **JCO Precision Oncology**, 2020;4:1393–1402.
- Sun H, Tu J. *PKBOIN-12: A Bayesian Optimal Interval Phase I/II Design Incorporating Pharmacokinetics Outcomes to Find the Optimal Biological Dose.* **Pharmaceutical Statistics**, 2025;24:e2444.
- Lin R, Yin G. *STEIN: A Simple Toxicity and Efficacy Interval Design for Seamless Phase I/II Clinical Trials.* **Statistics in Medicine**, 2017;36:4106–4120.
- Cheng Y, Chu S, et al. *Exposure-Response–Based Multiattribute Clinical Utility Score Framework to Facilitate Optimal Dose Selection for Oncology Drugs.* **Journal of Clinical Oncology**, 2024.
