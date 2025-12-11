# Oncology Dose CUS Framework 

[![View ShinyApp](https://img.shields.io/badge/View-ShinyApp-blue?style=for-the-badge&logo=R)](https://peiyuliu.shinyapps.io/ontology-cus-pkboin-v1/)

**Live Demo:** https://peiyuliu.shinyapps.io/ontology-cus-pkboin-v1/


## Overview

This R Shiny application implements the **Clinical Utility Score (CUS) framework** to facilitate **optimal dose selection for oncology drugs**.  
By integrating **Exposure-Response (ER)** relationships for both **efficacy** and **safety** endpoints, the app calculates a unified utility score to identify the **optimal therapeutic window**, aligning with the philosophy of the FDA's **Project Optimus**.


## Key Features

- **Case Study Library**  
  Includes pre-loaded datasets for real-world oncology drugs (e.g., *Loncastuximab tesirine*, *Polatuzumab vedotin*).

- **Interactive Visualization**  
  Dynamic plots showing the CUS curve overlaid with individual endpoint probabilities using Plotly.

- **Sensitivity Analysis**  
  Real-time, debounced sliders to explore how varying **weights (Safety vs. Efficacy)** impact the optimal dose selection.

- **High Performance**  
  Built with a modular architecture, utilizing:
  - `future` for asynchronous computation  
  - `bindCache` for instant rendering  



## Methodology

The framework calculates the optimal dose via:

### **1. ER Modeling**
Using Logistic Regression parameters (*Intercept* / *Slope*) to predict event probabilities.

### **2. Utility Function**
Mapping probabilities to utility scores:

- **Efficacy:** \( P \rightarrow P \)  
- **Safety:** \( 1 - P \rightarrow 1 - P \)

### **3. Aggregation**
Using a **weighted multiplicative model** to derive the final **Clinical Utility Score (CUS)**.



## References

Cheng Y, Chu S et al. *Exposure-Response–Based Multiattribute Clinical Utility Score Framework to Facilitate Optimal Dose Selection for Oncology Drugs.*  
**Journal of Clinical Oncology**, 2024.

Sun, H., & Tu, J. (2025). *PKBOIN-12: A Bayesian optimal interval Phase I/II design incorporating pharmacokinetics outcomes to find the optimal biological dose.*  
**Pharmaceutical Statistics**, Wiley.


