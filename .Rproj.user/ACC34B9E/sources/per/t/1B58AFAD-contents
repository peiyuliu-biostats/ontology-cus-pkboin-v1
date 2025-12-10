# R/data_cases.R

ep <- function(name, type, int, slope, weight = 1) {
  list(name = name, type = type, intercept = int, slope = slope, weight = weight)
}

case_library <- list(
  "Loncastuximab" = list(
    label = "Loncastuximab tesirine (Lymphoma)",
    desc = "ADC targeting CD19. Indication: Large B-cell lymphoma. Key tradeoff: ORR vs GGT/Skin reactions. Recommended PK ~ 0.66.",
    pk_min = 0, pk_max = 2.0, pk_ref = 0.66,
    endpoints = list(
      ep("ORR (Efficacy)", "Efficacy", -0.746, 1.81, weight = 1),
      ep("GGT >= Grade 2", "Safety", -4.3, 1.32, weight = 1),
      ep("Liver Function Tests", "Safety", -1.67, 0.79, weight = 1), # Added missing endpoint from Table 2
      ep("Skin/Nail Disorders", "Safety", -2.75, 1.13, weight = 1),
      ep("TEAE Discontinuation", "Safety", -1.46, 1.11, weight = 1)
    )
  ),
  "Polatuzumab" = list(
    label = "Polatuzumab vedotin (Lymphoma)",
    desc = "Ref: Fig S3 (1:1 weight). Recommended dose 1.8 mg/kg corresponds to PK approx 3400. CUS peaks later with 1:1 weights.",
    pk_min = 0, pk_max = 6000, pk_ref = 3400,
    endpoints = list(
      # Parameters estimated to match Fig S3 curves visually
      # Efficacy (BOR): Starts ~0.4, rises to ~0.7. Logistic approx: Int=-0.5, Slope=0.0003
      ep("Best Overall Response", "Efficacy", -0.5, 0.0003, weight = 1), 
      # Safety (Neuropathy): Starts low, rises to ~0.4. Logistic approx: Int=-3.0, Slope=0.0005
      ep("Peripheral Neuropathy", "Safety", -3.0, 0.0005, weight = 1)
    )
  ),
  "Tisotumab" = list(
    label = "Tisotumab vedotin (Cervical Cancer)",
    desc = "Ref: Fig S4. PK 55. Efficacy vs Ocular TEAEs.",
    pk_min = 20, pk_max = 70, pk_ref = 55,
    endpoints = list(
      # Parameters estimated from Fig S4
      ep("Objective Response", "Efficacy", -2.0, 0.05, weight = 1),
      ep("Ocular TEAEs", "Safety", -4.5, 0.08, weight = 1) 
    )
  )
)