# PKBOIN-12 fake upload datasets

Use these with Method = PKBOIN-12, Upload trial data, Number of doses = 5.
Defaults expected: r_P = 6000, r_I/r_P = 0.6, zeta1 = 4800, C_P = 0.95.

All files are patient-level and include:
patient_id, cohort, dose, dlt, response, pk

## Files

- pkboin_ds1_normal_replay.csv
  - Basic positive-control replay. PK is adequate after dose 2; no intentional PK termination.

- pkboin_ds2_dose1_pk_low_no_prune.csv
  - Dose 1 has n = 6 and very low PK. Paper-consistent behavior: dose 1 PK flag must not prune dose 1 because d = 1 has no lower dose.

- pkboin_ds3_interior_pk_prunes_lowest.csv
  - Dose 2 has n = 6 and very low PK. Expected: PK elimination removes the lowest uneliminated lower dose, dose 1.

- pkboin_ds4_top_dose_pk_terminates.csv
  - Top dose 5 has n = 6 and very low PK. Expected: PK elimination flags all doses and terminates.

- pkboin_ds5_final_obd_pk_floor.csv
  - Dose 1 has low PK and dose 2+ adequate PK. Expected: final OBD selection uses d_PK_min from the PK PAVA step, so dose 1 should not be selected if it falls below the final PK floor.

## Quick R check

```r
setwd("C:/Users/TopSoarer/Desktop/Dr.Liu Personal/Git-Rshiny/oncology-stein-boin12/OBD12")
source("app.R")

f <- "C:/Users/TopSoarer/Documents/Codex/2026-07-06/c-users-topsoarer-desktop-dr-liu/outputs/pkboin_upload_examples/pkboin_ds3_interior_pk_prunes_lowest.csv"
x <- read.csv(f)
v <- pkboin_validate_upload(x, 5)
stopifnot(v$ok)

design <- list(phi_T=.35, phi_E=.25, CT=.95, CE=.9)
b <- boin_boundaries(.35)
pk <- list(r_P=6000, r_I_mult=.6, C_P=.95, CV=.25, g_P=1,
           zeta1=pkboin_zeta1(6000,.6))
u <- c(100,60,40,0)

rp <- pkboin_replay_uploaded(v$patients, design, b, pk, u, 5)
rp$log
rp$final_obd$summary
```
