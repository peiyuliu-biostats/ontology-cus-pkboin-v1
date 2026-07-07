# BOIN12 upload-mode verification datasets

All expected results below were computed from an independent Python re-implementation
of the same BOIN12 rules the app uses, under the **default design settings**. Before
uploading, set the sidebar to these values (they are the app defaults):

| Parameter | Value |
|---|---|
| Method | BOIN12 |
| Mode | Upload |
| n_dose (number of doses) | **5** |
| phi_T (target toxicity) | 0.35 |
| phi_E (efficacy futility) | 0.25 |
| CT / CE | 0.95 / 0.90 |
| phi1 / phi2 (auto) | 0.21 / 0.49 |
| utilities u1,u2,u3,u4 | **100, 60, 40, 0** |

Derived constants shown in Design/Flowchart should read:
**lambda_e = 0.2763, lambda_d = 0.4189, u_b = 70.50, N* = 6.**

Column contract (cohort-level files): `cohort, dose, n, n1, n2, n3, n4` where the four
joint categories are `n1=eff&no-tox`, `n2=eff&tox`, `n3=no-eff&no-tox`, `n4=no-eff&tox`
and `n1+n2+n3+n4 = n`. Toxicity count = `n2+n4`, efficacy count = `n1+n2`.

> RDS values are rounded to 3 decimals. Small differences in the last digit are fine
> (Beta-tail numerics); the **ordering** and the **decisions** must match exactly.

---

## ds1_basic_escalation.csv — RDS pick + escalate/stay/de-escalate

Trial Conduct replay log should be:

| cohort | dose | n≥6 | admissible | RDS (per dose) | decision | next |
|---|---|---|---|---|---|---|
| 1 | 1 | no | {1,2} | d1=.269 d2=.295 | escalate | 2 |
| 2 | 2 | no | {1,2,3} | d1=.269 d2=.501 d3=.295 | stay | 2 |
| 3 | 3 | no | {2,3,4} | d2=.501 d3=.587 d4=.295 | stay | 3 |
| 4 | 3 | yes | {2,3} | d2=.501 d3=.530 | stay | 3 |
| 5 | 4 | no | {3} | d3=.530 | de-escalate | 3 |
| 6 | 3 | yes | {2,3,4} | d2=.501 d3=.604 d4=.417 | stay | 3 |

**Final OBD:** p̃ = (0, 0, 0.222, 0.667) for doses 1–4; **d_MTD = 3**;
EU_d (posterior-mean, Dirichlet(1,1,1,1)) = d1:54.29, d2:62.86, d3:69.23, d4:60.00;
admissible (≤MTD, not elim) = {1,2,3}; **OBD = dose 3** (EU 69.23 is the max among
doses ≤ d_MTD).

> Note: EU_d here is the **posterior-mean** utility Σ u_k·(1+n_k)/(4+n), the estimate
> BOIN12 uses for final OBD selection — not the raw observed-frequency utility. On the
> final counts (d1 n=3, d2 n=3, d3 n=9, d4 n=3) dose 3 has both the most data and the
> highest posterior-mean utility, so it is the OBD.

---

## ds2_Nstar_branch.csv — N* branching (admissible set narrows at n≥6)

Watch dose 2 as its cumulative n grows 3 → 6 → 9:

| cohort | dose | cum n at dose | n≥6 | admissible | decision | next |
|---|---|---|---|---|---|---|
| 1 | 1 | 3 | no | {1,2} | escalate | 2 |
| 2 | 2 | 3 | no | {1,2,3} | escalate | 3 |
| 3 | 2 | 6 | yes | {1,2,3} | escalate | 3 |
| 4 | 2 | 9 | yes | {3} | **fast-escalate (n≥9)** | 3 |
| 5 | 3 | 3 | no | {2,3,4} | stay | 3 |
| 6 | 3 | 6 | yes | {2,3} | stay | 3 |

Note cohort 4: dose 2 has n=9, low toxicity, and dose 3 has been tried, so the
`n≥9` shortcut applies and the admissible set collapses toward escalation.

**Final OBD:** d_MTD = 3; EU_d (posterior-mean) = d1:54.29, d2:58.46, d3:66.00; **OBD = dose 3**.

---

## ds3_fast_escalation.csv — n≥9 fast-escalation from dose 1

| cohort | dose | cum n | n≥6 | admissible | decision | next |
|---|---|---|---|---|---|---|
| 1 | 1 | 3 | no | {1,2} | stay | 1 |
| 2 | 1 | 6 | yes | {1,2} | stay | 1 |
| 3 | 1 | 9 | yes | {2} | **fast-escalate (n≥9)** | 2 |

At cohort 3, dose 1 has n=9, zero toxicity, and dose 2 has never been tried →
the paper's fast-escalation shortcut fires; admissible set = {2}, decision =
fast-escalate. (Trial data ends here, so it stops after that recommendation.)

**Final OBD:** only dose 1 tried; d_MTD = 1; **OBD = dose 1** (EU posterior-mean 66.15).

---

## ds4_cascade_elimination.csv — cascade toxicity elimination

Dose 1 is extremely toxic (6/6 then 12/12 DLTs):

| cohort | dose | cum n/tox | eliminated | decision |
|---|---|---|---|---|
| 1 | 1 | 6 / 6 | **{1,2,3,4,5}** | stop: no admissible dose |
| 2 | 1 | 12 / 12 | {1,2,3,4,5} | stop: no admissible dose |

The safety rule `Pr(p1 > 0.35 | data) > 0.95` fires at dose 1 and **cascades to all
higher doses** (1 through 5), so every dose is eliminated.

**Final OBD:** none (all doses eliminated) — the OBD tab should say
"No OBD selected (all doses eliminated)".

---

## ds5_mtd_anchor.csv — MTD-anchor restricts OBD to ≤ d_MTD

This is the key test that OBD is **not** just the max-utility dose. Dose 4 has the
highest expected utility, but the MTD anchor forces the choice lower.

Replay ends with the trial oscillating around dose 4. Final estimates (EU_d is the
posterior-mean utility Σ u_k·(1+n_k)/(4+n)):

| dose | p̃ (isotonic) | EU_d |
|---|---|---|
| 1 | 0.000 | 54.29 |
| 2 | 0.000 | 62.86 |
| 3 | 0.333 | 57.14 |
| 4 | 0.333 | **72.00** ← highest EU |
| 5 | 0.667 | 64.00 |

**d_MTD = 3** (isotonic toxicity closest to 0.35 is at dose 3). Because OBD is
restricted to doses ≤ d_MTD = 3, dose 4 (EU 72.00) is **excluded**, and among
{1,2,3} the max EU is dose 2 (EU 62.86). **OBD = dose 2.**

> If the app instead returned dose 4 here, the MTD-anchor (step 3) would be broken.
> The `le_MTD` column in the OBD summary table should be TRUE only for doses 1–3.

---

## ds1_patient_level.csv — patient-level format (same trial as ds1)

Identical trial to ds1 but in the alternative accepted format
`patient_id, cohort, dose, dlt, response` (one row per patient, dlt/response ∈ {0,1}).
After the app aggregates it, **every Conduct/OBD result must be identical to
ds1_basic_escalation.csv** — this verifies the patient→cohort aggregation.

Mapping used: (dlt,response) = (0,1)→n1, (1,1)→n2, (0,0)→n3, (1,0)→n4.

---

## ds6_with_n0_placeholder.csv — n=0 row is silently dropped

Three rows, the middle one is an untried-dose placeholder with n=0:

```
cohort,dose,n,n1,n2,n3,n4
1,1,3,1,0,2,0
2,2,0,0,0,0,0     <- n=0 placeholder, should be dropped with a warning
3,2,3,2,0,1,0
```

Expected: upload succeeds with a **warning** that 1 placeholder row was ignored;
the analysis proceeds on 2 cohorts (dose 1 then dose 2). It must **not** be rejected.

---

## What to check in each tab

- **Trial Conduct**: the replay log's `admissible_set`, `rds_by_dose`, `n_ge_Nstar`,
  `decision`, `next_dose` match the tables above row-for-row.
- **OBD Determination**: `d_MTD`, `le_MTD` flags, `EU_d`, `RDS`, and the starred OBD
  match. For ds4 it shows no OBD; for ds5 the OBD is 2 (not 4).
- **Data** (upload mode): the snapshot shows lambda_e=0.2763, lambda_d=0.4189,
  u_b=70.50, N*=6; the replay log and final OBD match; the download produces a CSV
  with the snapshot + raw cohorts + replay log + per-dose OBD summary.
- **No simulate-mode leakage**: upload mode must never show p_true/q_true, selection %,
  mean allocation, or a "true OBD" star.
