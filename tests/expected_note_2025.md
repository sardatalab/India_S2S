# Expected results: July 2025 note (Fernandez and Olivieri)

Numbers a run of `config/fy2022_backcast.R` (1000 / 100 simulations) should reproduce.
Small differences are expected with fewer simulations.

## Table 2. Poverty (%) and Gini, 2022-23 (Stage 1)

Output files (in the run folder): `tables/table 2 poverty.csv` and `tables/table 2 gini.csv`.

| | PPP$3.0 | PPP$4.2 | PPP$8.3 | Gini |
|---|---|---|---|---|
| HCES, actual | 5.3 | 23.9 | 82.1 | 25.5 |
| PLFS, imputed | 5.7 | 25.0 | 84.3 | 24.1 |

Figure 10 (urban, PPP$8.3): PLFS imputed 72.5, HCES 68.1.

## Figure 13 A. Poverty (%), imputed PLFS 2017-2021 (Stage 2)

Output file (in the run folder): `tables/Poverty_trend_match_median.csv` (national rows).
2022 is official HCES.

| Line | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 (HCES) |
|---|---|---|---|---|---|---|
| PPP$3.0 | 16.1 | 12.1 | 10.8 | 9.2 | 8.7 | 5.3 |
| PPP$4.2 | 38.7 | 34.0 | 31.3 | 30.1 | 29.4 | 23.9 |
| PPP$8.3 | 83.7 | 83.0 | 83.0 | 84.2 | 83.3 | 82.1 |
