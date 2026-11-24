# prep/: data preparation (Stata)

Builds the harmonized S2S files that `RUN_S2S.R` reads, from the survey data in
`P:\SARMD\SARDATABANK\WORKINGDATA` and the external inputs in `P:\SARMD\SARDATABANK\S2S\IND\inputs`.
Outputs go to `P:\SARMD\SARDATABANK\S2S\IND\harmonized\<FY2022|FY2023|CY2023>\`.

> Status: `00_master.do` runs **fy2022_backcast** (to be validated with `99_compare.do`).
> fy2023 and cy2023_forward: their do-files are here as they were found (old paths); the master does not run them yet.

## How to run

```
1. 00_master.do  section 1: your paths   section 2: exercise + test = 1
2. Do 00_master.do   ─► harmonized\FY2022_test\     (intermediate files and log in _work\)
3. Do 99_compare.do  ─► compares FY2022_test with FY2022 (the files of the note) ─► _work\compare_FY2022.csv
4. All IDENTICAL? set test = 0 and run again ─► harmonized\FY2022\ (what RUN_S2S.R reads)
```

Parameters set in the master and used by the do-files: `${anchor_cpi}` (temporal deflation of PLFS),
`${spdef_file}` (spatial deflators built by the HCES step), `${s2s_inputs}` (PRIMUS, household CPI, weights),
`${harmonized}` (output folder), `${input}`/`${output}` (per step).

## Workflow

```
FY2022 (HCES 2022-23 anchor, backward to PLFS 2017-2021)
  hces/IND_2022-23_HCES_v02_M.do ─► hces/IND_2022-23_HCES_v02_M_v02_A_s2s_HCES_to_PLFS.do ─┬─► HCES22_s2s.dta
     (MoSPI levels + PRIMUS 2022-23)                                                       └─► Sp_deflators_HCES_22.dta
  plfs/IND_2022_PLFS-V1_v01_M.do ─► plfs/IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.do ──────► IND_2022_PLFS_..._HCES_to_PLFS.dta
  plfs/IND_<y>_PLFS-V1_v01_M.do  ─► plfs/IND_<y>_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.do ───────► IND_<y>_PLFS_..._PLFS_to_PLFS.dta  (y = 2017-2023)

FY2023 (HCES 2023-24 anchor, Stage 1 only)
  hces/IND_2023-24_HCES_v01_M.do ─► hces/IND_2023-24_HCES_v01_M_v01_A_s2s_HCES_to_PLFS.do ─┬─► HCES23_s2s.dta
     (MoSPI levels + PRIMUS 2023-24 v2)                                                    └─► Sp_deflators_HCES_23.dta
  hces/correct_hhid.do (hhid + INTPOV mpce_2023_all) ─► HCES23_s2s_v2.dta;  hces/add_value30d_file.do (INTPOV mpce_2023)
  plfs/IND_2023_PLFS-V1_v01_M.do ─► plfs/IND_2023_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.do ──────► IND_2023_PLFS_..._HCES_to_PLFS.dta

CY2023 (calendar-year HCES 2023 anchor, forward to PLFS CY2024-2025; Leo Tornarolli, Nov 2026, work in progress)
  hces/IND_2023_HCES_v01_M_v01_A_s2s_HCES_to_PLFS.do   (SARMD HCES 2022-23 panels 6-10 + 2023-24 panels 1-5)
  plfs/IND_2023_PLFS_v02_M_v01_A_s2s_HCES_to_PLFS.do   (SARLAB PLFS CY2023, Stage 1 receiver)
  plfs/IND_<y>_PLFS_v0X_M_v01_A_s2s_PLFS_to_PLFS.do    (SARLAB PLFS CY2023, 2024, 2025, Stage 2)
```

## Folders

| Folder | What |
|---|---|
| `hces/` | HCES: preparation of the MoSPI levels and harmonization to the PLFS covariates |
| `plfs/` | PLFS: preparation of the MoSPI text files (`-V1_v01_M`) and harmonization, one file per year |
| `cpi/` | average CPI of the anchor period (`cpi_master_monthly`) |
| `eus/` | EUS / NSS 2011 (robustness checks of the note only) |
| `doc/` | variable lists of the harmonization (FY and CY) |
| `legacy/` | the original masters (`0-main.do` of Stage 1 and 2, FY2022 and FY2023) and two HCES do-files of the old Stage 2 folder that were not run, kept for reference |

## Known issues (to fix in the next steps)

- FY2022 files and the PLFS_to_PLFS files: paths and anchor CPI now come from the master. FY2023 and CY2023 files still have the old paths.
- FY2023 should use the anchor CPI 186.7333 and CY2023 181.7875 (PLFS 2023 FY file still has 177.7042 typed in).
- EUS 2011 (robustness only) is not in the master.
- PLFS 2020 and 2021: the official collection (datalibweb SARRAW, P:) has hhvisit1/personvisit1 .dta, not the MoSPI text files.
  `plfs/IND_<y>_PLFS-V1_v01_M_dta.do` reads them (checked: same records and values as the text files, Nov 2026).
- HCES 2023-24 names its state dummies `State_` (the R code expects `state_`).
- CY2023: prices to CY2023 average prices, CY2023 spatial deflators, the household CPI by year,
  the weight factors and the paths (`code` not defined) still to be done.
