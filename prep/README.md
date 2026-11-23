# prep/: data preparation (Stata)

Builds the harmonized S2S files that `RUN_S2S.R` reads, from the survey data in
`P:\SARMD\SARDATABANK\WORKINGDATA` and the external inputs in `P:\SARMD\SARDATABANK\S2S\IND\inputs`.
Outputs go to `P:\SARMD\SARDATABANK\S2S\IND\harmonized\<FY2022|FY2023|CY2023>\`.

> Status: the do-files are copied **as they were** (with their original paths) as a first step.
> The master (`00_master.do`) with one block of paths per user comes next.

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

- Paths are hard-coded to the old OneDrive folders.
- The anchor CPI (177.7042) is typed in every PLFS file; FY2023 should use 186.7333 and CY2023 181.7875.
- HCES 2023-24 names its state dummies `State_` (the R code expects `state_`).
- CY2023: prices to CY2023 average prices, CY2023 spatial deflators, the household CPI by year,
  the weight factors and the paths (`code` not defined) still to be done.
