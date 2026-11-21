# India_S2S

Two-stage PMM-based survey-to-survey (S2S) imputation to reconstruct poverty and inequality
trends in India: consumption from the HCES is imputed into the PLFS (Stage 1), and the
imputed-to-abbreviated consumption ratio is carried to other PLFS rounds (Stage 2).
Methodology: Fernandez and Olivieri (July 2025), *Evolution of Post-2011 Poverty in India:
A Survey-to-Survey Imputation Approach*.

## How to run

1. Clone this repo into `C:/Users/<you>/WBG_DEV/Github/` (GitHub Desktop).
2. Open `RUN_S2S.R` and, the first time only, add your block of paths in section 2:
   - `input_path`: the folder with the harmonized inputs of each exercise (read only)
   - `output_path`: where your runs are saved
3. In section 1, choose the exercise and what to run. The parameters of each exercise
   (years, input files, simulations, prices) are in section 3.
4. Run it: in RStudio click **Source** (top right of the editor) or press **Ctrl+Shift+S**.
   Do not use **Run** or **Ctrl+Enter**, which send the lines one by one to the console.
   Wait for `Done. Results in: ...` in the console. No need to open `India_S2S.Rproj`.

Each run creates its own folder, `<output_path>/<exercise>/<date_time>_<nsim1>x<nsim2>/`, with
`data/`, `models/`, `tables/`, `figures/`, `logs/`, a copy of the master file used and
`run_log.txt` (code version, inputs, settings, R and package versions). Nothing is written
into the repo or the input folder.

## Exercises (section 3 of `RUN_S2S.R`)

| Exercise | What it does |
|---|---|
| `fy2022_backcast` | HCES 2022-23 anchor, backward to PLFS 2017-18 ... 2021-22 (the July 2025 note) |
| `fy2023` | to come: HCES 2023-24 anchor |
| `cy2023_forward` | to come: calendar-year HCES 2023 anchor, forward to PLFS CY2024 and CY2025 |

Expected results of the note: `tests/expected_note_2025.md`.

## Files

| File | What |
|---|---|
| `RUN_S2S.R` | the master file: what to run, your paths, parameters of each exercise (the only file you edit) |
| `Code/01_setup.R` | run folder, logs, packages, data preparation (if `run_prep`) |
| `Code/02_stage1_clean.R` ... `05_stage1_outputs.R` | Stage 1: HCES to PLFS in the anchor year |
| `Code/06_stage2_clean.R` ... `09_stage2_outputs.R` | Stage 2: anchor PLFS to the other PLFS rounds |
| `prep/` | to come: Stata data preparation (inputs to harmonized files) |
| `Outputs/` | figures of the July 2025 note, as published (not written by runs) |
| `tests/` | expected results of the note |

## Packages

Missing R packages are installed from CRAN at the first run. renv is not activated
automatically (it cannot download itself on World Bank laptops); `renv.lock` is kept as the
record of the package versions used for the note, and each run logs the versions it used.
