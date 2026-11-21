### =============================================================================
### India S2S: survey-to-survey imputation of consumption (HCES -> PLFS)
### Master file: the only file you need to open and edit.
###
### HOW TO RUN
###   1. Open this file in RStudio (no need to open India_S2S.Rproj).
###   2. First time only: add your block of paths in section 2.
###   3. Choose the exercise in section 1.
###   4. Click "Source" (top right of this editor), or press Ctrl+Shift+S.
###      Do NOT use "Run" or Ctrl+Enter: they send the lines one by one to the console.
###   5. Wait for "Done. Results in: ..." in the console. A full run takes many hours;
###      keep RStudio open and the computer awake (sleep off, plugged in).
###   Alternative without RStudio: in a terminal, Rscript RUN_S2S.R (from the repo folder)
###
### Sections: 1. What to run   2. Paths (one block per user, first time only)
###           3. Parameters of each exercise   4. Run (nothing to edit)
### =============================================================================

rm(list = ls())

#===============================================================================
# 1. What to run
#===============================================================================

exercise <- "fy2022_backcast"   # fy2022_backcast: HCES 2022-23 anchor, backward to PLFS 2017-2021 (July 2025 note)
                                # (to come: fy2023, cy2023_forward)
run_prep <- FALSE               # TRUE = rebuild the harmonized input files (Stata, prep/)
run_s2s  <- TRUE                # TRUE = run Stage 1 and Stage 2 (R)

#===============================================================================
# 2. Paths: one block per user, found by your Windows username
#===============================================================================
# input_path : for each exercise, the folder with its harmonized inputs (read only)
# output_path: where your runs are saved. Each run creates its own folder,
#              <output_path>/<exercise>/<date_time>_<nsim1>x<nsim2>/
# stata_exe  : optional, only if run_prep = TRUE and Stata is not found automatically

user <- tolower(Sys.info()[["user"]])

#Diego
if (user == "wb587758") {
  input_path  <- c(fy2022_backcast = "C:/Users/wb587758/OneDrive - WBG/IND/S2S/2022-23")
  output_path <- "C:/Users/wb587758/OneDrive - WBG/IND/S2S/runs"
#Jaime
} else if (user == "wb553773") {
  input_path  <- c(fy2022_backcast = "C:/Users/wb553773/OneDrive - WBG/Stats Team/IND S2S imputation/Reproducibility package")
  output_path <- "C:/Users/wb553773/OneDrive - WBG/Stats Team/IND S2S imputation/runs"
#Anyone else: add a block like the ones above
} else {
  stop("No paths for user '", user, "': add your block in section 2 of RUN_S2S.R")
}

#===============================================================================
# 3. Parameters of each exercise
#===============================================================================
# Years are the first calendar year of each July-June round (2022 = 2022-23).
# Input files are relative to the exercise's input_path.

#---------------------------------------------------------------------------
# Common to all exercises
#---------------------------------------------------------------------------
n.a  = .8                                     # resampling share, stages 1 and 2
seed = 1729                                   # seed for reproducibility
X.mtc1    = c("ymatch","hh_size","hh_head_age")  # stage 1: nearest neighbor search variables
don.vars1 = c("mpce_sp_def_ind")                 # stage 1: variable to be imputed
X.mtc2    = c("ymatch","hh_size","hhb_year")     # stage 2: nearest neighbor search variables
don.vars2 = c("ratio")                           # stage 2: variable to be imputed
use_stat  = "median"                          # stage 2 ensemble: mean, median or geometric_mean
use_mod   = "match"                           # "match" is PMM-style, "pred" is MI-style
lic  = 3.0                                    # international poverty lines, 2021 PPP
lmic = 4.2
umic = 8.3

#---------------------------------------------------------------------------
# fy2022_backcast: HCES 2022-23 anchor, backward to PLFS 2017-18 ... 2021-22
# Reproduces the July 2025 note (Fernandez and Olivieri)
#---------------------------------------------------------------------------
if (exercise == "fy2022_backcast") {
  anchor_year  <- 2022             # stage 1: HCES and PLFS round used to impute consumption
  stage2_years <- 2017:2021        # stage 2: PLFS rounds that receive the imputed ratio
  train_year   <- stage2_years[1]  # stage 2 models are trained once, on this round, and reused

  hces_file    <- "Data/Stage 1/Cleaned/HCES22_s2s.dta"                                   # HCES donor, stage 1
  plfs_s1_file <- "Data/Stage 1/Cleaned/IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.dta"  # PLFS receiver, stage 1
  plfs_s2_file <- "Data/Stage 2/Cleaned/IND_%d_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta"    # PLFS rounds, stage 2 (%d = year)
  states_file  <- "Data/Stage 1/Cleaned/states.xlsx"                                     # state names

  nsim1 = 1000                     # simulations, stage 1
  nsim2 = 100                      # simulations, stage 2

  cpi21 = 1.101906                 # 2022 prices to 2021 PPP
  icp21 = 19.46895
}

#---------------------------------------------------------------------------
# fy2023 and cy2023_forward: to come
#---------------------------------------------------------------------------

if (!exists("anchor_year")) stop("No parameters for exercise '", exercise, "' in section 3")

#===============================================================================
# 4. Run (nothing to edit below this line)
#===============================================================================

# Find the repo folder (works with Source in RStudio and with Rscript)
find_repo <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) return(dirname(normalizePath(f[1], winslash = "/")))
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }
  if (file.exists("RUN_S2S.R")) return(normalizePath(getwd(), winslash = "/"))
  stop("Could not find the repo folder: open RUN_S2S.R in RStudio and click Source")
}
path <- find_repo()
code <- function(f) source(file.path(path, "Code", f), chdir = TRUE, encoding = "UTF-8")

#Setup: run folder, logs, packages, data preparation (if run_prep)
code("01_setup.R")

if (run_s2s) {
  #Stage 1: HCES -> PLFS (anchor year)
  code("02_stage1_clean.R")
  code("03_stage1_simulation.R")
  code("04_stage1_ensemble.R")
  code("05_stage1_outputs.R")
  #Stage 2: PLFS anchor year -> other PLFS rounds
  code("06_stage2_clean.R")
  code("07_stage2_simulation.R")
  code("08_stage2_ensemble.R")
  code("09_stage2_outputs.R")
}

#Close the logs
cat(paste("finished:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "",
    capture.output(sessionInfo()), file = run_log, sep = "\n", append = TRUE)
sink()
close(console_log)
message("Done. Results in: ", run_path)
