### Evolution of Post-2011 Poverty in India: A Survey-to-Survey Imputation Approach
### Reproducibility Package
### Author: Jaime Fernandez Romero (jfernandezromero@worldbank.org)
###
### Setup of a run, called by RUN_S2S.R (do not run this file directly):
### checks the paths, creates the run folder and its logs, runs the data preparation
### (if run_prep) and loads the packages (if run_s2s).

if (!exists("path") || !exists("exercise") || !exists("s2s_root")) {
  stop("Run RUN_S2S.R (in the repo folder), not the files in Code/")
}
#Folders of the S2S data root (section 2 of RUN_S2S.R)
inputs_path     <- file.path(s2s_root, "inputs")                          # external inputs
harmonized_path <- file.path(s2s_root, "harmonized", harmonized_folder)   # harmonized files of this exercise
output_path     <- file.path(s2s_root, "output")                          # runs
for (d in c(s2s_root, inputs_path, harmonized_path)) {
  if (!dir.exists(d)) stop("Folder not found: ", d, " (check s2s_root in section 2 of RUN_S2S.R)")
}

#===============================================================================
# Run folder: <output_path>/<exercise>/<date_time>_<nsim1>x<nsim2>/
#===============================================================================
run_name <- paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_", nsim1, "x", nsim2)
run_path <- file.path(output_path, exercise, run_name)
for (d in c("data", "models", "tables", "figures", "logs")) {
  dir.create(file.path(run_path, d), recursive = TRUE, showWarnings = FALSE)
}
#Copy of the master file, with all the settings of this run
file.copy(file.path(path, "RUN_S2S.R"), file.path(run_path, "RUN_S2S_used.R"))

# Console output goes to the screen and to logs/console.log
while (sink.number() > 0) sink()   # in case a previous run stopped with an error
console_log <- file(file.path(run_path, "logs", "console.log"), open = "wt")
sink(console_log, split = TRUE)

# Run log: what was run, from which code, on which inputs
git_commit <- tryCatch({
  git_head <- readLines(file.path(path, ".git/HEAD"))
  if (startsWith(git_head, "ref: ")) {
    ref <- sub("ref: ", "", git_head)
    paste0(sub("refs/heads/", "", ref), " @ ", readLines(file.path(path, ".git", ref)))
  } else git_head
}, error = function(e) "unknown")
run_log <- file.path(run_path, "run_log.txt")
writeLines(c(
  paste("exercise:   ", exercise),
  paste("started:    ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("user:       ", user),
  paste("code:       ", git_commit),
  paste("harmonized: ", harmonized_path),
  paste("inputs:     ", inputs_path),
  paste("run_prep:   ", run_prep, "  run_s2s:", run_s2s),
  paste("simulations:", nsim1, "(stage 1),", nsim2, "(stage 2)"),
  paste("seed:       ", seed),
  paste("R:          ", R.version.string)
), run_log)
message("Run folder: ", run_path)

#===============================================================================
# A. Data preparation (Stata, batch mode)
#===============================================================================
if (run_prep) {
  prep_master <- file.path(path, "prep", "00_master.do")
  if (!file.exists(prep_master)) {
    stop("run_prep = TRUE, but the Stata data preparation is not in the repo yet (prep/00_master.do)")
  }
  find_stata <- function() {
    if (exists("stata_exe")) return(stata_exe)
    hits <- Sys.glob(file.path(c("C:/Program Files", "C:/Program Files (x86)"), "Stata*",
                               c("StataMP-64.exe", "StataSE-64.exe", "StataBE-64.exe", "Stata-64.exe")))
    if (!length(hits)) stop("Stata not found: set stata_exe in your block of RUN_S2S.R")
    sort(hits, decreasing = TRUE)[1]
  }
  stata <- find_stata()
  message("Running the data preparation in Stata: ", stata)
  old_wd <- setwd(file.path(run_path, "logs"))   # Stata writes 00_master.log here
  # arguments of 00_master.do: exercise, s2s_root, repo path (its own "test" switch decides
  # whether it writes to harmonized/<FOLDER>_test or harmonized/<FOLDER>)
  system2(stata, c("/e", "do", shQuote(prep_master), shQuote(exercise), shQuote(s2s_root), shQuote(path)))
  setwd(old_wd)
  stata_log <- file.path(run_path, "logs", "00_master.log")
  if (!file.exists(stata_log) || any(grepl("^r\\([0-9]+\\);", readLines(stata_log, warn = FALSE)))) {
    stop("The Stata data preparation failed: see ", stata_log)
  }
}

#===============================================================================
# B. Packages for the S2S imputation (R)
#===============================================================================
if (run_s2s) {

  # Packages: missing ones are installed from CRAN, without questions
  packages <- c(
    "StatMatch", "survey", "questionr", "reldist", "glmnet", "useful",
    "data.table", "haven", "statar", "parallel", "foreach", "doParallel",
    "dplyr", "tidyr", "dineq", "convey", "transport", "ggridges",
    "ggplot2","forcats","scales","readxl","Hmisc","viridis","ggh4x","patchwork",
    "GGally"
  )
  options(repos = c(CRAN = "https://cloud.r-project.org"),
          install.packages.compile.from.source = "never")
  missing_pkgs <- packages[!vapply(packages, function(p)
    suppressWarnings(requireNamespace(p, quietly = TRUE)), logical(1))]
  if (length(missing_pkgs)) {
    lib <- Sys.getenv("R_LIBS_USER")
    if (!dir.exists(lib)) dir.create(lib, recursive = TRUE)
    .libPaths(c(lib, .libPaths()))
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, lib = lib)
  }
  invisible(lapply(packages, library, character.only = TRUE))

  # Package versions compared with the ones of the July 2025 note (renv.lock)
  if (requireNamespace("jsonlite", quietly = TRUE) && file.exists(file.path(path, "renv.lock"))) {
    lock <- jsonlite::fromJSON(file.path(path, "renv.lock"))$Packages
    key <- c("StatMatch", "glmnet", "survey", "transport", "data.table", "dplyr")
    diffs <- vapply(key, function(p) {
      now <- as.character(packageVersion(p)); was <- lock[[p]]$Version
      if (!is.null(was) && now != was) paste0(p, " ", now, " (note: ", was, ")") else ""
    }, character(1))
    diffs <- diffs[nzchar(diffs)]
    if (length(diffs)) {
      msg <- paste("Package versions differ from the note:", paste(diffs, collapse = "; "))
      message(msg)
      cat(paste("packages:   ", msg), file = run_log, sep = "\n", append = TRUE)
    }
  }
}
