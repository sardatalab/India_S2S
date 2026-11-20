### Evolution of Post-2011 Poverty in India: A Survey-to-Survey Imputation Approach
### Reproducibility Package
### Author: Jaime Fernandez Romero (jfernandezromero@worldbank.org)

### Main R Script
### 1. Open India_S2S.Rproj (so the working directory is the repo folder)
### 2. First time only: add your block of paths in config/paths.R
### 3. Choose the exercise below and run this script
### Parameters of each exercise (years, files, prices, simulations) are in config/<exercise>.R

#clear all
rm(list=ls())

#Exercise to run: one file per exercise in config/
exercise <- "fy2022_backcast"   # FY 2022-23 anchor, backward to 2017-2021 (July 2025 note)

#Packages: renv::restore() is not called here, because it can stop at an interactive
#menu and needs access to the package repository in renv.lock. To use the exact package
#versions of the note, run renv::restore() once in the console (see README).
# Check intallation of required packages
packages <- c(
  "StatMatch", "survey", "questionr", "reldist", "glmnet", "useful",
  "data.table", "haven", "statar", "parallel", "foreach", "doParallel",
  "dplyr", "tidyr", "dineq", "convey", "renv", "transport", "ggridges",
  "ggplot2","forcats","scales","readxl","Hmisc","viridis","ggh4x","patchwork",
  "GGally"
)

# CRAN mirror (optional but recommended)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to check for installed packages
is_installed <- function(pkg) {
  suppressWarnings(requireNamespace(pkg, quietly = TRUE))
}

missing_pkgs <- packages[!vapply(packages, is_installed, logical(1))]

if (length(missing_pkgs)) {
  install.packages(missing_pkgs)
} else {
  message("All packages already installed.")
}


# Load all packages
lapply(packages, require, character.only = TRUE)

# Set paths
if (!file.exists("config/paths.R")) {
  stop("Open India_S2S.Rproj first, so that the working directory is the repo folder")
}
path <- normalizePath(getwd(), winslash = "/")   # the repo folder
source(file.path(path, "config/paths.R"))
if (!exercise %in% names(datapaths)) {
  stop("No data folder for exercise '", exercise, "': add it to your block in config/paths.R")
}
datapath <- datapaths[[exercise]]

# Set the parameters of the exercise
source(file.path(path, "config", paste0(exercise, ".R")))

# Create the output folders if missing (git does not keep empty folders)
for (d in c(file.path(path, c("Outputs/Main/Figures", "Outputs/Main/Tables",
                              "Outputs/Annex/Figures", "Outputs/Annex/Tables",
                              "Outputs/Intermediate/Models")),
            file.path(datapath, c("Outputs/Intermediate",
                                  "Data/Stage 1/Final", "Data/Stage 2/Final")))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Run the R scripts

#Stage 1
source(file.path(path, "Code/00-Stage 1-Clean.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/01-Stage 1-Simulation.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/02-Stage 1-Ensemble.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/03-Stage 1-Outputs.R"),chdir = TRUE, encoding = "UTF-8")
#Stage 2
source(file.path(path, "Code/04-Stage 2-Simulation.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/05-Stage 2-Ensemble.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/06-Stage 2-Outputs.R"),chdir = TRUE, encoding = "UTF-8")
