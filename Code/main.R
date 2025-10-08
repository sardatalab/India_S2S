### Evolution of Post-2011 Poverty in India: A Survey-to-Survey Imputation Approach
### Reproducibility Package
### This version: July 29, 2029
### Author: Jaime Fernandez Romero (jfernandezromero@worldbank.org)

### Main R Script

#clear all
rm(list=ls())

#renv::init()

# Check intallation of required packages
packages <- c(
  "StatMatch", "survey", "questionr", "reldist", "glmnet", "useful",
  "data.table", "haven", "statar", "parallel", "foreach", "doParallel",
  "dplyr", "tidyr", "dineq", "convey", "renv", "transport", "ggridges",
  "ggplot2","forcats","scales","readxl"
)

# Install missing packages
installed <- installed.packages()
for (pkg in packages) {
  if (!(pkg %in% rownames(installed))) {
    install.packages(pkg, dependencies = TRUE)
  }
}

#renv::restore()

# Load all packages
lapply(packages, require, character.only = TRUE)

# Set paths
path <- "C:/Users/wb553773/GitHub/India_S2S"
datapath <- "C:/Users/wb553773/WBG/Nishtha Kochhar - INDDATA/S2S imputations_CES_LFS/Reproducibility package"

# Set global parameters

# Number of simulations stage 1
nsim1=10

# Number of simulations stage 2
nsim2=10

# Resampling parameter stages 1 and 2
n.a = .8

# Seed for reproducibility
seed = 1729

# Matching parameters stage 1
X.mtc1=c("ymatch","hh_size","hh_head_age") # nearest neighbor search variables
don.vars1=c("mpce_sp_def_ind") #variables to be imputed

# Matching parameters stage 2
X.mtc2=c("ymatch","hh_size","hh_head_age") # nearest neighbor search variables
don.vars2=c("mpce_sp_def_ind") #variables to be imputed

# Parameters to convert vectors in 2022 prices to 2021 PPP
cpi21=1.101906
icp21=19.46895

# International poverty lines in 2021 PPP
lic=3.0
lmic=4.2
umic=8.3

# Run the R scripts

#Stage 1
source(file.path(path, "Code/00-Stage 1-Clean.R"))
source(file.path(path, "Code/01-Stage 1-Simulation.R"))
source(file.path(path, "Code/02-Stage 1-Ensemble.R"))
source(file.path(path, "Code/03-Stage 1-Outputs.R"))


