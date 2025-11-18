### Evolution of Post-2011 Poverty in India: A Survey-to-Survey Imputation Approach
### Reproducibility Package
### This version: July 29, 2029
### Author: Jaime Fernandez Romero (jfernandezromero@worldbank.org)

### Main R Script

#clear all
rm(list=ls())

#renv::init()
#renv::restore()
# Check intallation of required packages
packages <- c(
  "StatMatch", "survey", "questionr", "reldist", "glmnet", "useful",
  "data.table", "haven", "statar", "parallel", "foreach", "doParallel",
  "dplyr", "tidyr", "dineq", "convey", "renv", "transport", "ggridges",
  "ggplot2","forcats","scales","readxl","Hmisc","viridis","ggh4x","patchwork",
  "GGally","xgboost","matrixStats"
)

# Install missing packages
installed <- installed.packages()
for (pkg in packages) {
  if (!(pkg %in% rownames(installed))) {
    install.packages(pkg)
  }
}

# Load all packages
lapply(packages, require, character.only = TRUE)

# Set paths
path <- "C:/Users/wb553773/GitHub/India_S2S"
datapath <- "C:/Users/wb553773/OneDrive - WBG/Stats Team/IND S2S imputation/Reproducibility package"

# Set global parameters

# Number of simulations stage 1
nsim1=1000

# Number of simulations stage 2
nsim2=100

# Resampling parameter stages 1 and 2
n.a = .8

# Third stage parameter to complete NAs after second stage
# imputation (stage3==1). Default: stage3==0
stage3==0

# Seed for reproducibility
seed = 1729

# Matching parameters stage 1
X.mtc1=c("ymatch","hh_size","hh_head_age") # nearest neighbor search variables
don.vars1=c("mpce_sp_def_ind") #variables to be imputed

# Matching parameters stage 2
X.mtc2=c("ymatch","hh_size","hhb_year") # nearest neighbor search variables
don.vars2=c("ratio") #variables to be imputed

# Matching parameters stage 3 (if stage3==1)
X.mtc3=c("ymatch","hh_size","hh_head_age") # nearest neighbor search variables
don.vars3=c("ratio") #variables to be imputed

# Statistic to be used to ensemble simulations in stage 2
use_stat="median" #alternatively: mean, median geometric_mean

# Type of model: "match" is PMM-style and "pred" is MI-style
use_mod="match"  

# Parameters to convert vectors in 2022 prices to 2021 PPP
cpi21=1.101906
icp21=19.46895

# International poverty lines in 2021 PPP
lic=3.0
lmic=4.2
umic=8.3

# Run R scripts

#Stage 1
source(file.path(path, "Code/00-Stage 1-Clean.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/01-Stage 1-Simulation.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/02-Stage 1-Ensemble.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/03-Stage 1-Outputs.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/04-Stage 2-Simulation.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/05-Stage 2-Ensemble.R"),chdir = TRUE, encoding = "UTF-8")
source(file.path(path, "Code/06-Stage 2-Outputs.R"),chdir = TRUE, encoding = "UTF-8")

