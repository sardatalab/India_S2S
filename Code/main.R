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
path <- "C:/Users/wb562318/Github"
datapath <- "C:/Users/wb562318/OneDrive - WBG/Documents/POV-SAR/SL/PA/Analysis/Data/"

# Set global parameters

# Number of simulations stage 1
nsim1=10

# Number of simulations stage 2
nsim2=10

# Resampling parameter stages 1 and 2
n.a = .9

# Seed for reproducibility
seed = 1729

# Matching parameters stage 1
X.mtc1=c("ymatch","hhsize","age_hhh") # nearest neighbor search variables
don.vars1=c("welfare") #variables to be imputed //update to rcpccons 

# Matching parameters stage 2
X.mtc2=c("ymatch","hhsize","age_hhh") # nearest neighbor search variables
don.vars2=c("ratio","welfare") #variables to be imputed

# Parameters to convert vectors in 2019 prices to 2021 PPP
cpi21=0.88027848 #this is to convert to 2021PPPs
icp21=58.296108 #set up as in GMD

# Run the R scripts
#Stage 1
source(file.path(path, "India_S2S/Code/00-Stage 1-Clean.R"))
source(file.path(path, "India_S2S/Code/01-Stage 1-Simulation.R"))
source(file.path(path, "India_S2S/Code/02-Stage 1-Ensemble.R"))
source(file.path(path, "India_S2S/Code/03-Stage 1-Outputs.R"))

source(file.path(path, "India_S2S/Code/04-Stage 2-Simulation"))


