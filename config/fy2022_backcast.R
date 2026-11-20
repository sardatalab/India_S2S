### Exercise: FY 2022-23 anchor, backward to PLFS 2017-18 ... 2021-22
### Reproduces the July 2025 note (Fernandez and Olivieri).
###
### Years are the first calendar year of each July-June round (2022 = 2022-23).

#Years
anchor_year  <- 2022             # Stage 1: HCES and PLFS round used to impute consumption
stage2_years <- 2017:2021        # Stage 2: PLFS rounds that receive the imputed ratio
train_year   <- stage2_years[1]  # Stage 2 models are trained once, on this round, and reused for the others

#Input files, relative to the exercise's data folder (datapaths in config/paths.R)
hces_file    <- "Data/Stage 1/Cleaned/HCES22_s2s.dta"                                   # HCES donor, Stage 1
plfs_s1_file <- "Data/Stage 1/Cleaned/IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.dta"  # PLFS receiver, Stage 1
plfs_s2_file <- "Data/Stage 2/Cleaned/IND_%d_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta"    # PLFS rounds, Stage 2 (%d = year)
states_file  <- "Data/Stage 1/Cleaned/states.xlsx"                                     # state names

#Number of simulations stage 1
nsim1=1000

#Number of simulations stage 2
nsim2=100

#Resampling parameter stages 1 and 2
n.a = .8

#Seed for reproducibility
seed = 1729

#Matching parameters stage 1
X.mtc1=c("ymatch","hh_size","hh_head_age") # nearest neighbor search variables
don.vars1=c("mpce_sp_def_ind") #variables to be imputed

#Matching parameters stage 2
X.mtc2=c("ymatch","hh_size","hhb_year") # nearest neighbor search variables
don.vars2=c("ratio") #variables to be imputed

#Statistic to be used to ensemble simulations in stage 2
use_stat="median" #alternatively: mean, median geometric_mean

#Type of model: "match" is PMM-style and "pred" is MI-style
use_mod="match"

#Parameters to convert vectors in 2022 prices to 2021 PPP
cpi21=1.101906
icp21=19.46895

#International poverty lines in 2021 PPP
lic=3.0
lmic=4.2
umic=8.3
