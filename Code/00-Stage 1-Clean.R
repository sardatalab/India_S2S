# Data cleaning and construction

#####Prepare receiver survey##### 
data.rec=read_dta(paste(datapath,
  "/Data/Stage 1/Cleaned/IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.dta",sep="")) 
#create sequential IDs
data.rec$hidseq=seq(1:nrow(data.rec))

#Additional clean and construction
#create population weights
data.rec$pop_wgt=with(data.rec,hh_size*hhwt)
#hh age squared
data.rec$hh_head_age_sq = with(data.rec,hh_head_age^2)
#Regroup state dummies
state_vars <- paste0("state_", 1:37)
state_vars = state_vars[-26] # State 26 does not exist
data.rec$state <- max.col(data.rec[, state_vars])
#correct state variable for state 26
data.rec$state = ifelse(data.rec$state<=25,data.rec$state,data.rec$state+1)
data.rec$statenum = data.rec$state
data.rec$state = as.factor(data.rec$state)
#Regroup hhtype
type_vars <- paste0("hhtype_", 1:8)
data.rec$hh_type <- as.factor(max.col(data.rec[, type_vars]))
#Drop dummies for state and hhtype
data.rec <- data.rec %>% 
    select(-starts_with("state_"))  %>% 
    select(-starts_with("hhtype_"))
######

# Missing values report
missing_report.rec <- data.rec %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)
#based on report, these variables are excluded only when using linear models
data.rec=subset(data.rec,sel=-c(hh_dep_ratio,hh_sex_ratio,
                                hh_sh1564_lit,hh_sp_educ))

data.rec=na.omit(data.rec)

#####Prepare donor survey#####

data.don=read_dta(paste(datapath,
                        "/Data/Stage 1/Cleaned/HCES22_s2s.dta",sep="")) 
#create sequential Ids
data.don$hidseq=seq(1:nrow(data.don))

#Additional clean and construction
#Regroup state dummies
data.don$state <- max.col(data.don[, state_vars])
#correct state variable for state 26
data.don$state = ifelse(data.don$state<=25,data.don$state,data.don$state+1)
data.don$statenum = data.don$state
data.don$state = as.factor(data.don$state)
#Regroup hh type
data.don$hh_type <- as.factor(max.col(data.don[, type_vars]))
#Drop dummies for state and hhtype, remove rows with NAs in consumption
data.don <- data.don %>% 
    select(-starts_with("state_"))  %>% 
    select(-starts_with("hhtype_")) %>%
    filter(!is.na(mpce_sp_def_ind))
#hh age squared
data.don$hh_head_age_sq = with(data.don,hh_head_age^2)
# Missing values report
missing_report.don <- data.don %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)

