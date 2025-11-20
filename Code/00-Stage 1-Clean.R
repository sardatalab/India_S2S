# Data cleaning and construction

#####Prepare receiver survey##### 
data.rec=read_dta(paste(datapath,"cleaned/lfs2019_clean.dta",sep="")) 
#create sequential IDs
data.rec$hidseq=seq(1:nrow(data.rec))

#Additional clean and construction
#create population weights
#data.rec$pop_wgt=with(data.rec,hh_size*hhwt)
#hh age squared
data.rec$hh_head_age_sq = with(data.rec,age_hhh^2)

# Missing values report
missing_report.rec <- data.rec %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)
#based on report, these variables are excluded only when using linear models
data.rec=subset(data.rec,sel=-c(num_agri_emp,
                                num_indexcons_emp,
                                num_cons_emp,
                                num_ind_emp,
                                num_serv_emp,
                                num_public_emp,
                                num_pvt_emp,
                                num_family_worker,
                                num_employer,
                                num_self_emp))

data.rec=na.omit(data.rec)

#####Prepare donor survey#####
data.don=read_dta(paste(datapath,"cleaned/hies2019_clean.dta",sep="")) 
#create sequential Ids
data.don$hidseq=seq(1:nrow(data.don))

#Additional clean and construction
#Regroup state dummies
#data.don$state <- max.col(data.don[, state_vars])
#correct state variable for state 26
#data.don$state = ifelse(data.don$state<=25,data.don$state,data.don$state+1)
#data.don$statenum = data.don$state
#data.don$state = as.factor(data.don$state)
#Regroup hh type
#data.don$hh_type <- as.factor(max.col(data.don[, type_vars]))
#Drop dummies for state and hhtype, remove rows with NAs in consumption
data.don <- data.don %>% 
    filter(!is.na(welfare))
#hh age squared
data.don$hh_head_age_sq = with(data.don,age_hhh^2)
# Missing values report
missing_report.don <- data.don %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)
#data.don=subset(data.don,sel=-c(sex_ratio))

