### Stage 2, cleaning
### 1. Donor: the PLFS of the anchor year, with the consumption imputed in Stage 1 and the
###    ratio of imputed to abbreviated consumption (the variable imputed in Stage 2)
### 2. prep_plfs_s2(): variables of the Stage 2 models, the same for the donor and for the
###    receivers (the other PLFS rounds, prepared in 07_stage2_simulation.R)

#===============================================================================
# Variables of the Stage 2 models
#===============================================================================
#ref_year: survey year of the data (used for the household head's year of birth)
prep_plfs_s2 <- function(df, ref_year) {
  df$hh_type <- as.factor(df$hh_type)
  df$state <- as.factor(df$state)
  df$hhb_year=with(df,ref_year-hh_head_age)
  df$hh_head_age_sq = with(df,hh_head_age^2)
  df$hh_size_sq = with(df,hh_size^2)
  df$log_labor_pc_adj=log(df$total_labor_pc_adj+1)
  df$log_consumption_pc_adj=log(df$consumption_pc_adj+1)
  #Drop dummies for state and hhtype
  df <- df %>% 
    mutate(across(
      c(hh_sh_job_contract, hh_sh_paidleave, hh_sh_pension, 
        hh_sh_gratuity, hh_sh_health),
      ~ if_else(flag5_all_lab == 1 & is.na(.x), 0, .x)
    )) %>%
    select(-starts_with("state_"))  %>% 
    select(-starts_with("hhtype_")) %>%
    select(-starts_with("hh_sp_")) %>%
    select(-starts_with("hh_size_age")) %>%
    select(-c(hh_sex_ratio,flag2_sex,hh_dep_ratio,flag1_depen,
              hh_sh1564_lit,flag3_spouse,
              hh_head_job_contract,hh_head_paidleave,
              hh_head_pension,hh_head_gratuity,
              hh_head_health, flag4_head_lab,
              hh_sh_age0,hh_sh_age1,hh_sh_age2,hh_sh_age3,
              hh_sh_age4,flag5_all_lab,hh_head_ilit,hh_sh_ilit,
              hh_sh_female))
  df
}

#===============================================================================
# Donor: PLFS anchor year with the imputed consumption and the ratio
#===============================================================================
#plfs.don is here the harmonized PLFS 2022 with all common variables between the
#different rounds of the PLFS to be used in stage 2
plfs.don=read_dta(file.path(input_path, sprintf(plfs_s2_file, anchor_year)))
#We bring the dataset containing the imputed consumption
plfs.imp=read_dta(paste(run_path,
          "/data/Imputed_PLFS_22_match.dta",sep=""))
plfs.imp=subset(plfs.imp,select=c(hhid,mpce_sp_def_ind))
plfs.don=merge(plfs.don,plfs.imp,by="hhid",all.x=TRUE)
rm(plfs.imp)
plfs.don=subset(plfs.don,!is.na(mpce_sp_def_ind))
#The ratio is calculated by dividing the imputed consumption by the
#previously deflated and temporaly adjusted 
#abbreviated consumption available in the PLFS.
plfs.don$ratio = with(plfs.don,
                      mpce_sp_def_ind/consumption_pc_adj)

#Variables also created by the figures of the ratio (kept here so that the donor data
#are exactly the ones of the original code)
plfs.don$quintile=xtile(plfs.don$mpce_sp_def_ind,n=5,wt=plfs.don$weight)
plfs.don$quintile=as.factor(plfs.don$quintile)
plfs.don$pop_wgt=with(plfs.don,hh_size * weight)
plfs.don$decile_mmrp=xtile(plfs.don$mpce_sp_def_ind,n=10,wt=plfs.don$pop_wgt)
plfs.don$decile_abbr=xtile(plfs.don$consumption_pc_adj,n=10,wt=plfs.don$pop_wgt)

#Additional data cleaning & construction

#create sequential Ids
plfs.don$hidseq=seq(1:nrow(plfs.don))
#Create quintiles of abbreviated consumption
#plfs.don$qtil=xtile(plfs.don$consumption_pc_adj,n=5,wt=plfs.don$weight)
plfs.don=prep_plfs_s2(plfs.don, anchor_year)
# Missing values report
missing_report.don <- plfs.don %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)
