
######
#Poverty rates resulting from imputations 
######

####Figure 13 A####

povrates <- tab_export %>%
    select(c(povlic, povlmic, povumic,year,sect)) %>%
    pivot_longer(
    cols = c(povlic, povlmic, povumic),
    names_to = "line",
    values_to = "value"
  ) %>%
    pivot_wider(
    names_from = sect,
    values_from = value
  ) %>%
  mutate(line = sub("^pov", "", line))


ggplot(povrates, aes(x = year, y = national * 100, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", national * 100)), 
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "National Poverty",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0, 100)) +
  theme_minimal()

ggsave(paste(path,
      "/Outputs/Main/Figures/Figure 13 A.png",sep=""),
       width = 20, height = 12, units = "cm")


####Figure 13 B####

ginis <- tab_export %>%
  select(c(gini_cons_imp,gini_cons_abb,year,sect)) %>%
  rename(`Imputed/Official`=gini_cons_imp , Abbreviated=gini_cons_abb) %>%
  pivot_longer(
    cols = c(`Imputed/Official`,Abbreviated),
    names_to = "Aggregate",
    values_to = "value"
  ) 
  

ggplot(ginis[ginis$sect=="national",], 
       aes(x = year, y = value * 100, color = Aggregate)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", value * 100)), 
            vjust = -0.3, size = 5, show.legend = FALSE) +
  labs(title = "",
       x = "Year", y = "Gini Coefficient") +
    theme_minimal()

ggsave(paste(path,
             "/Outputs/Main/Figures/Figure 13 B.png",sep=""),
       width = 20, height = 12, units = "cm")

####Figure 14####

# Rural
p2 <- ggplot(povrates, aes(x = year, y = rural*100, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", rural*100)), 
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "Rural Poverty",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0,100))+
  theme_minimal()+theme(legend.position = "none")

# Urban
p3 <- ggplot(povrates, aes(x = year, y = urban*100, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", urban*100)), 
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "Urban Poverty",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0,100))+
  theme_minimal()

# Side by side
p2 + p3

ggsave(paste(path,
          "/Outputs/Main/Figures/Figure 14.png",sep=""),
       width = 25, height = 15, units = "cm")


########
#Poverty rates using PLFS only
########

#load HCES
hces=read_dta(paste(datapath,
              "/Data/Stage 1/Cleaned/HCES22_s2s.dta",sep="")) 
# state_vars <- paste0("state_", 1:37)
# state_vars = state_vars[-26] # State 26 does not exist
# #Regroup state dummies
# hces$state <- max.col(hces[, state_vars])
# #correct state variable for state 26
# hces$state = ifelse(hces$state<=25,hces$state,hces$state+1)

hces <- hces %>%
  select(urb, mpce_sp_def_ind, pop_wgt) %>%
  mutate(survey = "HCES")

df=na.omit(hces)

df$pov30 = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<3,1,0)
df$pov42 = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<4.2,1,0)
df$pov83 = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<8.3,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~pop_wgt)
#Overall Poverty
tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE)
tab1

#Poverty by sector
tab2=svyby(~pov30+pov42+pov83, ~urb, design=svydf, 
           svymean,na.rm=TRUE)
tab2$urb=factor(tab2$urb, levels=c(0,1),labels=c("Rural","Urban"))
tab2 = tab2 %>% rename(Sector=urb)
tab2

#load PLFS 22
plfs=read_dta(paste(datapath,
       "/Data/Stage 2/Cleaned/IND_2022_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",
       sep=""))
plfs.imp=read_dta(paste(datapath,
              "/Data/Stage 1/Final/Imputed_PLFS_22_match.dta",sep=""))
plfs.imp=subset(plfs.imp,select=c(hhid,mpce_sp_def_ind))
plfs=merge(plfs,plfs.imp,by="hhid",all.x=TRUE)
rm(plfs.imp)
plfs=subset(plfs,!is.na(mpce_sp_def_ind))

plfs$log_mpce=with(plfs,log(mpce_sp_def_ind))
plfs$log_abbr=with(plfs,log(consumption_pc_adj))

####Figure B 3####

#correlation plot between the two log aggregates in plfs 2022
ggpairs(plfs[, c("log_mpce", "log_abbr")])
ggsave(paste(path,
             "/Outputs/Annex/Figures/Figure B 3.png",sep=""),
       width = 20, height = 20, units = "cm")

#Abbreviated consumption in 2021 PPP
plfs$abbr_cons_ppp=with(plfs,consumption_pc_adj*(12/365)/cpi21/icp21)

plfs <- plfs %>%
  mutate(pop_wgt = hh_size*weight) %>%
  select(urban, abbr_cons_ppp, pop_wgt) %>%
  mutate(survey = "PLFS")

df=na.omit(plfs)
df$pov30_ab = ifelse(df$abbr_cons_ppp<3,1,0)
df$pov42_ab = ifelse(df$abbr_cons_ppp<4.2,1,0)
df$pov83_ab = ifelse(df$abbr_cons_ppp<8.3,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~pop_wgt)

#Overall Poverty using abbreviated consumption only
tab3=svyby(~pov30_ab+pov42_ab+pov83_ab, ~survey, design=svydf, svymean,
           na.rm=TRUE)
tab3

#equivalent international poverty lines to replicate HCES rates
lines <- svyquantile(~abbr_cons_ppp, 
                           svydf, 
                           quantiles = as.numeric(tab1[2:4])) 
#endogenous lines to replicate HCES headcounts
q_vals <- lines$abbr_cons_ppp[, "quantile"]

cat("Endogenous poverty lines are: ",q_vals," in PPP$ (lic, lmic, and umic, respectively)")

######
#Poverty trend in PLFS 2017-2022#
#####

years=c(2017:2022)

cl <- makeCluster(length(years))
registerDoParallel(cl)

pov_results <- foreach (year = years, .combine = "rbind",
 .packages = c("survey", "haven","dplyr","tidyr","dineq"),
 .export   = c("q_vals","cpi21","icp21","lic","lmic","umic")) %dopar% {
                          
  #Load PLFS data
  data.rec=read_dta(paste(datapath,
      "/Data/Stage 2/Cleaned/IND_",year,"_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",
      sep=""))
  data.rec$pop_wgt = with(data.rec,weight*hh_size)
  data.rec = subset(data.rec,!is.na(consumption_pc_adj) & !is.na(pop_wgt))
  #poverty with intl lines
  data.rec$povlic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lic,1,0)
  data.rec$povlmic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lmic,1,0)
  data.rec$povumic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<umic,1,0)
  
  #poverty with equivalent lines
  lic_end=q_vals[1] #lic equivalent line
  lmic_end=q_vals[2] #lmic equivalent line
  umic_end=q_vals[3] #umic equivalent line
  data.rec$povlic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lic_end,1,0)
  data.rec$povlmic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lmic_end,1,0)
  data.rec$povumic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<umic_end,1,0)
  
  svydf <- svydesign(ids = ~hhid, data = data.rec, strata= ~strata, 
                                             weights = ~pop_wgt)
                          
  #Overall Poverty with intl lines
  tab1=data.frame(svymean(~povlic+povlmic+povumic,  design=svydf, 
                                       na.rm=TRUE))
  #Overall Poverty with endogenous lines
  tab2=data.frame(svymean(~povlic_end+povlmic_end+povumic_end,  design=svydf, 
               na.rm=TRUE))
  tab1$year=year
  data.frame(tab1,tab2)
}

stopCluster(cl)

pov_results$line <- factor(rep(c("pov_lic", "pov_lmic", "pov_umic"), times = 6))
pov_results = pov_results %>%
  rename(international = mean,
         endogenous = mean.1)

pov_results$international=100*pov_results$international
pov_results$endogenous=100*pov_results$endogenous

write.csv(pov_results,file=paste(path,
    "/Outputs/Intermediate/Poverty_trends_only_PLFS.csv",sep=""))



p1 <- ggplot(pov_results, aes(x = year, y = international, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", international)), 
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "International Lines",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0,100))+
  theme_minimal()+theme(legend.position = "none")

# Second plot: endogenous
p2 <- ggplot(pov_results, aes(x = year, y = endogenous, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", endogenous)), 
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "Endogenous Lines",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0,100))+
  theme_minimal()

# Side by side
p1 + p2

ggsave(paste(path,
   "/Outputs/Annex/Figures/Figure D 1.png",sep=""),
       width = 25, height = 15, units = "cm")


###########
#The same by urban and rural
###########

cl <- makeCluster(length(years))
registerDoParallel(cl)

pov_results_sec <- foreach (year = years, .combine = "rbind",
  .packages = c("survey", "haven","dplyr","tidyr","dineq"),
  .export   = c("q_vals","cpi21","icp21","lic","lmic","umic")) %dopar% {
                          
  #Load PLFS data
  data.rec=read_dta(paste(datapath,
       "/Data/Stage 2/Cleaned/IND_",year,"_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",
       sep=""))
  data.rec$pop_wgt = with(data.rec,weight*hh_size)
  data.rec = subset(data.rec,!is.na(consumption_pc_adj) & !is.na(pop_wgt))

  data.rec$povlic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lic,1,0)
  data.rec$povlmic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lmic,1,0)
  data.rec$povumic = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<umic,1,0)
                          
  #poverty with equivalent lines
  lic_end=q_vals[1] #lic equivalent line
  lmic_end=q_vals[2] #lmic equivalent line
  umic_end=q_vals[3] #umic equivalent line
  data.rec$povlic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lic_end,1,0)
  data.rec$povlmic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<lmic_end,1,0)
  data.rec$povumic_end = ifelse(data.rec$consumption_pc_adj*(12/365)/cpi/icp<umic_end,1,0)
                          
  svydf <- svydesign(ids = ~hhid, data = data.rec, strata= ~strata, 
                                             weights = ~pop_wgt)
                          
  #Poverty by sector with intl lines
  tab1=data.frame(svyby(~povlic+povlmic+povumic, ~urban, design=svydf, 
                                     svymean,na.rm=TRUE))
  tab1$urban=factor(tab1$urban, levels=c(0,1),labels=c("Rural","Urban"))
  tab1 = tab1 %>% rename(Sector=urban) %>%
                              pivot_longer(
                              cols = c(povlic, povlmic, povumic),  
                              names_to = "line",               
                              values_to = "povrate"              
                            ) %>%
                            select(c(Sector,line,povrate))
                          
  #Poverty by sector with endogenous lines
  tab2=data.frame(svyby(~povlic_end+povlmic_end+povumic_end, ~urban, design=svydf, 
                                                svymean,na.rm=TRUE))
  tab2$urban=factor(tab2$urban, levels=c(0,1),labels=c("Rural","Urban"))
  tab2 = tab2 %>% rename(Sector=urban) %>%
                            pivot_longer(
                              cols = c(povlic_end, povlmic_end, povumic_end),  
                              names_to = "line",               
                              values_to = "povrate"               
                            ) %>%
                            select(c(Sector,line,povrate))
  tab1$year=year
  data.frame(cbind(tab1,tab2))
}

stopCluster(cl)

pov_results_sec$line <- factor(rep(c("pov_lic", "pov_lmic", "pov_umic"), 
                                   times = 12))
pov_results_sec = pov_results_sec %>%
  rename(international = povrate,
         endogenous = povrate.1)

pov_results_sec$international=100*pov_results_sec$international
pov_results_sec$endogenous=100*pov_results_sec$endogenous

write.csv(pov_results,file=paste(path,
        "/Outputs/Intermediate/Poverty_trends_only_PLFS_sector.csv",sep=""))


#First plot: intl linea
p1 <- ggplot(pov_results_sec, aes(x = year, y = international, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", international)),
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "International Lines",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0, 100)) +
  facet_wrap(~Sector, ncol = 2) +   # facet horizontally by urban
  theme_minimal()+theme(legend.position = "none")

# Second plot: Endogenous
p2 <- ggplot(pov_results_sec, aes(x = year, y = endogenous, color = line)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", endogenous)),
            vjust = -0.5, size = 5, show.legend = FALSE) +
  labs(title = "Endogenous Lines",
       x = "Year", y = "Poverty rate (%)") +
  ylim(c(0, 100)) +
  facet_wrap(~Sector, ncol = 2) +   # facet horizontally by urban
  theme_minimal()

# Stack plots vertically
combined <- p1 / p2

# Save to file
ggsave(paste0(path,
              "/Outputs/Annex/Figures/Figure D 2.png"),
       combined, width = 25, height = 30, units = "cm")
