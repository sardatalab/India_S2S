
years=stage2_years
sim = nsim2 #number of simulations that were used
rm(df)

cl <- makeCluster(length(years))
registerDoParallel(cl)

pov_results <- foreach (year = years, .combine = "rbind",
.packages = c("survey", "haven","dplyr","tidyr","dineq")) %dopar% {
  
  #Load PLFS data
  plfs.rec=read_dta(file.path(harmonized_path, sprintf(plfs_s2_file, year)))
  #Load simulations results
  df=readRDS(file=paste(run_path,
            "/data/Simulations_model_",use_mod,"_",year,".rds",sep=""))
  #only for geometric_mean
  if (use_stat=="geometric_mean") {
    df[df <= 0] <- NA
    geometric_mean <- function(x, na.rm = TRUE) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      if(any(x <= 0)) {
        stop("All values must be positive to compute the geometric mean.")
      }
      exp(mean(log(x)))
    }
  }
  #Ensembles match
  df$mpce_sp_def_ind=apply(df[,2:(sim+1)],
                           1,use_stat,na.rm=TRUE)
  data.rec=merge(plfs.rec,df[,c("hhid","mpce_sp_def_ind")],
                 by="hhid",all.x = TRUE)
  data.rec$pop_wgt = with(data.rec,weight*hh_size)
  ##Enable when ready to save final results
  write_dta(data.rec,paste0(run_path,
        "/data/IND_",year,"_PLFS_imputed_ratio_",sim,
                "_",use_stat,".dta"))
  data.rec = subset(data.rec,!is.na(mpce_sp_def_ind))
  #prices and poverty lines come from section 3 of RUN_S2S.R (cpi21, icp21, lic, lmic, umic)
  data.rec$povlic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi21/icp21<lic,1,0)
  data.rec$povlmic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi21/icp21<lmic,1,0)
  data.rec$povumic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi21/icp21<umic,1,0)
  
  
  #Tables
  
  #Overall Poverty
  svydf <- svydesign(ids = ~hhid, data = data.rec, strata= ~strata, 
                     weights = ~pop_wgt)
  
  tab1=svymean(~povlic+povlmic+povumic,  design=svydf, 
               na.rm=TRUE,vartype = "ci")
  conf_int <- confint(tab1)
  tab1$year=year
  tab1$gini_cons_imp=gini.wtd(data.rec$mpce_sp_def_ind,data.rec$pop_wgt)
  tab1$gini_cons_abb=gini.wtd(data.rec$consumption_pc_adj,data.rec$pop_wgt)
  tab1$sect="national"

  
  #Urban poverty
  data.rec.subset=subset(data.rec,urban==1)
  svydf2 <- svydesign(ids = ~hhid, data = data.rec.subset, 
                      strata= ~strata, weights = ~pop_wgt)
 
  
  tab2=svymean(~povlic+povlmic+povumic,  design=svydf2, 
               na.rm=TRUE,vartype = "ci")
  conf_int2 <- confint(tab2)
  tab2$year=year
    tab2$gini_cons_imp=gini.wtd(data.rec.subset$mpce_sp_def_ind,
                                data.rec.subset$pop_wgt)
  tab2$gini_cons_abb=gini.wtd(data.rec.subset$consumption_pc_adj,
                              data.rec.subset$pop_wgt)
  tab2$sect="urban"
  
  #Urban poverty
  data.rec.subset=subset(data.rec,urban==0)
  svydf3 <- svydesign(ids = ~hhid, data = data.rec.subset, 
                      strata= ~strata, weights = ~pop_wgt)
  
  
  tab3=svymean(~povlic+povlmic+povumic,  design=svydf3, 
               na.rm=TRUE,vartype = "ci")
  conf_int3 <- confint(tab3)
  tab3$year=year
  tab3$gini_cons_imp=gini.wtd(data.rec.subset$mpce_sp_def_ind,
                              data.rec.subset$pop_wgt)
  tab3$gini_cons_abb=gini.wtd(data.rec.subset$consumption_pc_adj,
                              data.rec.subset$pop_wgt)
  tab3$sect="rural"
  
  rbind(data.frame(tab1),data.frame(tab2),data.frame(tab3))
}

stopCluster(cl)

# Bring official numbers for the anchor year

plfs=read_dta(file.path(harmonized_path, sprintf(plfs_s2_file, anchor_year)))
plfs$survey="PLFS"
plfs = plfs %>%
  mutate( weight=weight*hh_size) %>%
  rename(welfare=consumption_pc_adj) %>%
  filter(!is.na(weight))

hces=read_dta(file.path(harmonized_path, hces_file))
hces$survey="HCES"
hces = hces %>%
  rename(welfare=mpce_sp_def_ind, 
         weight=pop_wgt,
         urban=urb) %>%
  filter(!is.na(weight))

df=rbind(subset(hces,select=c(hhid,survey,urban,welfare,weight)),
         subset(plfs,select=c(hhid,survey,urban,welfare,weight)))
df=na.omit(df)

df$povlic = ifelse(df$welfare*(12/365)/cpi21/icp21<lic,1,0)
df$povlmic = ifelse(df$welfare*(12/365)/cpi21/icp21<lmic,1,0)
df$povumic = ifelse(df$welfare*(12/365)/cpi21/icp21<umic,1,0)

svydf <- svydesign(ids = ~hhid, data = df, weights = ~weight)

#Overall Poverty
tab1=svyby(~povlic+povlmic+povumic, ~survey, design=svydf, svymean,
         na.rm=TRUE,vartype = "ci")
tab1=tab1[,1:4]

#Poverty by sector
tab2=svyby(~povlic+povlmic+povumic, ~survey+urban, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")
tab2=tab2[,1:5]

gini_cons_imp=c(with(df[df$survey=="HCES",],gini.wtd(welfare,weight)),
          with(df[df$survey=="HCES" & df$urban==0,],gini.wtd(welfare,weight)),
          with(df[df$survey=="HCES" & df$urban==1,],gini.wtd(welfare,weight)))

gini_cons_abb=c(with(df[df$survey=="PLFS",],gini.wtd(welfare,weight)),
                with(df[df$survey=="PLFS" & df$urban==0,],gini.wtd(welfare,weight)),
                with(df[df$survey=="PLFS" & df$urban==1,],gini.wtd(welfare,weight)))


pov_2022=data.frame(sect=c("national","rural","urban"),
                  rbind(tab1[tab1$survey=="HCES",c("povlic","povlmic","povumic")],
                        tab2[tab2$survey=="HCES",c("povlic","povlmic","povumic")]),
                  gini_cons_imp,gini_cons_abb,year=anchor_year)

tab_export=bind_rows(pov_results,pov_2022)
row.names(tab_export)=NULL
# Export table with outputs
write.csv(tab_export,file=paste(run_path,
        "/tables/Poverty_trend_",use_mod,"_",use_stat,".csv",
                       sep=""))

rm(hces,plfs,plfs.rec,data.don,data.rec,data.rec2)   #plfs.don is kept for the figures of 09
