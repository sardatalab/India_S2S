
years=c(2017:2021)
sim = nsim2 #number of simulations that were used
rm(df)

cl <- makeCluster(length(years))
registerDoParallel(cl)

pov_results <- foreach (year = years, .combine = "rbind",
.packages = c("survey", "haven","dplyr","tidyr","dineq")) %dopar% {
  
  #Load PLFS data
  plfs.rec=read_dta(paste(datapath,
      "/Data/Stage 2/Cleaned/IND_",year,"_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",
      sep="")) 
  #Load simulations results
  df=readRDS(file=paste(datapath,
            "/Data/Stage 2/Final/Simulations_model_",use_mod,"_",year,".rds",sep=""))
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
  rm(df)
  data.rec$pop_wgt = with(data.rec,weight*hh_size)
  data.rec$state=as.factor(data.rec$state)
  data.rec$logwelfare=log(data.rec$mpce_sp_def_ind)
  data.rec$logconsumption_pc_adj=log(data.rec$consumption_pc_adj)

  ## New block: Impute within survey using XGBoost for HHS with missing welfare
  if(stage3==1){
    data.rec.temp=data.rec %>%
      mutate(logwelfare=ifelse(is.na(logwelfare),-999,logwelfare)) %>%
      filter(!is.na(logconsumption_pc_adj) & !is.na(pop_wgt) &
               logconsumption_pc_adj>-Inf) %>%
      select(where(~ !any(is.na(.x)))) %>%
      mutate(logwelfare=ifelse(logwelfare==-999,NA,logwelfare),
             ratio=exp(logwelfare)/exp(logconsumption_pc_adj))
    hh_vars = grep("^hh_", names(data.rec.temp), value = TRUE)
    oth_vars = c("urban","state","logconsumption_pc_adj")
    covariates=c(hh_vars,oth_vars)
    
    #Create dummy to split donor and receiver datasets later
    data.rec.temp$dummy.rec=ifelse(is.na(data.rec.temp$logwelfare),1,0)
    
    simcons_na=subset(data.rec.temp[data.rec.temp$dummy.rec==1,],
                      sel=c(hhid))
    foreach(j = 1:nsim2) %do% {
      cat("Simulation Stage 3: ",j, "\n")
      # Bootstrap the training data
      train_sample <- data.rec.temp %>%   
        filter(dummy.rec==0) %>%
        group_by(state) %>%
        sample_frac(n.a)
      
      mod.a=lm(logwelfare~.,data=train_sample[, c("logwelfare",covariates)])
      X_train = model.matrix(mod.a)
      y_train <- train_sample$logwelfare
      dtrain <- xgb.DMatrix(data = X_train, label = y_train)
      
      # Run CV to get optimal nrounds on the bootstrap sample
      cv_model <- xgb.cv(
        data = dtrain,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 100,
        verbose = 0
      )
      best_nrounds <- cv_model$best_iteration
      
      # Train model on the bootstrap sample
      model <- xgboost(
        data = dtrain,
        nrounds = best_nrounds,
        verbose = 0
      )
      
      # Get predictions for each survey round
      mod.full=lm(consumption_pc_adj~.,data.rec.temp[,
                                    c("dummy.rec","consumption_pc_adj",covariates)])
      X_full=model.matrix(mod.full)
      
      #Donor
      X_don = X_full[X_full[,"dummy.rec"]==0,]
      X_don=X_don[,colnames(X_don)!="dummy.rec"]
      ddon <- xgb.DMatrix(data = X_don)
      Y.a=predict(model, ddon)
      #Receiver
      X_rec = X_full[X_full[,"dummy.rec"]==1,]
      X_rec=X_rec[,colnames(X_rec)!="dummy.rec"]
      drec <- xgb.DMatrix(data = X_rec)
      Y.b=predict(model, drec)
      
      X.samp.b.pred=data.table(
        hhid = data.rec.temp[data.rec.temp$dummy.rec==1, "hhid"],
        ymatch = exp(Y.b)) 
      X.samp.a.pred = data.table(
        hhid = data.rec.temp[data.rec.temp$dummy.rec==0, "hhid"],
        ymatch = exp(Y.a))
      
      rm(Y.b,Y.a)
      
      colnames(X.samp.b.pred)=c("hhid","ymatch")
      colnames(X.samp.a.pred)=c("hhid","ymatch")
      
      #Merge predictions with original base
      samp.btemp=merge.data.table(data.rec.temp[data.rec.temp$dummy.rec==1,],
                                  X.samp.b.pred,
                                  by="hhid",all=TRUE,sort=TRUE)
      samp.atemp=merge.data.table(data.rec.temp[data.rec.temp$dummy.rec==0,],
                                  X.samp.a.pred,
                                  by="hhid",all=TRUE,sort=TRUE)
      samp.btemp=data.frame(samp.btemp)
      samp.btemp$ratio=NULL
      samp.atemp=data.frame(samp.atemp)
      
      if (min(table(data.rec.temp[data.rec.temp$dummy.rec==0,]$state,
                    data.rec.temp[data.rec.temp$dummy.rec==0,]$hh_type))>0){
        group.v <- c("state","hh_type")  # donation classes
      }  else {
        group.v <- c("state","urban")  # donation classes
      }
      
      #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
      rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                                match.vars=X.mtc3, don.class=group.v,
                                dist.fun="Euclidean",
                                cut.don="min")
      
      #Create fused dataset
      fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                              mtc.ids=rnd.2$mtc.ids,
                              z.vars=don.vars3) 
      fA.wrnd$mpce_sp_def_ind = with(fA.wrnd,
                                     ratio*consumption_pc_adj)
      fA.wrnd = fA.wrnd[,c("hhid","mpce_sp_def_ind")]
      names(fA.wrnd)[2]=paste("mpce_sp_def_ind_",j,sep="")
      simcons_na=merge(simcons_na,fA.wrnd,by="hhid")
      rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
    } #end for loop
    rm(data.rec.temp)
    df=simcons_na
    rm(simcons_na)
    df$mpce_sp_def_ind_na=apply(df[,2:(sim+1)],
                             1,use_stat,na.rm=TRUE)
  } #end if (stage3==1)
  
  ##Enable when ready to save final results
  write_dta(data.rec,paste0(datapath,
        "/Data/Stage 2/Final/IND_",year,"_PLFS_imputed_ratio_",sim,
                "_",use_stat,".dta"))
  data.rec = subset(data.rec,!is.na(mpce_sp_def_ind))
  cpi=1.101906  # 1.3317103 for 2017
  icp=19.46895  # 19.46904 for 2017
  lic=3  # 2.15 for 2017
  lmic=4.2  # 3.65 for 2017
  umic=8.3  # 6.85 for 2017
  data.rec$povlic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi/icp<lic,1,0)
  data.rec$povlmic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi/icp<lmic,1,0)
  data.rec$povumic = ifelse(data.rec$mpce_sp_def_ind*(12/365)/cpi/icp<umic,1,0)
  
  
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

# Bring official numbers for 2022

plfs=read_dta(paste0(datapath,
      "/Data/Stage 2/Cleaned/IND_2022_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta"))
plfs$survey="PLFS"
plfs = plfs %>%
  mutate( weight=weight*hh_size) %>%
  rename(welfare=consumption_pc_adj) %>%
  filter(!is.na(weight))

hces=read_dta(paste0(datapath,
                     "/Data/Stage 1/Cleaned/HCES22_s2s.dta"))
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
                  gini_cons_imp,gini_cons_abb,year=2022)

tab_export=bind_rows(pov_results,pov_2022)
row.names(tab_export)=NULL
# Export table with outputs
write.csv(tab_export,file=paste(path,
        "/Outputs/Intermediate/Poverty_trend_",use_mod,"_",use_stat,".csv",
                       sep=""))

rm(hces,plfs,plfs.don,plfs.rec,data.don,data.rec,data.rec2)
