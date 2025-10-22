
#number of simulations
sim=nsim2

# #create sequential Ids
# #plfs.don$hidseq=seq(1:nrow(plfs.don))
# #Create quintiles of abbreviated consumption
# #plfs.don$state <- as.factor(plfs.don$state)
# plfs.don$hhb_year=with(plfs.don,2023-age_hhh)
# plfs.don$hh_head_age_sq = with(plfs.don,age_hhh^2)
# plfs.don$hh_size_sq = with(plfs.don,hh_size^2)
# plfs.don$log_labor_pc_adj=log(plfs.don$total_labor_pc_adj+1)
# plfs.don$log_consumption_pc_adj=log(plfs.don$consumption_pc_adj+1)
# #Drop dummies for state and hhtype
# plfs.don <- plfs.don %>% 
#   mutate(across(
#     c(hh_sh_job_contract, hh_sh_paidleave, hh_sh_pension, 
#       hh_sh_gratuity, hh_sh_health),
#     ~ if_else(flag5_all_lab == 1 & is.na(.x), 0, .x)
#   )) %>%
#   select(-starts_with("state_"))  %>% 
#   select(-starts_with("hhtype_")) %>%
#   select(-starts_with("hh_sp_")) %>%
#   select(-starts_with("hh_size_age")) %>%
#   select(-c(hh_sex_ratio,flag2_sex,hh_dep_ratio,flag1_depen,
#             hh_sh1564_lit,flag3_spouse,
#             hh_head_job_contract,hh_head_paidleave,
#             hh_head_pension,hh_head_gratuity,
#             hh_head_health, flag4_head_lab,
#             hh_sh_age0,hh_sh_age1,hh_sh_age2,hh_sh_age3,
#             hh_sh_age4,flag5_all_lab,hh_head_ilit,hh_sh_ilit,
#             hh_sh_female))
# # Missing values report
# missing_report.don <- plfs.don %>%
#   summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
#   pivot_longer(cols = everything(), 
#                names_to = "Variable", values_to = "PercentMissing")
# subset(missing_report.don,PercentMissing>0)

set.seed(seed)

#Current date
date_mod=Sys.Date()


#############
#YEAR = 2023#
#############
#We train models in 2022 data only in the first year (2019)
year=2023
#####Prepare receiver survey##### this needs to be LFS 2023 file - cleaned and harmonized 
lfs.rec=read_dta(paste(datapath,
      "cleaned/lfs2023_clean.dta",
      sep="")) 
#create sequential IDs
lfs.rec$hidseq=seq(1:nrow(lfs.rec))


lfs.rec = lfs.rec %>%
    select(-sex_ratio)

# Missing values report
missing_report.rec <- lfs.rec %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)


df.sim.match=list()
df.sim.pred=list()


#variables to be excluded when a==1 (with zero incomes)
inc_vars    = c("ln_rpcinc1", "sh_wages","sh_selfemp")

r2.l=list()
md.l=list()
coefs.l=list()

foreach (a = c(0,1)) %do% {  # 0: HHs with income information, 1 otherwise
  if(a == 1) {
    data.don <- lfs.don %>%
      filter(flag6_income==a) %>% #generate this variable as a filter  in both LFS rounds if income = 1 
      select(-all_of(c(inc_vars))) # exclude income vars if zero 
    data.rec <- lfs.rec %>%
      filter(flag6_income==a) %>%
      select(-all_of(c(inc_vars)))
  } else {
    data.don <- lfs.don %>% #generating two objects based on availability of income data
      filter(flag6_income==a) 
    data.rec <- lfs.rec %>%
      filter(flag6_income==a)
  }
  # Missing values report data.don
  missing_report.don <- data.don %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
  subset(missing_report.don,PercentMissing>0)
  data.don=na.omit(data.don)
  
  # Missing values report data.rec
  missing_report.rec <- data.rec %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
  subset(missing_report.rec,PercentMissing>0)
  data.rec=na.omit(data.rec)
  
  #Define variables to be used in the models: use same model as Stage 1 for now
  hh_vars = c("hhsize","hhsize_sq","age_avg","avg_age_sq","share_dep","share_kids","sh_mem_014",
              "sh_mem_1564","sh_mem_male","has_in_school","sh_in_school")
  hhh_vars = c("buddhist_hhh","married_hhh","sinhala_hhh","female_hhh","age_hhh", 
               "edu_hhh_none","edu_hhh_prim","edu_hhh_sec","edu_hhh_secincomp","edu_hhh_high")
  econ_vars = c("hh_main_agri","hh_main_ind","hh_main_serv","have_agri_emp","have_constr_emp",
                "have_ind_emp","have_public_emp","have_semiskilled_worker", "sh_selfempl","public_emp_hhh")
  disab_vars = c("has_conc_disab","has_comms_disab","eye_disab_hhh","hear_disab_hhh") #larger differences so dropping for now- can use in step 2
  inc_vars    = c("ln_rpcinc1", "sh_wages","sh_selfemp")
  oth_vars    = c("urban","district")
  
  #Combine the covariate names
  if(a == 0) {
      covariates <- c(hh_vars,hhh_vars, econ_vars,inc_vars,oth_vars,"hidseq")
    X.mtc=c("ymatch","hhsize",
            "hhb_year") # NN search variables
  } else {
      covariates <- c(hh_vars,hhh_vars, econ_vars ,oth_vars,"hidseq")
    X.mtc=c("ymatch","hhsize","hhb_year") # NN search variables
  }
  data.rec$fic_dep_var=1
  # Create the formula 
  formula.mod.a <- as.formula(paste(paste("welfare ~", 
                                          paste(covariates, collapse = " + ")),"+hidseq",sep=""))
  #left side variable could be any numeric that is not a covariate; it is
  #used just to create the design matrix
  formula.mod.b <- as.formula(paste(paste("fic_dep_var ~", 
                                          paste(covariates, collapse = " + ")),"+hidseq",sep=""))

  
  #prepare global matching parameters
  don.vars=don.vars2 #variables to be imputed

  #empty objects to save results
  #matching
  simcons=subset(data.rec,sel=c(hhid))
  #prediction
  simcons_pred=subset(data.rec,sel=c(hhid))
  #R squared
  r2=c()
  md=c()
  
  #Design matrices for applying LASSO model
  X.samp.b =build.x(formula = formula.mod.b, 
                    data.rec,
                    contrasts = FALSE)
  X.samp.a =build.x(formula = formula.mod.a, 
                    data.don,
                    contrasts = FALSE)
  #Simulation loop
  
  foreach (j=1:sim) %do% { #replace to dopar later
    print(paste("Year ",year," Simulation number ",j," Sector ",a,
                sep=""))
    
    #Sample (preserves state participation in sample)
    train.a <- data.don %>%
      group_by(district) %>%
      sample_frac(n.a)
    
    #Make sure all donation classes have sufficient data
    if (min(table(train.a$district,train.a$urban))>0){
      group.v <- c("district","urban")  # donation classes
    }  else {
      group.v <- c("district")  # donation classes
    }
    #subset to model covariates
    train.a = train.a[,c("welfare",covariates)] 
    #remove NAs for training
    train.a=na.omit(train.a)
    #Design matrix for training model
    mod.a=lm(welfare~.,data=train.a)
    X.a = model.matrix(mod.a)
    y.a = as.matrix(train.a$welfare)
    y.a= log(y.a+1)
    
    #Estimation
    #AdapLASSO
    cv.ridge1 = cv.glmnet(X.a, y=y.a,alpha=0)
    ridge_coefs1 <- coef(cv.ridge1, s = "lambda.min")
    # Compute adaptive weights (inverse of absolute Ridge coefficients)
    # We exclude the intercept (ridge_coefs[1])
    # Adding small value to avoid division by zero
    adapt_wgts1 <- 1 / (abs(ridge_coefs1[-1]) + 1e-6)
    #Fit Lasso through CV using adaptive weights
    cv.lasso1 = cv.glmnet(X.a, y=y.a,
                          penalty.factor = adapt_wgts1)
    best.lambda1 = cv.lasso1$lambda.min
    lasso_best1 <- glmnet(X.a, y=y.a, alpha = 1, 
                          lambda = best.lambda1, 
                          penalty.factor = adapt_wgts1)
    
    #Predict log consumption using Adaplasso
    Ya.al<-predict(lasso_best1, newx=X.a, s=best.lambda1)
    r2_al <- compute_r_squared(y.a, Ya.al)
    
    #LASSO
    cv.lasso2 = cv.glmnet(X.a, y=y.a, alpha=1)
    best.lambda2 = cv.lasso2$lambda.min
    lasso_best2 <- glmnet(X.a, y=y.a, alpha = 1, 
                          lambda = best.lambda2)
    #Predict log consumption using lasso
    Ya.l<-predict(lasso_best2, newx=X.a, s=best.lambda2)
    r2_l <- compute_r_squared(y.a, Ya.l)
    r2[j]=max(r2_al,r2_l)
    md[j]=ifelse(r2_l<=r2_al,"AdapLasso","Lasso")
    
    #Subset common variables
    X.newb =X.samp.b[,colnames(X.a)]
    X.a =X.samp.a[,colnames(X.newb)]
    
    if (r2_l<=r2_al) {
      Yb<-predict(lasso_best1, newx=X.newb, s=best.lambda1)
      coef_temp = data.frame(as.matrix(coefficients(lasso_best1)))
      Ya=predict(lasso_best1, newx=X.a, s=best.lambda1)
      save(lasso_best1, file=paste(path,
            "/Outputs/Intermediate/Models/Mod_",a,"_",j,sep=""))
      save(best.lambda1, file=paste(path,
            "/Outputs/Intermediate/Models/Lambda_",a,"_",j,sep=""))
    } else {
      Yb<-predict(lasso_best2, newx=X.newb, s=best.lambda2)
      coef_temp = data.frame(as.matrix(coefficients(lasso_best2)))
      Ya=predict(lasso_best2, newx=X.a, s=best.lambda2)
      save(lasso_best2, file=paste(path,
                 "/Outputs/Intermediate/Models/Mod_",a,"_",j,sep=""))
      save(best.lambda2, file=paste(path,
                 "/Outputs/Intermediate/Models/Lambda_",a,"_",j,sep=""))
    }
    
    #Best model coefficients  
    names(coef_temp)=paste("coef_",j,sep="") 
    if (j==1){ coefs = coef_temp } else {coefs=cbind(coefs,coef_temp)}
    
    #save predictions from best model on PLFS
    Pred_Yb=data.table(cbind(data.rec[,"hhid"], exp(Yb)-1))
    names(Pred_Yb)=c("hhid",paste("welfare_",j,sep=""))
    simcons_pred=merge(simcons_pred,Pred_Yb,by="hhid")
    
    #Calculate consumption predictions on both surveys
    X.samp.b.pred=data.table(cbind(X.samp.b[,"hidseq"], exp(Yb)-1))
    X.samp.a.pred=data.table(cbind(X.samp.a[,"hidseq"], exp(Ya)-1))
    rm(Yb,Ya,Ya.l,Ya.al)
    colnames(X.samp.b.pred)=c("hidseq","ymatch")
    colnames(X.samp.a.pred)=c("hidseq","ymatch")
    
    #Merge predictions with original base
    samp.btemp=merge.data.table(data.rec,X.samp.b.pred,
                                by="hidseq",all=TRUE,sort=TRUE)
    samp.atemp=merge.data.table(data.don,X.samp.a.pred,
                                by="hidseq",all=TRUE,sort=TRUE)
    rm(train.a,y.a,X.samp.b.pred,X.samp.a.pred,X.a,X.newb,
       mod.a,cv.lasso1,best.lambda1,
       lasso_best1,adapt_wgts1,
       ridge_coefs1,cv.ridge1, coef_temp,Pred_Yb,
       r2_l,r2_al,cv.lasso2,lasso_best2,best.lambda2)
    samp.btemp=data.frame(samp.btemp)
    samp.atemp=data.frame(samp.atemp)
    row.names(samp.btemp)=as.character(seq(1:nrow(samp.btemp)))
    row.names(samp.atemp)=as.character(seq(1:nrow(samp.atemp)))
    
    #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
    rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                              match.vars=X.mtc, don.class=group.v,
                              dist.fun="Euclidean",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                            mtc.ids=rnd.2$mtc.ids,
                            z.vars=don.vars2) 
    fA.wrnd$welfare = with(fA.wrnd,ifelse(flag6_income==0,
                                   ratio*rpcinc1,welfare))
    fA.wrnd = fA.wrnd[,c("hhid","welfare")]
    names(fA.wrnd)[2]=paste("welfare_",j,sep="")
    simcons=merge(simcons,fA.wrnd,by="hhid")
    rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
  }
  coefs.l[[as.character(a)]]=coefs
  r2.l[[as.character(a)]]=r2
  md.l[[as.character(a)]]=md
  df.sim.match[[as.character(a)]]=simcons
  df.sim.pred[[as.character(a)]]=simcons_pred
  rm(data.don,data.rec,r2,md,simcons,simcons_pred)
} #end foreach loop in 'a' (income availability)
coef.tot <- do.call(rbind, coefs.l)
r2.tot <- do.call(rbind, r2.l)
md.tot <- do.call(rbind, md.l)
df.match <- do.call(rbind, df.sim.match)
df.pred <- do.call(rbind, df.sim.pred)

#save simulations results
#R-squared
write.csv(r2.tot,file=paste(path,
                  "/Outputs/Intermediate/Models/Simulations_R2.csv",sep=""),
          row.names = FALSE)
#Model used
write.csv(md.tot,file=paste(path,
                      "/Outputs/Intermediate/Models/Simulations_model_used.csv",sep=""),
          row.names = FALSE)

#Ensemble coefficients
coef.tot$coef=apply(coef.tot, 1,mean,na.rm=TRUE)

write.csv(coef.tot,file=paste(path,
         "cleaned/Outputs/Intermediate/Models/Models_coefficients.csv",sep=""),
          row.names = TRUE)

#Simulation results
write.csv(df.match,file=paste(datapath,
                "cleaned/Stage 2/Final/Simulations_model_match_",year,
                ".csv",sep=""),
          row.names = FALSE)
saveRDS(df.match,file=paste(datapath,
                            "cleaned/Stage 2/Final/Simulations_model_match_",
                            year,".rds",sep=""))

write.csv(df.pred,file=paste(datapath,
                             "/Data/Stage 2/Final/Simulations_model_pred_",year,
                      ".csv",sep=""),
          row.names = FALSE)
saveRDS(df.pred,file=paste(datapath,
                           "cleaned/Stage 2/Final/Simulations_model_pred_",year,
                    ".rds",sep=""))

df.match$welfare_mean=apply(df.match[,-1],
                                 1,mean,na.rm=TRUE)
df.match$welfare_median=apply(df.match[,-1],
                                   1,median,na.rm=TRUE)
df.match$welfare_geom=apply(df.match[,-1],
                                 1,geometric_mean,na.rm=TRUE)


lfs.imp=merge(lfs.rec,df.match[,c("hhid","welfare_geom")],by="hhid",
              all.x=TRUE)

lfs.imp = lfs.imp %>%
    rename(welfare=welfare_geom)

gini.23=with(lfs.imp,gini.wtd(welfare,popwt))

lfs.imp$pov30 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<3,1,0)
lfs.imp$pov42 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<4.2,1,0)
lfs.imp$pov83 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<8.3,1,0)

svydf <- svydesign(ids = ~hhid, data = lfs.imp, 
                   weights = ~popwt)


svymean(~pov30+pov42+pov83, design=svydf,
           na.rm=TRUE,vartype = "ci")

########################
#OTHER YEARS AFTER 2017#
########################
# #starting the 2nd year, we use the models previously trained###
# years=2018:2021
# 
# # parallel set
# cl <- makeCluster(length(years))
# registerDoParallel(cl)
# 
# foreach (year = years,
#          .packages = c("StatMatch", "survey", "questionr", "reldist", 
#                        "glmnet", "useful", "data.table", "haven", 
#                        "statar", "dplyr", "tidyr", "dineq", "convey",
#                        "parallel","foreach"),
#          .export   = c("plfs.don","path","sim","date_mod",
#                        "compute_r_squared")) %dopar% {
# 
# #####Prepare receiver survey##### 
# plfs.rec=read_dta(paste(path,
#    "/Data/PLFS/IND_",year,"_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",sep="")) 
# #create sequential IDs
# plfs.rec$hidseq=seq(1:nrow(plfs.rec))
# plfs.rec=subset(plfs.rec,!is.na(consumption_pc_adj))
# plfs.rec$hh_type <- as.factor(plfs.rec$hh_type)
# plfs.rec$state <- as.factor(plfs.rec$state)
# plfs.rec$hhb_year=with(plfs.rec,year-hh_head_age)
# plfs.rec$hh_head_age_sq = with(plfs.rec,hh_head_age^2)
# plfs.rec$hh_size_sq = with(plfs.rec,hh_size^2)
# plfs.rec$log_labor_pc_adj=log(plfs.rec$total_labor_pc_adj+1)
# plfs.rec$log_consumption_pc_adj=log(plfs.rec$consumption_pc_adj+1)
# #Drop dummies for state and hhtype
# plfs.rec <- plfs.rec %>% 
#   mutate(across(
#     c(hh_sh_job_contract, hh_sh_paidleave, hh_sh_pension, 
#       hh_sh_gratuity, hh_sh_health),
#     ~ if_else(flag5_all_lab == 1 & is.na(.x), 0, .x)
#   )) %>%
#   select(-starts_with("state_"))  %>% 
#   select(-starts_with("hhtype_")) %>%
#   select(-starts_with("hh_sp_")) %>%
#   select(-starts_with("hh_size_age")) %>%
#   select(-c(hh_sex_ratio,flag2_sex,hh_dep_ratio,flag1_depen,
#             hh_sh1564_lit,flag3_spouse,
#             hh_head_job_contract,hh_head_paidleave,
#             hh_head_pension,hh_head_gratuity,
#             hh_head_health, flag4_head_lab,
#             hh_sh_age0,hh_sh_age1,hh_sh_age2,hh_sh_age3,
#             hh_sh_age4,flag5_all_lab,hh_head_ilit,hh_sh_ilit,
#             hh_sh_female))
# 
# # Missing values report
# missing_report.rec <- plfs.rec %>%
#   summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
#   pivot_longer(cols = everything(), 
#                names_to = "Variable", values_to = "PercentMissing")
# subset(missing_report.rec,PercentMissing>0)
# 
# df.sim.match=list()
# df.sim.pred=list()
# 
# 
# #variables to be excluded when a==1 (no labor income information)
# occ_vars = grep("^hh_sh_occ", names(plfs.don), value = TRUE)
# inc_vars = c("shr_regwages","shr_caswages","shr_selfinc",
#              "log_labor_pc_adj")
# 
#     foreach (a = c(0,1)) %do% {  # 0: HHs with income information, 1 otherwise
#       if(a == 1) {
#         data.don <- plfs.don %>%
#           filter(flag6_income==a) %>%
#           select(-all_of(c(occ_vars,inc_vars)))
#         data.rec <- plfs.rec %>%
#           filter(flag6_income==a) %>%
#           select(-all_of(c(occ_vars,inc_vars)))
#       } else {
#         data.don <- plfs.don %>%
#           filter(flag6_income==a)
#         data.rec <- plfs.rec %>%
#           filter(flag6_income==a)
#       }
#       # Missing values report data.don
#       missing_report.don <- data.don %>%
#         summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
#         pivot_longer(cols = everything(), 
#                      names_to = "Variable", values_to = "PercentMissing")
#       subset(missing_report.don,PercentMissing>0)
#       data.don=na.omit(data.don)
#       
#       # Missing values report data.rec
#       missing_report.rec <- data.rec %>%
#         summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
#         pivot_longer(cols = everything(), 
#                      names_to = "Variable", values_to = "PercentMissing")
#       subset(missing_report.rec,PercentMissing>0)
#       data.rec=na.omit(data.rec)
#       
#       #Define variables to be used in the models
#       #Extract variable names starting with "hh_" (includes occ_vars when a==0)
#       hh_vars = grep("^hh_", names(data.don), value = TRUE)
#       oth_vars = c("urban","state","log_consumption_pc_adj")
#       
#       #Combine the covariate names
#       if(a == 0) {
#         covariates <- c(hh_vars, inc_vars, oth_vars)
#         X.mtc=c("ymatch","hh_size",
#                 "hhb_year") # NN search variables
#       } else {
#         covariates <- c(hh_vars, oth_vars)
#         X.mtc=c("ymatch","hh_size","hhb_year") # NN search variables
#       }
#       
#       # Create the formula 
#       formula.mod.a <- as.formula(paste(paste("mpce_sp_def_ind ~", 
#                       paste(covariates, collapse = " + ")),"+hidseq",sep=""))
#       #left side variable could be any numeric that is not a covariate; it is
#       #used just to create the design matrix
#       formula.mod.b <- as.formula(paste(paste("consumption_pc ~", 
#                       paste(covariates, collapse = " + ")),"+hidseq",sep=""))
#       
#       #prepare global matching parameters
#       don.vars=c("ratio") #variables to be imputed
#       
#       # resampling parameter 80%
#       #n.a = .8
#       #empty objects to save results
#       #matching
#       simcons=subset(data.rec,sel=c(hhid))
#       #prediction
#       simcons_pred=subset(data.rec,sel=c(hhid))
#       #R squared
#       #r2=c()
#       #md=c()
#       
#       #Design matrices for applying LASSO model
#       #Receiver
#       X.samp.b =build.x(formula = formula.mod.b, 
#                         data.rec,
#                         contrasts = FALSE)
#       #Donor
#       X.samp.a =build.x(formula = formula.mod.a, 
#                         data.don,
#                         contrasts = FALSE)
#       #Iteration loop
#       foreach (j=1:sim) %do% { #replace to dopar later
#         print(paste("Year ",year," Iteration number ",j," Sector ",a,
#                     sep=""))
#         
#         #Make sure all donation classes have sufficient data
#         if (min(table(data.don$state,data.don$hh_type))>0){
#           group.v <- c("state","hh_type")  # donation classes
#         }  else {
#           group.v <- c("state","urban")  # donation classes
#         }
#         
#         X.newb =X.samp.b
#         X.a =X.samp.a
#         lasso_best=load(file=paste(path,
#                                    "/Outputs/Intermediate/Models/Mod_",a,"_",j,
#                                    sep=""))
#         if (lasso_best=="lasso_best1"){
#           predictor_names <- setdiff(rownames(coef(lasso_best1)), "(Intercept)")
#           X.newb <- X.newb[, predictor_names, drop = FALSE]
#           X.newb=data.frame(`(Intercept)`=1,X.newb)
#           X.newb=as.matrix(X.newb)
#           Yb<-predict(lasso_best1, newx=X.newb)
#           X.a <- X.a[, predictor_names, drop = FALSE]
#           X.a=data.frame(`(Intercept)`=1,X.a)
#           X.a=as.matrix(X.a)
#           Ya=predict(lasso_best1, newx=X.a)
#         } else{
#           predictor_names <- setdiff(rownames(coef(lasso_best2)), "(Intercept)")
#           X.newb <- X.newb[, predictor_names, drop = FALSE]
#           X.newb=data.frame(`(Intercept)`=1,X.newb)
#           X.newb=as.matrix(X.newb)
#           Yb<-predict(lasso_best2, newx=X.newb)
#           X.a <- X.a[, predictor_names, drop = FALSE]
#           X.a=data.frame(`(Intercept)`=1,X.a)
#           X.a=as.matrix(X.a)
#           Ya=predict(lasso_best2, newx=X.a)
#         }
#         
#         #save predictions from best model on PLFS
#         Pred_Yb=data.table(cbind(data.rec[,"hhid"], exp(Yb)-1))
#         names(Pred_Yb)=c("hhid",paste("mpce_sp_def_ind_",j,sep=""))
#         simcons_pred=merge(simcons_pred,Pred_Yb,by="hhid")
#         
#         #Calculate consumption predictions on both surveys
#         X.samp.b.pred=data.table(cbind(X.samp.b[,"hidseq"], exp(Yb)-1))
#         X.samp.a.pred=data.table(cbind(X.samp.a[,"hidseq"], exp(Ya)-1))
#         rm(Yb,Ya)
#         colnames(X.samp.b.pred)=c("hidseq","ymatch")
#         colnames(X.samp.a.pred)=c("hidseq","ymatch")
#         
#         #Merge predictions with original base
#         samp.btemp=merge.data.table(data.rec,X.samp.b.pred,
#                                     by="hidseq",all=TRUE,sort=TRUE)
#         samp.atemp=merge.data.table(data.don,X.samp.a.pred,
#                                     by="hidseq",all=TRUE,sort=TRUE)
#         rm(X.samp.b.pred,X.samp.a.pred,X.a,X.newb,
#            Pred_Yb)
#         if (lasso_best=="lasso_best1"){
#           rm(lasso_best1,lasso_best,best.lambda)
#         } else{
#           rm(lasso_best2,lasso_best,best.lambda)
#         }
#         samp.btemp=data.frame(samp.btemp)
#         samp.atemp=data.frame(samp.atemp)
#         row.names(samp.btemp)=as.character(seq(1:nrow(samp.btemp)))
#         row.names(samp.atemp)=as.character(seq(1:nrow(samp.atemp)))
#         
#         #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
#         rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
#                                   match.vars=X.mtc, don.class=group.v,
#                                   dist.fun="Euclidean",
#                                   cut.don="min")
#         
#         #Create fused dataset 
#         fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
#                                 mtc.ids=rnd.2$mtc.ids,
#                                 z.vars=don.vars) 
#         fA.wrnd$mpce_sp_def_ind = with(fA.wrnd,
#                                        ratio*consumption_pc_adj)
#         fA.wrnd = fA.wrnd[,c("hhid","mpce_sp_def_ind")]
#         names(fA.wrnd)[2]=paste("mpce_sp_def_ind_",j,sep="")
#         simcons=merge(simcons,fA.wrnd,by="hhid")
#         rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
#       }
#       df.sim.match[[as.character(a)]]=simcons
#       df.sim.pred[[as.character(a)]]=simcons_pred
#       rm(data.don,data.rec,simcons,simcons_pred)
#     } #end foreach loop in 'a' (income availability)
#     df.match <- do.call(rbind, df.sim.match)
#     df.pred <- do.call(rbind, df.sim.pred)
#     
#     #save simulations results
# 
#     write.csv(df.match,file=paste(datapath,
#                                   "/Data/Stage 2/Final/Simulations_model_match_",
#                                   year,".csv",sep=""),
#               row.names = FALSE)
#     saveRDS(df.match,file=paste(datapath,
#                                 "/Data/Stage 2/Final/Simulations_model_match_",
#                                 year,".rds",sep=""))
#     
#     write.csv(df.pred,file=paste(datapath,
#                                  "/Data/Stage 2/Final/Simulations_model_pred_",
#                                  year,".csv",sep=""),
#               row.names = FALSE)
#     saveRDS(df.pred,file=paste(datapath,
#                                "/Data/Stage 2/Final/Simulations_model_pred_",
#                                year,".rds",sep=""))
#     
# } #end year loop
stopCluster(cl)
