### Stage 2, simulations: trains the models on the donor (PLFS anchor year, built in
### 06_stage2_clean.R) in train_year and applies them to the other PLFS rounds.

#number of simulations
sim=nsim2

set.seed(seed)

#########################
#FIRST YEAR (train_year)#
#########################
#We train models on the anchor year (donor) data only in the first year
year=train_year
#####Prepare receiver survey##### 
plfs.rec=read_dta(file.path(input_path, sprintf(plfs_s2_file, year)))
#create sequential IDs
plfs.rec$hidseq=seq(1:nrow(plfs.rec))
plfs.rec=subset(plfs.rec,!is.na(consumption_pc_adj))
plfs.rec=prep_plfs_s2(plfs.rec, year)   #variables of the Stage 2 models (06_stage2_clean.R)

# Missing values report
missing_report.rec <- plfs.rec %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)

df.sim.match=list()
df.sim.pred=list()


#variables to be excluded when a==1 (no labor income information)
occ_vars = grep("^hh_sh_occ", names(plfs.don), value = TRUE)
inc_vars = c("shr_regwages","shr_caswages","shr_selfinc",
             "log_labor_pc_adj")

r2.l=list()
md.l=list()
coefs.l=list()

foreach (a = c(0,1)) %do% {  # 0: HHs with income information, 1 otherwise
  if(a == 1) {
    data.don <- plfs.don %>%
      filter(flag6_income==a) %>%
      select(-all_of(c(occ_vars,inc_vars)))
    data.rec <- plfs.rec %>%
      filter(flag6_income==a) %>%
      select(-all_of(c(occ_vars,inc_vars)))
  } else {
    data.don <- plfs.don %>%
      filter(flag6_income==a)
    data.rec <- plfs.rec %>%
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
  
  #Define variables to be used in the models
  #Extract variable names starting with "hh_" (includes occ_vars when a==0)
  hh_vars = grep("^hh_", names(data.don), value = TRUE)
  oth_vars = c("urban","state","log_consumption_pc_adj")
  
  #Combine the covariate names
  if(a == 0) {
    covariates <- c(hh_vars, inc_vars, oth_vars)
    X.mtc=X.mtc2 # NN search variables
  } else {
    covariates <- c(hh_vars, oth_vars)
    X.mtc=X.mtc2 # NN search variables
  }
  
  # Create the formula 
  formula.mod.a <- as.formula(paste(paste("mpce_sp_def_ind ~", 
                                          paste(covariates, collapse = " + ")),"+hidseq",sep=""))
  #left side variable could be any numeric that is not a covariate; it is
  #used just to create the design matrix
  formula.mod.b <- as.formula(paste(paste("consumption_pc ~", 
                                          paste(covariates, collapse = " + ")),"+hidseq",sep=""))
  
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
      group_by(state) %>%
      sample_frac(n.a)
    
    #Make sure all donation classes have sufficient data
    if (min(table(train.a$state,train.a$hh_type))>0){
      group.v <- c("state","hh_type")  # donation classes
    }  else {
      group.v <- c("state","urban")  # donation classes
    }
    #subset to model covariates
    train.a = train.a[,c("mpce_sp_def_ind",covariates)] 
    #remove NAs for training
    train.a=na.omit(train.a)
    #Design matrix for training model
    mod.a=lm(mpce_sp_def_ind~.,data=train.a)
    X.a = model.matrix(mod.a)
    y.a = as.matrix(train.a$mpce_sp_def_ind)
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
      save(lasso_best1, file=paste(run_path,
            "/models/Mod_",a,"_",j,sep=""))
      save(best.lambda1, file=paste(run_path,
            "/models/Lambda_",a,"_",j,sep=""))
    } else {
      Yb<-predict(lasso_best2, newx=X.newb, s=best.lambda2)
      coef_temp = data.frame(as.matrix(coefficients(lasso_best2)))
      Ya=predict(lasso_best2, newx=X.a, s=best.lambda2)
      save(lasso_best2, file=paste(run_path,
                 "/models/Mod_",a,"_",j,sep=""))
      save(best.lambda2, file=paste(run_path,
                 "/models/Lambda_",a,"_",j,sep=""))
    }
    
    #Best model coefficients  
    names(coef_temp)=paste("coef_",j,sep="") 
    if (j==1){ coefs = coef_temp } else {coefs=cbind(coefs,coef_temp)}
    
    #save predictions from best model on PLFS
    Pred_Yb=data.table(cbind(data.rec[,"hhid"], exp(Yb)-1))
    names(Pred_Yb)=c("hhid",paste("mpce_sp_def_ind_",j,sep=""))
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
                              match.vars=X.mtc2, don.class=group.v,
                              dist.fun="Euclidean",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                            mtc.ids=rnd.2$mtc.ids,
                            z.vars=don.vars2) 
    fA.wrnd$mpce_sp_def_ind = with(fA.wrnd,
                                   ratio*consumption_pc_adj)
    fA.wrnd = fA.wrnd[,c("hhid","mpce_sp_def_ind")]
    names(fA.wrnd)[2]=paste("mpce_sp_def_ind_",j,sep="")
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
write.csv(r2.tot,file=paste(run_path,
            "/models/Simulations_R2.csv",sep=""),
          row.names = FALSE)
#Model used
write.csv(md.tot,file=paste(run_path,
              "/models/Simulations_model_used.csv",sep=""),
          row.names = FALSE)

#Ensemble coefficients
coef.tot$coef=apply(coef.tot, 1,mean,na.rm=TRUE)

write.csv(coef.tot,file=paste(run_path,
         "/models/Models_coefficients.csv",sep=""),
          row.names = TRUE)

#Simulation results
write.csv(df.match,file=paste(run_path,
                "/data/Simulations_model_match_",year,
                ".csv",sep=""),
          row.names = FALSE)
saveRDS(df.match,file=paste(run_path,
                   "/data/Simulations_model_match_",
                            year,".rds",sep=""))

write.csv(df.pred,file=paste(run_path,
                   "/data/Simulations_model_pred_",year,
                      ".csv",sep=""),
          row.names = FALSE)
saveRDS(df.pred,file=paste(run_path,
              "/data/Simulations_model_pred_",year,
                    ".rds",sep=""))
rm(year,df.match,df.pred,plfs.rec)
#############
#OTHER YEARS#
#############
#starting the 2nd year, we use the models previously trained###
years=setdiff(stage2_years, train_year)

# parallel set
cl <- makeCluster(length(years))
registerDoParallel(cl)

foreach (year = years,
         .packages = c("StatMatch", "survey", "questionr", "reldist", 
                       "glmnet", "useful", "data.table", "haven", 
                       "statar", "dplyr", "tidyr", "dineq", "convey",
                       "parallel","foreach"),
         .export   = c("plfs.don","run_path","sim",
                       "compute_r_squared")) %dopar% {

#####Prepare receiver survey##### 
plfs.rec=read_dta(file.path(input_path, sprintf(plfs_s2_file, year)))
#create sequential IDs
plfs.rec$hidseq=seq(1:nrow(plfs.rec))
plfs.rec=subset(plfs.rec,!is.na(consumption_pc_adj))
plfs.rec=prep_plfs_s2(plfs.rec, year)   #variables of the Stage 2 models (06_stage2_clean.R)

# Missing values report
missing_report.rec <- plfs.rec %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)

df.sim.match=list()
df.sim.pred=list()


#variables to be excluded when a==1 (no labor income information)
occ_vars = grep("^hh_sh_occ", names(plfs.don), value = TRUE)
inc_vars = c("shr_regwages","shr_caswages","shr_selfinc",
             "log_labor_pc_adj")

    foreach (a = c(0,1)) %do% {  # 0: HHs with income information, 1 otherwise
      if(a == 1) {
        data.don <- plfs.don %>%
          filter(flag6_income==a) %>%
          select(-all_of(c(occ_vars,inc_vars)))
        data.rec <- plfs.rec %>%
          filter(flag6_income==a) %>%
          select(-all_of(c(occ_vars,inc_vars)))
      } else {
        data.don <- plfs.don %>%
          filter(flag6_income==a)
        data.rec <- plfs.rec %>%
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
      
      #Define variables to be used in the models
      #Extract variable names starting with "hh_" (includes occ_vars when a==0)
      hh_vars = grep("^hh_", names(data.don), value = TRUE)
      oth_vars = c("urban","state","log_consumption_pc_adj")
      
      #Combine the covariate names
      if(a == 0) {
        covariates <- c(hh_vars, inc_vars, oth_vars)
        X.mtc=X.mtc2 # NN search variables
      } else {
        covariates <- c(hh_vars, oth_vars)
        X.mtc=X.mtc2 # NN search variables
      }
      
      # Create the formula 
      formula.mod.a <- as.formula(paste(paste("mpce_sp_def_ind ~", 
                      paste(covariates, collapse = " + ")),"+hidseq",sep=""))
      #left side variable could be any numeric that is not a covariate; it is
      #used just to create the design matrix
      formula.mod.b <- as.formula(paste(paste("consumption_pc ~", 
                      paste(covariates, collapse = " + ")),"+hidseq",sep=""))
      

      #empty objects to save results
      #matching
      simcons=subset(data.rec,sel=c(hhid))
      #prediction
      simcons_pred=subset(data.rec,sel=c(hhid))
      #R squared
      #r2=c()
      #md=c()
      
      #Design matrices for applying LASSO model
      #Receiver
      X.samp.b =build.x(formula = formula.mod.b, 
                        data.rec,
                        contrasts = FALSE)
      #Donor
      X.samp.a =build.x(formula = formula.mod.a, 
                        data.don,
                        contrasts = FALSE)
      #Iteration loop
      foreach (j=1:sim) %do% { #replace to dopar later
        print(paste("Year ",year," Iteration number ",j," Sector ",a,
                    sep=""))
        
        #Make sure all donation classes have sufficient data
        if (min(table(data.don$state,data.don$hh_type))>0){
          group.v <- c("state","hh_type")  # donation classes
        }  else {
          group.v <- c("state","urban")  # donation classes
        }
        
        X.newb =X.samp.b
        X.a =X.samp.a
        lasso_best=load(file=paste(run_path,
                                   "/models/Mod_",a,"_",j,
                                   sep=""))
        if (lasso_best=="lasso_best1"){
          predictor_names <- setdiff(rownames(coef(lasso_best1)), "(Intercept)")
          X.newb <- X.newb[, predictor_names, drop = FALSE]
          X.newb=data.frame(`(Intercept)`=1,X.newb)
          X.newb=as.matrix(X.newb)
          Yb<-predict(lasso_best1, newx=X.newb)
          X.a <- X.a[, predictor_names, drop = FALSE]
          X.a=data.frame(`(Intercept)`=1,X.a)
          X.a=as.matrix(X.a)
          Ya=predict(lasso_best1, newx=X.a)
        } else{
          predictor_names <- setdiff(rownames(coef(lasso_best2)), "(Intercept)")
          X.newb <- X.newb[, predictor_names, drop = FALSE]
          X.newb=data.frame(`(Intercept)`=1,X.newb)
          X.newb=as.matrix(X.newb)
          Yb<-predict(lasso_best2, newx=X.newb)
          X.a <- X.a[, predictor_names, drop = FALSE]
          X.a=data.frame(`(Intercept)`=1,X.a)
          X.a=as.matrix(X.a)
          Ya=predict(lasso_best2, newx=X.a)
        }
        
        #save predictions from best model on PLFS
        Pred_Yb=data.table(cbind(data.rec[,"hhid"], exp(Yb)-1))
        names(Pred_Yb)=c("hhid",paste("mpce_sp_def_ind_",j,sep=""))
        simcons_pred=merge(simcons_pred,Pred_Yb,by="hhid")
        
        #Calculate consumption predictions on both surveys
        X.samp.b.pred=data.table(cbind(X.samp.b[,"hidseq"], exp(Yb)-1))
        X.samp.a.pred=data.table(cbind(X.samp.a[,"hidseq"], exp(Ya)-1))
        rm(Yb,Ya)
        colnames(X.samp.b.pred)=c("hidseq","ymatch")
        colnames(X.samp.a.pred)=c("hidseq","ymatch")
        
        #Merge predictions with original base
        samp.btemp=merge.data.table(data.rec,X.samp.b.pred,
                                    by="hidseq",all=TRUE,sort=TRUE)
        samp.atemp=merge.data.table(data.don,X.samp.a.pred,
                                    by="hidseq",all=TRUE,sort=TRUE)
        rm(X.samp.b.pred,X.samp.a.pred,X.a,X.newb,
           Pred_Yb)
        if (lasso_best=="lasso_best1"){
          rm(lasso_best1,lasso_best,best.lambda)
        } else{
          rm(lasso_best2,lasso_best,best.lambda)
        }
        samp.btemp=data.frame(samp.btemp)
        samp.atemp=data.frame(samp.atemp)
        row.names(samp.btemp)=as.character(seq(1:nrow(samp.btemp)))
        row.names(samp.atemp)=as.character(seq(1:nrow(samp.atemp)))
        
        #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
        rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                                  match.vars=X.mtc2, don.class=group.v,
                                  dist.fun="Euclidean",
                                  cut.don="min")
        
        #Create fused dataset 
        fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                                mtc.ids=rnd.2$mtc.ids,
                                z.vars=don.vars2) 
        fA.wrnd$mpce_sp_def_ind = with(fA.wrnd,
                                       ratio*consumption_pc_adj)
        fA.wrnd = fA.wrnd[,c("hhid","mpce_sp_def_ind")]
        names(fA.wrnd)[2]=paste("mpce_sp_def_ind_",j,sep="")
        simcons=merge(simcons,fA.wrnd,by="hhid")
        rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
      }
      df.sim.match[[as.character(a)]]=simcons
      df.sim.pred[[as.character(a)]]=simcons_pred
      rm(data.don,data.rec,simcons,simcons_pred)
    } #end foreach loop in 'a' (income availability)
    df.match <- do.call(rbind, df.sim.match)
    df.pred <- do.call(rbind, df.sim.pred)
    
    #save simulations results

    write.csv(df.match,file=paste(run_path,
                       "/data/Simulations_model_match_",
                                  year,".csv",sep=""),
              row.names = FALSE)
    saveRDS(df.match,file=paste(run_path,
                      "/data/Simulations_model_match_",
                                year,".rds",sep=""))
    
    write.csv(df.pred,file=paste(run_path,
                    "/data/Simulations_model_pred_",
                                 year,".csv",sep=""),
              row.names = FALSE)
    saveRDS(df.pred,file=paste(run_path,
                    "/data/Simulations_model_pred_",
                               year,".rds",sep=""))
    
} #end year loop
stopCluster(cl)
rm(years,df.match,df.pred,plfs.rec)
