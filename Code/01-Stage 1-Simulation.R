### This code requires the harmonized HCES and PLFS 2022 data sets. Implements
### S2S using Lasso-based PMM constrained by state and household type
### and uses Mahalanobis distance to find the nearest neighbor.


# parallel set
numCores <- detectCores()
cl <- makeCluster(numCores-1)
registerDoParallel(cl)

#####Define custom functions####

# R squared
compute_r_squared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)  # Total sum of squares
  ss_residual <- sum((actual - predicted)^2)  # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)   # Compute R²
  return(r_squared)
}
######
geometric_mean <- function(x, na.rm = TRUE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    if(any(x <= 0)) {
        stop("All values must be positive to compute the geometric mean.")
    }
    exp(mean(log(x)))
}
#number of simulations
sim = nsim1

#Define variables to be used in the models
#Extract variable names starting with "hh_" and "sh_"
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
#covariates <- names(data.rec) #to include all available variables in receiver data set
covariates <- c(hh_vars,hhh_vars, econ_vars, inc_vars ,oth_vars,"hidseq") #include mx vars

# #covariates <- c("hhsize","hhsize_sq","age_avg","avg_age_sq","share_dep","share_kids","sh_in_school",
#                 "sh_mem_male","buddhist_hhh","married_hhh","sinhala_hhh","female_hhh","age_hhh", 
#                 "edu_hhh_none", "hh_main_agri","hh_main_ind","have_public_emp","sh_wages",
#                 "has_conc_disab","has_comms_disab","hear_disab_hhh","sh_ecactive",
#                 "ln_rpcinc1","hidseq")
covariates <- unique(covariates)
list(covariates)

data.rec$fic_dep_var=1
# Create the formula 
formula.mod.a <- as.formula(paste("welfare ~", 
                            paste(covariates, collapse = " + ")))
formula.mod.b <- as.formula(paste("fic_dep_var ~", 
                                 paste(covariates, collapse = " + ")))

  #prepare global matching parameters
  X.mtc=X.mtc1
  don.vars=don.vars1
  
  #empty objects to save results
  #matching
  simcons_match=subset(data.rec,sel=c(hhid))
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
  set.seed(seed)
  foreach (j=1:sim) %do% { #replace to dopar later
    print(paste("simulation number ",j,
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
    train.a = train.a[,unique(c("welfare",covariates))] 
    #remove NAs for training
    train.a=na.omit(train.a)
    #Design matrix for training model
    mod.a=lm(welfare~.,data=train.a)
    X.a = model.matrix(mod.a)
    sd_cols <- apply(X.a, 2, function(z) sd(as.numeric(z), na.rm = TRUE))
    X.a     <- X.a[, is.finite(sd_cols) & sd_cols > 0, drop = FALSE]
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

   #  #Predict log consumption using Adaplasso
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
    #Keep only regular Lasso for computational ease
    r2[j]=max(r2_al,r2_l)
    md[j]=ifelse(r2_l<=r2_al,"AdapLasso","Lasso")
    
    #Subset common variables
    X.newb =X.samp.b[,colnames(X.a)]
    X.a =X.samp.a[,colnames(X.newb)]
    
    if (r2_l<=r2_al) {
      Yb<-predict(lasso_best1, newx=X.newb, s=best.lambda1)
      coef_temp = data.frame(as.matrix(coefficients(lasso_best1)))
      Ya=predict(lasso_best1, newx=X.a, s=best.lambda1)
    } else {
      Yb<-predict(lasso_best2, newx=X.newb, s=best.lambda2)
      coef_temp = data.frame(as.matrix(coefficients(lasso_best2)))
      Ya=predict(lasso_best2, newx=X.a, s=best.lambda2)
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
                            dist.fun="Mahalanobis",
                            cut.don="min")
  
  #Create fused dataset
  fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                          mtc.ids=rnd.2$mtc.ids,
                          z.vars=don.vars)  
  fA.wrnd = fA.wrnd[,c("hhid","welfare")]
  names(fA.wrnd)[2]=paste("welfare_",j,sep="")
  simcons_match=merge(simcons_match,fA.wrnd,by="hhid")
  rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
  }

stopCluster(cl)
  
  
#save simulations results
#R-squared
write.csv(r2,file=paste(datapath,
   "cleaned/Outputs/Intermediate/Simulations22_R2_",sim,".csv",sep=""),
            row.names = FALSE)
#Model used
write.csv(md,file=paste(datapath,
    "cleaned/Outputs/Intermediate/Simulations22_model_used_",sim,".csv",sep=""),
            row.names = FALSE)
  
  
#Ensembles match
  simcons_match$welfare_mean=apply(simcons_match[,-1],
                                     1,mean,na.rm=TRUE)
  simcons_match$welfare_median=apply(simcons_match[,-1],
                                       1,median,na.rm=TRUE)
  simcons_match$welfare_geom=apply(simcons_match[,-1],
                                     1,geometric_mean,na.rm=TRUE)
write.csv(simcons_match,file=paste(datapath,
        "cleaned/Stage 1/Final/Simulations22_match_",sim,".csv",sep=""),
        row.names = FALSE)
saveRDS(simcons_match,file=paste(datapath,
        "cleaned/Stage 1/Final/Simulations22_match_",sim,".rds",sep=""))

#Ensembles pred
simcons_pred$welfare_mean=apply(simcons_pred[,-1],
                                   1,mean,na.rm=TRUE)
simcons_pred$welfare_median=apply(simcons_pred[,-1],
                                     1,median,na.rm=TRUE)
simcons_pred$welfare_geom=apply(simcons_pred[,-1],
                                   1,geometric_mean,na.rm=TRUE)

write.csv(simcons_pred,file=paste(datapath,
       "cleaned/Stage 1/Final/Simulations22_pred_",sim,".csv",sep=""),
          row.names = FALSE)
saveRDS(simcons_pred,file=paste(datapath,
      "cleaned/Stage 1/Final/Simulations22_pred_",sim,".rds",sep=""))

#Ensemble coefficients
coefs$coef=apply(coefs, 1,mean,na.rm=TRUE)

write.csv(coefs,file=paste(datapath,
      "cleaned/Outputs/Intermediate/Simulations22_coefficients_",sim,".csv",
       sep=""),
          row.names = TRUE)

