# Parallelized Hyperparameter Tuning Loop
# Different XGB models: one predicting income, another predicting ratio, all of this
# for HHs without inc variables, and finally, another predicting welfare for the rest, 
# Euclidean distance, geom_mean ensemble

library(xgboost)
library(matrixStats)


# -----------------------------
# Inputs:
# -----------------------------
# training Data: PLFS22
lfs.don <- read_dta(paste(datapath,
                            "cleaned/Stage 1/Final/Imputed_PLFS_22_match.dta",
                            sep="")) 
lfs.don$ratio_tot=with(lfs.don,welfare/rpcinc_tot)
lfs.don$logwelfare=log(lfs.don$welfare)
lfs.don$district=as.factor(lfs.don$district)
lfs.don$ratio_tot=ifelse(lfs.don$ratio_tot<Inf,lfs.don$ratio_tot,NA)

# Missing values report
missing_report.don <- lfs.don %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)

#####Prepare receiver survey##### 
lfs.rec=read_dta(paste(datapath,
                       "cleaned/lfs2016_clean.dta",
                       sep="")) 

lfs.rec$hhb_year=lfs.rec$hhb_year+2    #check this with Marta
lfs.rec$district=as.factor(lfs.rec$district)

#create sequential IDs
#lfs.rec$hidseq=seq(1:nrow(lfs.rec))

missing_report.rec <- lfs.rec %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)

vars1=setdiff(names(lfs.don),names(lfs.rec))
#vars1=setdiff(vars1,c("welfare","ratio_tot"))
vars2=setdiff(names(lfs.rec),names(lfs.don))

var2excl=c(vars1,vars2,"hhid","psu","weight",
           "rpcinc1","rpcwage1","rpcself1",
           "flag6_income","flag6_income2",
           "public_emp_hhh","private_emp_hhh",
           "popwt","ln_rpcinc1","ln_rpcwage1","ln_rpcself1")
covariates=setdiff(names(lfs.don),var2excl)

set.seed(1729)  # For reproducibility

# -----------------------------
# Step 1: Hyperparameter Tuning to predict ratio
# trained on 2019 LFS data for HHs with ratio_tot != NA (& <Inf)
# -----------------------------

# Set up parallel backend using available cores
n_cores <- parallel::detectCores() - 1  # Reserve one core for OS
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Define candidate values for hyperparameters
max_depth_values <- c(3, 5, 7)
gamma_values <- c(0, 0.1, 0.3)
subsample_values <- c(0.7, 0.8, 0.9)
colsample_bytree_values <- c(0.6, 0.8, 1.0)

# Create a grid of all parameter combinations
param_grid <- expand.grid(max_depth = max_depth_values,
                          gamma = gamma_values,
                          subsample = subsample_values,
                          colsample_bytree = colsample_bytree_values)

# Prepare training data (2019)
mod.full=lm(ratio_tot~.,
            data=lfs.don[!is.na(lfs.don$ratio_tot), c("ratio_tot",covariates)])
X_train_full = model.matrix(mod.full)
y_train_full <- lfs.don[!is.na(lfs.don$ratio_tot),]$ratio_tot

# Base parameters (others will be added from the grid)
base_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  nthread = n_cores
)

# Parallel grid search using foreach
tuning_results_r <- foreach(i = 1:nrow(param_grid), 
    .combine = rbind, 
    .packages = "xgboost",
    .export = c("X_train_full", "y_train_full")) %dopar% {
                            
  dtrain_full <- xgb.DMatrix(data = X_train_full, label = y_train_full)
  params <- c(base_params,
               list(max_depth = param_grid$max_depth[i],
               gamma = param_grid$gamma[i],
                subsample = param_grid$subsample[i],
                colsample_bytree = param_grid$colsample_bytree[i]))
                            
   cv_model <- xgb.cv(
     params = params,
     data = dtrain_full,
     nrounds = 100,
     nfold = 5,
     early_stopping_rounds = 10,
     verbose = 0
    )
                            
   best_iter <- cv_model$best_iteration
   best_rmse <- cv_model$evaluation_log$test_rmse_mean[best_iter]
                            
   data.frame(max_depth = param_grid$max_depth[i],
             gamma = param_grid$gamma[i],
            subsample = param_grid$subsample[i],
            colsample_bytree = param_grid$colsample_bytree[i],
                                       best_rmse = best_rmse,
                                       best_iteration = best_iter)
}
# Stop the cluster after tuning
stopCluster(cl)
print(tuning_results_r)

rm(mod.full,X_train_full,y_train_full)

write.csv(tuning_results_r,file=paste(path,
        "/Outputs/Intermediate/Models/2016_ratio","_",
         Sys.Date(),".csv",sep=""),
          row.names = FALSE)

# Run these lines to upload tuning results previously saved
# tuning_results_r=read.csv(paste(path,
#                               "/Outputs/Intermediate/Models/2016_ratio","_",
#                               "2025-11-04",".csv",sep=""))


best_params_row_r <- tuning_results_r[which.min(tuning_results_r$best_rmse), ]

best_params_r <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = best_params_row_r$max_depth,
  gamma = best_params_row_r$gamma,
  subsample = best_params_row_r$subsample,
  colsample_bytree = best_params_row_r$colsample_bytree
)
best_nrounds_full_r <- best_params_row_r$best_iteration

cat("Best hyperparameters from tuning:\n")
print(best_params_r)
cat("Best nrounds (from tuning):", best_nrounds_full_r, "\n\n")

# -----------------------------
# Step 2: Hyperparameter Tuning to predict income trained on 2019 LFS data for HHs with 
# flag6_income2==0, i.e., with income information
# -----------------------------

# Set up parallel backend using available cores
n_cores <- parallel::detectCores() - 1  # Reserve one core for OS
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Prepare training data (2019)
mod.full=lm(loginc~.,
            data=lfs.don[lfs.don$flag6_income2==0, 
                         c("loginc",covariates)])
X_train_full = model.matrix(mod.full)
y_train_full <- lfs.don[lfs.don$flag6_income2==0,]$loginc


# Parallel grid search using foreach
tuning_results_0 <- foreach(i = 1:nrow(param_grid), 
          .combine = rbind, 
          .packages = "xgboost",
          .export = c("X_train_full", "y_train_full")) %dopar% {
                              
  dtrain_full <- xgb.DMatrix(data = X_train_full, label = y_train_full)
  params <- c(base_params, list(max_depth = param_grid$max_depth[i],
                                gamma = param_grid$gamma[i],
                                subsample = param_grid$subsample[i],
                                colsample_bytree = param_grid$colsample_bytree[i]))
  
  cv_model <- xgb.cv(
    params = params,
    data = dtrain_full,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_iter <- cv_model$best_iteration
  best_rmse <- cv_model$evaluation_log$test_rmse_mean[best_iter]
  
  data.frame(max_depth = param_grid$max_depth[i],
             gamma = param_grid$gamma[i],
             subsample = param_grid$subsample[i],
             colsample_bytree = param_grid$colsample_bytree[i],
             best_rmse = best_rmse,
             best_iteration = best_iter)
}

# Stop the cluster after tuning
stopCluster(cl)
print(tuning_results_0)

rm(mod.full,X_train_full,y_train_full)

write.csv(tuning_results_0,file=paste(path,
                                      "/Outputs/Intermediate/Models/XGB_tuning_2016_inc_0","_",
                                      Sys.Date(),".csv",sep=""),
          row.names = FALSE)

# Run these lines to upload tuning results previously saved
# tuning_results_0=read.csv(paste(path,
#                               "/Outputs/Intermediate/Models/XGB_tuning_2016_inc_0","_",
#                               "2025-11-04",".csv",sep=""))

best_params_row_0 <- tuning_results_0[which.min(tuning_results_0$best_rmse), ]

best_params_0 <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = best_params_row_0$max_depth,
  gamma = best_params_row_0$gamma,
  subsample = best_params_row_0$subsample,
  colsample_bytree = best_params_row_0$colsample_bytree
)
best_nrounds_full_0 <- best_params_row_0$best_iteration

cat("Best hyperparameters from tuning for HHs with income:\n")
print(best_params_0)
cat("Best nrounds for HHs with income (from tuning):", 
    best_nrounds_full_0, "\n\n")


# -----------------------------
# Step 3: Hyperparameter Tuning to predict welfare trained on 2019 LFS data for all HHS
# -----------------------------

# Set up parallel backend using available cores
n_cores <- parallel::detectCores() - 1  # Reserve one core for OS
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Prepare full training data (2019)
mod.full=lm(logwelfare~.,data=lfs.don[, c("logwelfare",covariates)])
X_train_full = model.matrix(mod.full)
y_train_full <- lfs.don$logwelfare


# Parallel grid search using foreach
tuning_results_w <- foreach(i = 1:nrow(param_grid), 
          .combine = rbind, 
          .packages = "xgboost",
          .export = c("X_train_full", "y_train_full")) %dopar% {
                            
  dtrain_full <- xgb.DMatrix(data = X_train_full, label = y_train_full)
  params <- c(base_params,
              list(max_depth = param_grid$max_depth[i],
                   gamma = param_grid$gamma[i],
                   subsample = param_grid$subsample[i],
                   colsample_bytree = param_grid$colsample_bytree[i]))
  
  cv_model <- xgb.cv(
    params = params,
    data = dtrain_full,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_iter <- cv_model$best_iteration
  best_rmse <- cv_model$evaluation_log$test_rmse_mean[best_iter]
  
  data.frame(max_depth = param_grid$max_depth[i],
             gamma = param_grid$gamma[i],
             subsample = param_grid$subsample[i],
             colsample_bytree = param_grid$colsample_bytree[i],
             best_rmse = best_rmse,
             best_iteration = best_iter)
}

rm(mod.full,X_train_full,y_train_full)

write.csv(tuning_results_w,file=paste(path,
                                    "/Outputs/Intermediate/Models/2016","_",
                                    Sys.Date(),".csv",sep=""),
          row.names = FALSE)

# Run these lines to upload tuning results previously saved
# tuning_results_w=read.csv(paste(path,
#                               "/Outputs/Intermediate/Models/2016","_",
#                               "2025-11-03",".csv",sep=""))

# Stop the cluster after tuning
stopCluster(cl)
print(tuning_results_w)

best_params_row_w <- tuning_results_w[which.min(tuning_results_w$best_rmse), ]

best_params_w <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = best_params_row_w$max_depth,
  gamma = best_params_row_w$gamma,
  subsample = best_params_row_w$subsample,
  colsample_bytree = best_params_row_w$colsample_bytree
)
best_nrounds_full_w <- best_params_row_w$best_iteration

cat("Best hyperparameters from tuning:\n")
print(best_params_w)
cat("Best nrounds (from tuning):", best_nrounds_full_w, "\n\n")


# -----------------------------
# Step 4: Predictions and PMM
# -----------------------------


n_sim <- nsim2  # Number of simulations
n.a = 0.9 #Bootstrap resampling parameter
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

df.sim.match=list()

start_time <- Sys.time()  # Start timer


foreach (a = c(0,1)) %do% {  # 0: HHs with income information, 1 otherwise
  if(a == 1) {
    data.don <- lfs.don %>%
      filter(flag6_income2==a) #generate this variable as a filter  in both LFS rounds if income = 1 
    data.rec <- lfs.rec %>%
      filter(flag6_income2==a) 
    best_params=best_params_1
  } else {
    data.don <- lfs.don %>% #generating two objects based on availability of income data
      filter(flag6_income2==a) 
    data.rec <- lfs.rec %>%
      filter(flag6_income2==a)
    best_params=best_params_0
  }
  
  #match
  simcons_match=subset(data.rec,sel=c(hhid))
  
  #Simulation loop
  foreach(sim = 1:n_sim) %do% {
    cat("Simulation ",sim)
    # Bootstrap the training data
    train_sample <- data.don %>%
      group_by(district) %>%
      sample_frac(n.a)
    
    if(a==0) {
      mod.a=lm(logwelfare~.,data=train_sample[, c("logwelfare",covariates)])
      X_train = model.matrix(mod.a)
      y_train <- train_sample$logwelfare
    } else {
      mod.a=lm(logwelfare~.,data=train_sample[, c("logwelfare",covariates)])
      X_train = model.matrix(mod.a)
      y_train <- train_sample$logwelfare           
    }
    dtrain <- xgb.DMatrix(data = X_train, label = y_train)
    
    # Use best_params from tuning; run CV to get optimal nrounds on the bootstrap sample
    cv_model <- xgb.cv(
      params = best_params,
      data = dtrain,
      nrounds = 100,
      nfold = 5,
      early_stopping_rounds = 100,
      verbose = 0
    )
    best_nrounds <- cv_model$best_iteration
    
    # Train model on the bootstrap sample
    model <- xgboost(
      params = best_params,
      data = dtrain,
      nrounds = best_nrounds,
      verbose = 0
    )
    
    # Get predictions for each survey round
    #Donor
    if(a==0){
      mod.a.full=lm(loginc~.,data=data.don[, c("loginc",covariates)]) #we just need the matrix
    } else {
      mod.a.full=lm(logwelfare~.,data=data.don[, c("logwelfare",covariates)]) #we just need the matrix 
    }
    X_don = model.matrix(mod.a.full)
    ddon <- xgb.DMatrix(data = X_don)
    Y.a=predict(model, ddon)
    #Receiver
    mod.b.full=lm(ln_rpcinc_tot~.,data=data.rec[, c("ln_rpcinc_tot",covariates)]) #we just need the matrix
    X_test = model.matrix(mod.b.full)
    dtest <- xgb.DMatrix(data = X_test)
    Y.b=predict(model, dtest)
    
    X.samp.b.pred=data.table(cbind(data.rec[,"hhid"], exp(Y.b)-1))
    X.samp.a.pred=data.table(cbind(data.don[,"hhid"], exp(Y.a)-1))
    rm(Y.b,Y.a)
    colnames(X.samp.b.pred)=c("hhid","ymatch")
    colnames(X.samp.a.pred)=c("hhid","ymatch") 
    #Merge predictions with original base
    samp.btemp=merge.data.table(data.rec,X.samp.b.pred,
                                by="hhid",all=TRUE,sort=TRUE)
    samp.atemp=merge.data.table(data.don,X.samp.a.pred,
                                by="hhid",all=TRUE,sort=TRUE)
    samp.btemp=data.frame(samp.btemp)
    samp.atemp=data.frame(samp.atemp)
    
    if (min(table(data.don$district,data.don$urban))>0){
      group.v <- c("district","urban")  # donation classes
    }  else {
      group.v <- c("district")  # donation classes
    }
    
    #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
    rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                              match.vars=X.mtc2, don.class=group.v,
                              dist.fun="Euclidean",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                            mtc.ids=rnd.2$mtc.ids,
                            z.vars=don.vars2) 
    fA.wrnd$welfare = with(fA.wrnd,ifelse(flag6_income2==0 & ratio_tot<Inf,
                                          ratio_tot*rpcinc_tot, welfare))
    fA.wrnd = fA.wrnd[,c("hhid","welfare")]
    names(fA.wrnd)[2]=paste("welfare_",sim,sep="")
    simcons_match=merge(simcons_match,fA.wrnd,by="hhid")
    rm(samp.atemp,samp.btemp,fA.wrnd,rnd.2)
  }
  
  df.sim.match[[as.character(a)]]=simcons_match
  rm(data.don,data.rec,simcons_match)
  
} #end foreach loop in 'a' (income availability)
end_time <- Sys.time()  # End timer

# Calculate total time taken
time_taken <- end_time - start_time
cat("Total time for parallel simulation loop:", time_taken, "\n")
# Stop the cluster after simulations
stopCluster(cl)

df.match <- do.call(rbind, df.sim.match)
df.pred <- do.call(rbind, df.sim.pred)

#df.match[] <- Map(function(x, y) replace(x, is.infinite(as.numeric(x)), y), df.match, df.pred)


df.match$welfare_mean=apply(df.match[,-1],
                            1,mean,na.rm=TRUE)
df.match$welfare_median=apply(df.match[,-1],
                              1,median,na.rm=TRUE)
df.match$welfare_geom=apply(df.match[,-1],
                            1,geometric_mean,na.rm=TRUE)

# #Option 2: Used predicted welfare for incomes ==0 
# df.pred$welfare_mean=apply(df.pred[,-1],
#                            1,mean,na.rm=TRUE)
# df.pred$welfare_median=apply(df.pred[,-1],
#                              1,median,na.rm=TRUE)
# df.pred$welfare_geom=apply(df.pred[,-1],
#                            1,geometric_mean,na.rm=TRUE)


lfs.imp=merge(lfs.rec,df.match[,c("hhid","welfare_median")],by="hhid",
              all.x=TRUE)

lfs.imp = lfs.imp %>%
  rename(welfare=welfare_median)

gini.16=with(lfs.imp,gini.wtd(welfare,popwt))

gini.16

lfs.imp$pov30 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<3,1,0)
lfs.imp$pov42 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<4.2,1,0)
lfs.imp$pov83 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<8.3,1,0)
lfs.imp$povnpl = ifelse(lfs.imp$welfare<6966,1,0)

svydf <- svydesign(ids = ~hhid, data = lfs.imp, 
                   weights = ~popwt)
svymean(~pov30+pov42+pov83+povnpl, design=svydf,
        na.rm=TRUE,vartype = "ci")
