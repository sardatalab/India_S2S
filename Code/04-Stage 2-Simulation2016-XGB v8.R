# Sames as v6, but training data changed for HHs w/o income (now HIES)
# Different strategies: First: hot deck random matching between
# LFS 2019 (donor, trained on HHs with income), and LFS 2016
# (receiver, HHS with income) using prcinc_tot, hhsize, and hhb_year 
# to find nearest neighbor
# Second model: Parallelized Hyperparameter Tuning Loop plus model to 
# predict welfare via PMM between HIES 2019
# (donor, trained on HHs w/o income) and 2016 (receiver, HHS w/o income)
# using ymatch, prcinc_tot, hhsize, and hhb_year to find nearest neighbor
# Best version as of 11-19-25 at 16:00 pm

# -----------------------------
# Inputs:
# -----------------------------
# training Data for HHs with income: LFS 2019
#lfs.don <- read_dta(paste(datapath,
#        "cleaned/Stage 1/Final/Imputed_LFS_19_final_at_least_for_now.dta.dta",
#                          sep="")) 
#lfs.don$ratio_tot=with(lfs.don,welfare/rpcinc_tot)
lfs.don$logwelfare=log(lfs.don$welfare)
lfs.don$district=as.factor(lfs.don$district)
#lfs.don$ratio_tot=ifelse(lfs.don$ratio_tot<Inf,lfs.don$ratio_tot,NA)

# Missing values report
missing_report.don <- lfs.don %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)

# training Data for HHs w/o income: LFS 2019

hies.don=read_dta(paste(datapath,"cleaned/hies2019_clean.dta",sep="")) 
hies.don$hhb_year=2019-hies.don$age_hhh
hies.don$district=as.factor(hies.don$district)
hies.don$logwelfare=log(hies.don$welfare)
missing_report.don.hies <- hies.don %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don.hies,PercentMissing>0)
hies.don$flag6_income2=with(hies.don,ifelse(rpcinc1>0,0,1))

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

vars1=setdiff(names(hies.don),names(lfs.rec))
#vars1=setdiff(vars1,c("welfare","ratio_tot"))
vars2=setdiff(names(lfs.rec),names(hies.don))

var2excl=c(vars1,vars2,"hhid","psu","weight",
           "rpcinc1","rpcwage1","rpcself1","age_hhh",
           "flag6_income","flag6_income2",
           "public_emp_hhh","private_emp_hhh",
           "popwt","ln_rpcinc1","ln_rpcwage1","ln_rpcself1")
covariates=setdiff(names(hies.don),var2excl)

set.seed(1729)  # For reproducibility

# -----------------------------
# Step 1: Random hot deck matching based on prcinc_tot, hhsize, and hhb_year
# -----------------------------

#n_sim <- nsim2  # Number of simulations
n.a = 0.8 #Bootstrap resampling parameter
don.vars2.0=c("ratio_tot","share_19") #variables to be imputed

start_time <- Sys.time()  # Start timer
#match
simcons_match=subset(lfs.rec,flag6_income2==0,sel=c(hhid))
simcons_match_i=subset(lfs.rec,flag6_income2==0,sel=c(hhid))

foreach(sim = 1:nsim2) %do% {
    cat("Simulation for HHS with labor income: ",sim, "\n")
    # Bootstrap the training data
    train_sample <- lfs.don %>%
        filter(flag6_income2==0) %>%
        group_by(district) %>%
        sample_frac(n.a)
    
    if (min(table(train_sample$district,
                  train_sample$urban))>0){
        group.v <- c("district","urban")  # donation classes
    }  else {
        group.v <- c("district")  # donation classes
    }
    
    samp.atemp=as.data.frame(train_sample)
    samp.btemp=as.data.frame(lfs.rec[lfs.rec$flag6_income2==0,])

    #Matching using rpcinc_tot and random nearest neighbor distance hot deck (D'Orazio, 2017)
    rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                              match.vars=X.mtc2.0, don.class=group.v,
                              dist.fun="Mahalanobis",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                            mtc.ids=rnd.2$mtc.ids,
                            z.vars=don.vars2.0) 
    fA.wrnd$welfare = with(fA.wrnd, ratio_tot*rpcinc_tot)
    fA.wrnd$y_nl = with(fA.wrnd, share_19*rpcinc_tot)
    fA.wrnd.c = fA.wrnd[,c("hhid","welfare")]
    fA.wrnd.i = fA.wrnd[,c("hhid","y_nl")]
    names(fA.wrnd.c)[2]=paste("welfare_",sim,sep="")
    names(fA.wrnd.i)[2]=paste("y_nl_",sim,sep="")
    simcons_match=merge(simcons_match,fA.wrnd.c,by="hhid")
    simcons_match_i=merge(simcons_match_i,fA.wrnd.i,by="hhid")
    rm(samp.atemp,samp.btemp,fA.wrnd.c,fA.wrnd.i,rnd.2)
}

df.match.0=simcons_match
df.match.0.i=simcons_match_i

df.match.0$welfare_median=apply(df.match.0[,-1],
                              1,median,na.rm=TRUE)
df.match.0.i$y_nl_median=apply(df.match.0.i[,-1],
                                1,median,na.rm=TRUE)

lfs.imp.0=merge(lfs.rec[lfs.rec$flag6_income2==0,],
                df.match.0[,c("hhid","welfare_median")],by="hhid",
                all.x=TRUE)
lfs.imp.0=merge(lfs.imp.0,
                df.match.0.i[,c("hhid","y_nl_median")],by="hhid",
                all.x=TRUE)

lfs.imp.0 = lfs.imp.0 %>%
    rename(welfare=welfare_median,y_nl=y_nl_median) %>%
    mutate(logwelfare=log(welfare))

# # -----------------------------
# # Step 2: Hyperparameter Tuning on 2019 HIES data for HHs without income
# # without income information
# # Run only once to find optimal parameters
# # -----------------------------

# # Set up parallel backend using available cores
# n_cores <- parallel::detectCores() - 1  # Reserve one core for OS
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
# 
# max_depth_values <- c(3, 5, 7)
# gamma_values <- c(0, 0.1, 0.3)
# subsample_values <- c(0.7, 0.8, 0.9)
# colsample_bytree_values <- c(0.6, 0.8, 1.0)
# 
# # Create a grid of all parameter combinations
# param_grid <- expand.grid(max_depth = max_depth_values,
#                           gamma = gamma_values,
#                           subsample = subsample_values,
#                           colsample_bytree = colsample_bytree_values)
# 
# # Base parameters (others will be added from the grid)
# base_params <- list(
#     objective = "reg:squarederror",
#     eval_metric = "rmse",
#     eta = 0.1,
#     nthread = n_cores
# )
# 
# # Prepare training data (2019), HHs w/o income
# mod.full=lm(logwelfare~.,
#             data=hies.don[hies.don$flag6_income2==1,
#                          c("logwelfare",covariates)])
# X_train_full = model.matrix(mod.full)
# y_train_full <- hies.don[hies.don$flag6_income2==1,]$logwelfare
# 
# 
# # Parallel grid search using foreach
# tuning_results_1 <- foreach(i = 1:nrow(param_grid),
#         .combine = rbind,
#         .packages = "xgboost",
#         .export = c("X_train_full", "y_train_full")) %dopar% {
# 
#     dtrain_full <- xgb.DMatrix(data = X_train_full, label = y_train_full)
#     params <- c(base_params, list(max_depth = param_grid$max_depth[i],
#                                   gamma = param_grid$gamma[i],
#                                   subsample = param_grid$subsample[i],
#                                   colsample_bytree = param_grid$colsample_bytree[i]))
# 
#     cv_model <- xgb.cv(
#         params = params,
#         data = dtrain_full,
#         nrounds = 100,
#         nfold = 5,
#         early_stopping_rounds = 10,
#         verbose = 0
#     )
# 
#     best_iter <- cv_model$best_iteration
#     best_rmse <- cv_model$evaluation_log$test_rmse_mean[best_iter]
# 
#     data.frame(max_depth = param_grid$max_depth[i],
#                gamma = param_grid$gamma[i],
#                subsample = param_grid$subsample[i],
#                colsample_bytree = param_grid$colsample_bytree[i],
#                best_rmse = best_rmse,
#                best_iteration = best_iter)
# }
# # Stop the cluster after tuning
# stopCluster(cl)
# print(tuning_results_1)
# rm(mod.full,X_train_full,y_train_full)
#  
#  write.csv(tuning_results_1,file=paste(path,
#                     "/Outputs/Intermediate/Models/XGB_tuning_hies_2019_1_v8","_",
#                     Sys.Date(),".csv",sep=""),
#            row.names = FALSE)

# Run these lines to load tuning results previously saved
tuning_results_1=read.csv(paste(path,
               "/Outputs/Intermediate/Models/XGB_tuning_hies_2019_1_v8","_",
               "2025-11-12",".csv",sep=""))

best_params_row_1 <- tuning_results_1[which.min(tuning_results_1$best_rmse), ]

best_params_1 <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,
    max_depth = best_params_row_1$max_depth,
    gamma = best_params_row_1$gamma,
    subsample = best_params_row_1$subsample,
    colsample_bytree = best_params_row_1$colsample_bytree
)
best_nrounds_full_1 <- best_params_row_1$best_iteration

cat("Best hyperparameters from tuning for HHs with income:\n")
print(best_params_1)


# -----------------------------
# Step 3: Predictions via PMM in 2016 (HHs w/o income)
# -----------------------------

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

don.vars2.1=c("welfare","rnlincpc19") #variables to be imputed

#match
simcons_match=subset(lfs.rec,flag6_income2==1,sel=c(hhid))
simcons_match_i=subset(lfs.rec,flag6_income2==1,sel=c(hhid))

foreach(sim = 1:nsim2) %do% {
    cat("Simulation for HHS w/o labor income: ",sim, "\n")
    # Bootstrap the training data
    train_sample <- hies.don %>%   
        filter(flag6_income2==1) %>%
        group_by(district) %>%
        sample_frac(n.a)
    
    mod.a=lm(logwelfare~.,data=train_sample[, c("logwelfare",covariates)])
    X_train = model.matrix(mod.a)
    y_train <- train_sample$logwelfare
    dtrain <- xgb.DMatrix(data = X_train, label = y_train)
    
    # Use best_params from tuning; run CV to get optimal nrounds on the bootstrap sample
    cv_model <- xgb.cv(
        params = best_params_1,
        data = dtrain,
        nrounds = 100,
        nfold = 5,
        early_stopping_rounds = 100,
        verbose = 0
    )
    best_nrounds <- cv_model$best_iteration
    
    # Train model on the bootstrap sample
    model <- xgboost(
        params = best_params_1,
        data = dtrain,
        nrounds = best_nrounds,
        verbose = 0
    )
    
    # Get predictions for each survey round
    #Donor
    mod.a.full=lm(logwelfare~.,data=hies.don[hies.don$flag6_income2==1,
                                            c("logwelfare",covariates)])
    X_don = model.matrix(mod.a.full)
    ddon <- xgb.DMatrix(data = X_don)
    Y.a=predict(model, ddon)
    #Receiver
    mod.b.full=lm(ln_rpcinc_tot~.,data=lfs.rec[lfs.rec$flag6_income2==1,
                                               c("ln_rpcinc_tot",covariates)])
    X_test = model.matrix(mod.b.full)
    dtest <- xgb.DMatrix(data = X_test)
    Y.b=predict(model, dtest)
    
    X.samp.b.pred=data.table(
        hhid = lfs.rec[lfs.rec$flag6_income2==1, "hhid"],
        ymatch = exp(Y.b)) 
    X.samp.a.pred = data.table(
        hhid = hies.don[hies.don$flag6_income2==1, "hhid"],
        ymatch = exp(Y.a))
    
    rm(Y.b,Y.a)

    colnames(X.samp.b.pred)=c("hhid","ymatch")
    colnames(X.samp.a.pred)=c("hhid","ymatch")
    
    #Merge predictions with original base
    samp.btemp=merge.data.table(lfs.rec[lfs.rec$flag6_income2==1,],
                                X.samp.b.pred,
                                by="hhid",all=TRUE,sort=TRUE)
    samp.atemp=merge.data.table(hies.don[hies.don$flag6_income2==1,],
                                X.samp.a.pred,
                                by="hhid",all=TRUE,sort=TRUE)
    samp.btemp=data.frame(samp.btemp)
    samp.atemp=data.frame(samp.atemp)

    
    
    if (min(table(hies.don[hies.don$flag6_income2==1,]$district,
                  hies.don[hies.don$flag6_income2==1,]$urban))>0){
        group.v <- c("district","urban")  # donation classes
    }  else {
        group.v <- c("district")  # donation classes
    }
    
    #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
    rnd.2 <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                              match.vars=X.mtc2.1, don.class=group.v,
                              dist.fun="Euclidean",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                            mtc.ids=rnd.2$mtc.ids,
                            z.vars=don.vars2.1) 
    fA.wrnd.c = fA.wrnd[,c("hhid","welfare")]
    fA.wrnd.i = fA.wrnd[,c("hhid","rnlincpc19")]
    names(fA.wrnd.c)[2]=paste("welfare_",sim,sep="")
    names(fA.wrnd.i)[2]=paste("y_nl_",sim,sep="")
    simcons_match=merge(simcons_match,fA.wrnd.c,by="hhid")
    simcons_match_i=merge(simcons_match_i,fA.wrnd.i,by="hhid")
    rm(samp.atemp,samp.btemp,fA.wrnd.c,fA.wrnd.i,rnd.2)
}
# Stop the cluster after simulations
stopCluster(cl)
end_time <- Sys.time()  # End timer

# Calculate total time taken
time_taken <- end_time - start_time
cat("Total time for parallel simulation loop:", time_taken, "\n")

df.match.1=simcons_match
df.match.1.i=simcons_match_i

df.match.1$welfare_median=apply(df.match.1[,-1],
                                1,median,na.rm=TRUE)
df.match.1.i$y_nl_median=apply(df.match.1.i[,-1],
                             1,median,na.rm=TRUE)

lfs.imp.1=merge(lfs.rec[lfs.rec$flag6_income2==1,],
                df.match.1[,c("hhid","welfare_median")],by="hhid",
                all.x=TRUE)
lfs.imp.1=merge(lfs.imp.1,
                df.match.1.i[,c("hhid","y_nl_median")],by="hhid",
                all.x=TRUE)

lfs.imp.1 = lfs.imp.1 %>%
  rename(welfare=welfare_median,y_nl=y_nl_median) %>%
  mutate(logwelfare=log(welfare))


# Adjustment for imputed welfare, only for HHS w\o income
# Adjustment factor using real growth of
# Households and NPISHs final consumption expenditure (constant 2015 US$)
# WDI: NE.CON.PRVT.KD
# Factor for 2016= 0.925230578. For 2023=0.960698836
adj_f=0.925230578
lfs.imp.1$welfare=lfs.imp.1$welfare*adj_f
lfs.imp.1$y_nl=lfs.imp.1$y_nl*adj_f

lfs.imp=bind_rows(lfs.imp.0,lfs.imp.1)

gini.16=with(lfs.imp,gini.wtd(welfare,popwt))

gini.16

lfs.imp$pov30 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<3,1,0)
lfs.imp$pov42 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<4.2,1,0)
lfs.imp$pov83 = ifelse(lfs.imp$welfare*(12/365)/cpi21/icp21<8.3,1,0)
lfs.imp$povnpl = ifelse(lfs.imp$welfare<6966,1,0)

svydf <- svydesign(ids = ~hhid, data = lfs.imp, 
                   weights = ~popwt)
tab1=svymean(~pov30+pov42+pov83+povnpl, design=svydf,
        na.rm=TRUE,vartype = "ci")
tab1

#write.csv(tab1,file=paste(path,
#       "/Outputs/Main/Tables/Poverty_imputed_2016.csv",sep=""),
#      row.names = FALSE)

#write_dta(lfs.imp,paste(datapath,
#       "/lfs2016_imputed_final_so_far.dta",sep=""))


#Save simulations

df.match.0$welfare_median=NULL
df.match.0.i$y_nl_median=NULL
df.match.1$welfare_median=NULL
df.match.1.i$y_nl_median=NULL

df.match=bind_rows(df.match.0,df.match.1)
df.match.i=  bind_rows(df.match.0.i,df.match.1.i)

write.csv(df.match,file=paste(datapath,
                              "cleaned/Stage 2/Final/Simulations_welf_2016_",sim,".csv",sep=""),
          row.names = FALSE)
saveRDS(df.match,file=paste(datapath,
                            "cleaned/Stage 2/Final/Simulations_welf_2016_",sim,".rds",sep=""))


write.csv(df.match.i,file=paste(datapath,
                                "cleaned/Stage 2/Final/Simulations_ynl_2016_",sim,".csv",sep=""),
          row.names = FALSE)
saveRDS(df.match.i,file=paste(datapath,
                              "cleaned/Stage 2/Final/Simulations_ynl_2016_",sim,".rds",sep=""))
