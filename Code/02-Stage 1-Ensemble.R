# Uses Wasserstein distance to find the simulation that more closely resembles
# the original distribution

#####Define custom functions####
compute_wasserstein_distance <- function(original, predicted_matrix) {
  distances <- apply(predicted_matrix, 2, function(pred) {
    wasserstein1d(original, pred)  # Compute 1D Wasserstein distance
  })
  return(distances)
}
######



#Add state and sector to predictions
simcons_match <- simcons_match %>%
  left_join(data.rec %>% select(hhid, state, urb), by = "hhid")

simcons_pred <- simcons_pred %>%
  left_join(data.rec %>% select(hhid, state, urb), by = "hhid")

# Missing values report
missing_report.match <- simcons_match %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.match,PercentMissing>0)

missing_report.pred <- simcons_pred %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.pred,PercentMissing>0)

# Replace missing values in matching-based imputations with prediction-based
simcons_match <- as.data.frame(mapply(function(x, y) {
  x[is.na(x)] <- y[is.na(x)]
  x
}, simcons_match, simcons_pred, SIMPLIFY = FALSE))

# Create empty lists
original_data <- list()
sim_data_match <- list()
sim_data_pred <- list()
hhid_match <- list()
hhid_pred <- list()

#Double check missing values in data.don
#data.don=na.omit(data.don)

# Group state-sector data using lists
foreach (i = c(1:25, 27:37)) %do% { 
  foreach (a = c(0,1)) %do% {
    cat("State", i, "Sector", a, "\n",sep=" ")
    
    # Original distribution in HCES
    original_data[[paste(i, a, sep = "_")]] <- as.numeric(subset(data.don,
                                 state == i & urb == a)$mpce_sp_def_ind)
    
    # Matching: Extract `hhid` first, then filter numeric variables
    match_filtered <- simcons_match %>%
      filter(state == i & urb == a)  
    
    hhid_match[[paste(i, a, sep = "_")]] <- match_filtered$hhid  # Store `hhid`
    
    pred_matrix_match <- match_filtered %>%  
      select(starts_with("mpce_")) %>%  # Only numeric values
      as.matrix()
    
    sim_data_match[[paste(i, a, sep = "_")]] <- pred_matrix_match
    
    # Predictions: Extract hhid first, then filter numeric variables
    pred_filtered <- simcons_pred %>%
      filter(state == i & urb == a)  
    
    hhid_pred[[paste(i, a, sep = "_")]] <- pred_filtered$hhid  # Store hhid
    
    pred_matrix_pred <- pred_filtered %>%
      select(starts_with("mpce_")) %>%
      as.matrix()
    
    sim_data_pred[[paste(i, a, sep = "_")]] <- pred_matrix_pred
  }
}

# Store predicted vectors for each state-sector
sel_predictions_match <- list()
sel_predictions_pred <- list()
closest_index_match <- list()
closest_index_pred <- list()

# Find the closest simulated distribution for each state-sector
for (key in names(original_data)) {
  cat("State_Sector", key, "\n", sep=" ")
  
  wasserstein_distances_match <- compute_wasserstein_distance(original_data[[key]], 
                                                              sim_data_match[[key]])
  wasserstein_distances_pred <- compute_wasserstein_distance(original_data[[key]], 
                                                             sim_data_pred[[key]])  
  
  # Find the index of the closest simulation
  closest_index_match[[key]] <- which.min(wasserstein_distances_match)
  closest_index_pred[[key]] <- which.min(wasserstein_distances_pred)
  
  # Store the closest simulated vector
  sel_predictions_match[[key]] <- data.frame(
    hhid = hhid_match[[key]],  # Merge with hhid
    mpce_sp_def_ind = sim_data_match[[key]][, closest_index_match[[key]]]
  )
  
  sel_predictions_pred[[key]] <- data.frame(
    hhid = hhid_pred[[key]],  # Merge with hhid
    mpce_sp_def_ind = sim_data_pred[[key]][, closest_index_pred[[key]]]
  )
}

# Concatenate the selected vectors into a single final prediction dataset
final_match_df <- do.call(rbind, sel_predictions_match)  # Merge all into a dataframe
final_pred_df <- do.call(rbind, sel_predictions_pred)  

closest_index_match <- unlist(closest_index_match)
closest_index_pred <- unlist(closest_index_pred)

# Print preview
head(final_match_df)
head(final_pred_df)


#Keep prediction-based imputation
data.rec2=merge(data.rec,final_pred_df,by="hhid",all.x = TRUE)
write_dta(data.rec,paste(datapath,
     "/Data/Stage 1/Final/Imputed_PLFS_22_pred.dta",sep=""))


#Keep matching-based imputation using distributional distance
data.rec2=merge(data.rec,final_match_df,by="hhid",all.x = TRUE)
write_dta(data.rec2,paste(datapath,
      "/Data/Stage 1/Final/Imputed_PLFS_22_match.dta",sep=""))
