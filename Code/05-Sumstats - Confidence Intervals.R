#Generate MI-style confidence (precision) intervals based on survey design and
#imputation uncertainty (Rubin Rules)

library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(dineq)
library(convey)
library(haven)
library(statar)
library(survey)
library(ggplot2)
library(ggh4x) #plot

# clear all
#rm(list = ls())

years    <- 2023
year = 2023
sim      <- 100            # number of simulations (columns 2:(sim+1))

# ---------- helpers ----------
clip01 <- function(p, eps = 1e-8) pmax(pmin(p, 1 - eps), eps)

# Compute survey mean of a binary var and return point + variance
svy_mean_var <- function(varname, design) {
  est <- svymean(as.formula(paste0("~", varname)), design, na.rm = TRUE)
  p   <- as.numeric(est)
  v   <- as.numeric(SE(est))^2
  list(p = p, v = v)
}

# Build poverty indicators for a given MPCE variable name (on base_df), compute p and v
pov_estimates_one_line <- function(base_df, welf_var, cpi, icp, line_value) {
  df <- base_df
  df$welf_use <- df[[welf_var]]
  df <- subset(df, !is.na(welf_use))
  cons_day_ppp <- df$welf_use * (12/365) / cpi / icp
  df$pov <- as.integer(cons_day_ppp < line_value)
  
  svyd <- svydesign(ids = ~hhid, weights = ~popwt, data = df)
  svy_mean_var("pov", svyd)
}

# ---------- parallel over years ----------
cl <- makeCluster(length(years))
registerDoParallel(cl)

mi_ci_results <- foreach(
  year = years, .combine = "rbind",
  .packages = c("survey","haven","dplyr","tidyr","dineq","statar")
) %dopar% {
  
  # --- Load PLFS microdata ---
  lfs.rec <- read_dta(paste0(
    datapath, "cleaned/Imputed Vectors/lfs", year, "_imputed.dta"
  ))
  
  # --- Load simulations (df: hhid + 100 sim columns) ---
  df <- readRDS(file = paste0(datapath,
      "cleaned/Stage 2/Final/Simulations_welf_",year,"_",sim,".rds"
  ))
  
  # expected sim columns
  sim_cols <- (2):(sim + 1)
  stopifnot(ncol(df) >= (sim + 1))
  
  # Per-household median (q50) from simulations
  sim_mat <- as.matrix(df[, sim_cols, drop = FALSE])
  welfare_q50 <- apply(sim_mat, 1, stats::quantile, probs = 0.5, na.rm = TRUE, names = FALSE)
  df_q <- data.frame(hhid = df$hhid, welfare_q50 = welfare_q50)
  
  # Merge to PLFS base
  base_df <- merge(lfs.rec, df_q, by = "hhid", all.x = TRUE)
  
  # CPI/ICP & lines
  cpi  <- cpi21
  icp  <- icp21
  lic  <- 3.0
  lmic <- 4.2
  umic <- 8.3
  
  # 1) Point estimates p50 (design-based on median consumption vector)
  est_lic_50  <- pov_estimates_one_line(base_df, "welfare_q50", cpi, icp, lic)$p
  est_lmic_50 <- pov_estimates_one_line(base_df, "welfare_q50", cpi, icp, lmic)$p
  est_umic_50 <- pov_estimates_one_line(base_df, "welfare_q50", cpi, icp, umic)$p
  
  # 2) For MI-style variance: compute p_s and V_s across simulations for each line
  p_lic  <- numeric(sim); V_lic  <- numeric(sim)
  p_lmic <- numeric(sim); V_lmic <- numeric(sim)
  p_umic <- numeric(sim); V_umic <- numeric(sim)
  
  for (s in 1:sim) {
    # Bind one simulation as mpce_use
    sim_df <- data.frame(hhid = df$hhid, welf_sim = df[[sim_cols[s]]])
    tmp <- merge(lfs.rec, sim_df, by = "hhid", all.x = TRUE)
    tmp <- subset(tmp, !is.na(welf_sim))
    
    cons_day_ppp <- tmp$welf_sim * (12/365) / cpi / icp
    
    # LIC
    tmp$pov <- as.integer(cons_day_ppp < lic)
    svyd <- svydesign(ids = ~hhid, weights = ~popwt, data = tmp)
    est  <- svymean(~pov, svyd, na.rm = TRUE)
    p_lic[s] <- as.numeric(est)
    V_lic[s] <- as.numeric(SE(est))^2
    
    # LMIC
    tmp$pov <- as.integer(cons_day_ppp < lmic)
    est  <- svymean(~pov, svyd, na.rm = TRUE)
    p_lmic[s] <- as.numeric(est)
    V_lmic[s] <- as.numeric(SE(est))^2
    
    # UMIC
    tmp$pov <- as.integer(cons_day_ppp < umic)
    est  <- svymean(~pov, svyd, na.rm = TRUE)
    p_umic[s] <- as.numeric(est)
    V_umic[s] <- as.numeric(SE(est))^2
  }
  
  # 3) Combine uncertainty (MI-style): T = mean(V_s) + (1 + 1/m)*Var(p_s)
  comb_T <- function(p_s, V_s) {
    Wbar <- mean(V_s, na.rm = TRUE)
    B    <- stats::var(p_s, na.rm = TRUE)
    Wbar + (1 + 1/length(p_s)) * B
  }
  
  T_lic  <- comb_T(p_lic,  V_lic)
  T_lmic <- comb_T(p_lmic, V_lmic)
  T_umic <- comb_T(p_umic, V_umic)
  
  # 4) 95% CI centered at p50 on the original proportion scale
  ci_centered_plain <- function(p50, Tvar) {
    se <- sqrt(max(Tvar, 0))
    lo <- p50 - 1.96 * se
    hi <- p50 + 1.96 * se
    c(clip01(lo), clip01(hi))
  }
  
  ci_lic  <- ci_centered_plain(est_lic_50,  T_lic)
  ci_lmic <- ci_centered_plain(est_lmic_50, T_lmic)
  ci_umic <- ci_centered_plain(est_umic_50, T_umic)
  
  # Single tidy row for this year
  data.frame(
    year = year,
    
    lic_p50 = est_lic_50,   lic_lo = ci_lic[1],   lic_hi = ci_lic[2],
    lmic_p50 = est_lmic_50, lmic_lo = ci_lmic[1], lmic_hi = ci_lmic[2],
    umic_p50 = est_umic_50, umic_lo = ci_umic[1], umic_hi = ci_umic[2]
  )
}

stopCluster(cl)

# Save results 
out_file <- paste0(
  path,
  "/Outputs/Main/Tables/CI_",year,".csv"
)
write.csv(mi_ci_results, file = out_file, row.names = FALSE)

# ----------- Faceted plot (free y by facet) -----------
pov_long <- bind_rows(
  mi_ci_results %>%
    transmute(year, line = "LIC",  p50 = lic_p50,  lo = lic_lo,  hi = lic_hi),
  mi_ci_results %>%
    transmute(year, line = "LMIC", p50 = lmic_p50, lo = lmic_lo, hi = lmic_hi),
  mi_ci_results %>%
    transmute(year, line = "UMIC", p50 = umic_p50, lo = umic_lo, hi = umic_hi)
) %>%
  mutate(
    p50 = p50 * 100, lo = lo * 100, hi = hi * 100,
    line = factor(line, levels = c("LIC","LMIC","UMIC"))
  )

#----- plot----------
p_faceted2 <- ggplot(pov_long, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
  geom_line(aes(y = p50), linewidth = 1) +
  geom_point(aes(y = p50), size = 1.5) +
  facet_wrap(~ line, ncol = 2, scales = "free_y") +
  ggh4x::facetted_pos_scales(
    y = list(
      line == "LIC"  ~ scale_y_continuous(limits = c(5, 20),  expand = expansion(mult = 0)),
      line == "LMIC" ~ scale_y_continuous(limits = c(25, 40), expand = expansion(mult = 0)),
      line == "UMIC" ~ scale_y_continuous(limits = c(75, 90), expand = expansion(mult = 0))
    )
  ) +
  scale_x_continuous(breaks = 2017:2021) +
  labs(title = "Median-centered MI-style 95% CIs",
       x = "Year", y = "Poverty rate (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(p_faceted2)

