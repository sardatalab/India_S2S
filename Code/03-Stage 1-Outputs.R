#Graphs and tables

####Figure 3####

#2022 HCES official and PLFS abbreviated consumption density plot
plfs=read_dta(paste0(datapath,
     "/Data/Stage 2/Cleaned/IND_2022_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta"))
plfs$survey="PLFS"
plfs = plfs %>%
  mutate(log_consumption_pc_adj = log(consumption_pc_adj),
         weight=weight*hh_size) %>%
  rename(welfare=log_consumption_pc_adj)

hces=read_dta(paste0(datapath,
          "/Data/Stage 1/Cleaned/HCES22_s2s.dta"))
hces$survey="HCES"
hces = hces %>%
  rename(welfare=mpce_sp_def_ind, weight=pop_wgt) %>%
  mutate(welfare=log(welfare))

df=rbind(subset(hces,select=c(survey,welfare,weight)),
         subset(plfs,select=c(survey,welfare,weight)))

ggplot(na.omit(df), aes(x = welfare, weight = weight,
                        fill = survey)) +
  geom_density(alpha = 0.4, adjust=1.5) +
  labs(x = "Log Consumption (2022 prices, spatially adjusted)",
       y = "Density",
       title = "Official (HCES) and Abbreviated (PLFS) Log Consumption Aggregate (2022-23)")

ggsave(paste(path,
             "/Outputs/Main/Figures/Figure 3.png",sep=""),
       width = 20, height = 10, units = "cm")
rm(plfs,hces)

####Figure 4####
years <- c(2017:2022)
for (year in years) {
  plfs.rec=read_dta(paste(datapath,
             "/Data/Stage 2/Cleaned/IND_",year,"_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",sep="")) 
  plfs.rec$log_labor_pc_adj=log(plfs.rec$total_labor_pc_adj+1)
  plfs.rec$log_consumption_pc_adj=log(plfs.rec$consumption_pc_adj+1)
  plfs.rec$pop_wgt=with(plfs.rec,weight*hh_size)
  assign(paste0("data", year), plfs.rec)
}
rm(plfs.rec)
survey_list <- list(
  "2017" = data2017,
  "2018" = data2018,
  "2019" = data2019,
  "2020" = data2020,
  "2021" = data2021,
  "2022" = data2022
  #"2023" = data2023
)

# Define all variables to keep
vars_to_keep <- c(
  "log_consumption_pc_adj", "total_labor_pc_adj", "year", "pop_wgt", "urban",
  "self_income", "total_labor", "hh_sp_educ", "casual_wages", "regular_wages",
  "hh_avg_educ", "hh_sp_educ0", "hh_sh_job_contract", "hh_sh_ls_wrk", 
  "hh_sh_health", "hh_head_lit", "hh_head_job_contract", "hh_sh_occ_7", 
  "hh_sh_occ_1", "hh_sh_occ_5", "flag6_income", "hh_head_occ_5", "hhtype_5", 
  "hh_head_occ_7", "hhtype_2", "hh_sp_educ_ter", "hh_sh_occ_3", 
  "hh_sp_educ_sec", "hh_sh_wrk_unpaid"
)

# Subset each data frame to keep only the desired columns
subset_list <- lapply(survey_list, function(df) {
  df[, intersect(vars_to_keep, names(df)), drop = FALSE]
})

# Append (row-bind) all the data frames into one
combined_data <- do.call(rbind, subset_list)
combined_data$year = fct_rev(as.factor(combined_data$year))

stats_data <- combined_data %>%
  filter(!is.na(pop_wgt)) %>%
  group_by(year, urban) %>%
  summarise(
    median_val = as.numeric(Hmisc::wtd.quantile(
      log_consumption_pc_adj,
      weights = pop_wgt, probs = 0.5
    )),
    .groups = "drop"
  ) %>%
  mutate(
    year_factor = as.factor(year),
    year_numeric = as.numeric(as.factor(year)),
    urban_label = ifelse(urban == 1, "Urban", "Rural") # label for legend
  )

# Ridge plot with median lines colored by urban/rural
ggplot(combined_data, aes(
  x = log_consumption_pc_adj,
  y = year,
  fill = as.factor(year)
)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "black") +
  geom_segment(
    data = stats_data,
    aes(
      x = median_val, xend = median_val,
      y = year_numeric, yend = year_numeric + 1,
      color = urban_label
    ),
    linetype = "dotted",
    size = 0.8
  ) +
  scale_color_manual(
    name = "Sector",
    values = c("Rural" = "#ab7126", "Urban" = "#1E90FF")  # green & blue
  ) +
  scale_fill_viridis_d(guide = "none") +
  labs(
    x = "Log Consumption (abbreviated, 2022 prices, spatially adjusted)",
    y = "Year",
    title = "Log Abbreviated Consumption Density and Median by Sector and Year"
  ) +
  scale_x_continuous(limits = c(6, 10), breaks = seq(6, 10, by = 0.5)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right"
  )
ggsave(paste(path,
         "/Outputs/Main/Figures/Figure 4.png",sep=""),
       width = 25, height = 15, units = "cm")



####Figure 6 and Table A 1####

####Figure 6####
exclude_vars <- c("log_consumption_pc_adj", "total_labor_pc_adj", 
                  "year", "pop_wgt", "urban")

weighted_means_long <- combined_data %>%
  group_by(year) %>%
  summarise(across(
    .cols = setdiff(names(combined_data), exclude_vars),
    .fns = ~ {
      valid <- !is.na(.x) & !is.na(pop_wgt) & pop_wgt > 0
      if (any(valid)) weighted.mean(.x[valid], pop_wgt[valid]) else NA_real_
    },
    .names = "{.col}"
  ))  %>%
  # Convert from wide to long
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "weighted_mean"
  )

ggplot(weighted_means_long, aes(x = as.integer(as.character(year)),
                                y = weighted_mean, 
                                group = 1, color = variable)) +
  geom_line(size = 0.7, show.legend = FALSE) +                # one line per facet, no legend
  facet_wrap(~ variable, ncol = 6, scales = "free_y") +       # 6×4 grid, independent y's
  scale_color_viridis_d(option = "D") +                       # 24 distinct but muted hues
  theme_minimal(base_size = 11) +                             # clean background
  labs(x = "Year", y = NULL) +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold", size = 8, colour = "#444444"),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#333333"),
    axis.text.y      = element_text(colour = "#333333"),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave(paste(path,
             "/Outputs/Main/Figures/Figure 6.png",sep=""),
       width = 25, height = 15, units = "cm")


####Table A 1####

# Pivot so variables are rows and years are columns
weighted_means_wide <- weighted_means_long %>%
  mutate(year = as.integer(as.character(year))) %>%
  pivot_wider(
    names_from = year,
    values_from = weighted_mean,
    names_sort = TRUE
  ) %>%
  arrange(desc(variable))

write.csv(weighted_means_wide,paste(path,
        "/Outputs/Annex/Tables/table A 1.csv",sep=""),
        row.names = FALSE)


####Figure 7####
ggplot(data.frame(r2), aes(x = r2)) +
  geom_density(alpha = 0.4, adjust=1.5) +
  labs(x = "R-squared",
       y = "Density",
       title = "Disribution of R-squared in selected model across simulations")+
  theme_minimal(base_size = 10) 

ggsave(paste(path,
             "/Outputs/Main/Figures/Figure 7.png",sep=""),
       width = 30, height = 20, units = "cm")


####Table 1####
coefs_summary <- t(apply(coefs, 1, function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    p2.5 = quantile(x, 0.025, na.rm = TRUE),
    p97.5 = quantile(x, 0.975, na.rm = TRUE)
  )
}))

write.csv(coefs_summary[-2,],paste(path,
       "/Outputs/Main/Tables/table 1.csv",sep=""),
          row.names = TRUE)

##############################
#####Poverty calculations#####
##############################

# Merge both datasets with states names before producing graphs and tables
data.rec2=merge(data.rec2,states,by.x="statenum",by.y="state",all.x=TRUE)
data.don=merge(data.don,states,by.x="statenum",by.y="state",all.x=TRUE)
#######

# Subset and add survey identifier
plfs <- data.rec2 %>%
  filter(!is.na(mpce_sp_def_ind)) %>%
  select(state_name, urb, mpce_sp_def_ind, pop_wgt) %>%
  mutate(survey = "PLFS")

hces <- data.don %>%
  select(state_name, urb, mpce_sp_def_ind, pop_wgt) %>%
  mutate(survey = "HCES")

# Ensure any labelled columns to plain numeric.
plfs <- plfs %>%
  mutate(
    mpce_sp_def_ind = as.numeric(mpce_sp_def_ind),
    pop_wgt         = as.numeric(pop_wgt)
  )

hces <- hces %>%
  mutate(
    mpce_sp_def_ind = as.numeric(mpce_sp_def_ind),
    pop_wgt         = as.numeric(pop_wgt)
  )

# Append
df <- bind_rows(plfs, hces)

# Remove NAs
df=na.omit(df)

df$povlic = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<lic,1,0)
df$povlmic = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<lmic,1,0)
df$povumic = ifelse(df$mpce_sp_def_ind*(12/365)/cpi21/icp21<umic,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~pop_wgt)

#Tables and graphs

###Figure 8a####

#Density of predicted consumption
ggplot(df, aes(x = log(mpce_sp_def_ind), weight = pop_wgt,
               fill = survey)) +
  geom_density(alpha = 0.4, adjust=1.5) +
  labs(x = "Log Consumption",
       y = "Density",
       title = "Original and Imputed Log Consumption by Survey (2022-23)")

ggsave(paste(path,
             "/Outputs/Main/Figures/figure 8a.png",sep=""),
       width = 30, height = 20, units = "cm")

####Figure 8b####

#ECDF
linelic = log(lic* (365/12)*icp21*cpi21)
linelmic = log(lmic* (365/12)*icp21*cpi21)
lineumic = log(umic* (365/12)*icp21*cpi21)

df_ecdf <- df %>%
  group_by(survey) %>%
  arrange(log_consumption = log(mpce_sp_def_ind)) %>%
  mutate(cum_weight = cumsum(pop_wgt),
         total_weight = sum(pop_wgt),
         ecdf = cum_weight / total_weight)


ggplot(df_ecdf, aes(x = log(mpce_sp_def_ind), y = ecdf, color = survey)) +
  geom_step() +
  labs(x = "Log Consumption",
       y = "Density",
       title = "ECDF of Original and Imputed Log Consumption by survey (2022-23)")+
  geom_vline(xintercept = linelic,linetype="dashed",size=0.5)+
  geom_vline(xintercept = linelmic,linetype="dashed",size=0.5)+
  geom_vline(xintercept = lineumic,linetype="dashed",size=0.5)+
  annotate("text", x = linelic, y = 0, label = "3.0", vjust = 1.5,size=1)+
  annotate("text", x = linelmic, y = 0, label = "4.2", vjust = 1.5,size=1)+
  annotate("text", x = lineumic, y = 0, label = "8.3", vjust = 1.5,size=1)

ggsave(paste(path,
        "/Outputs/Main/Figures/figure 8b.png",sep=""),
       width = 30, height = 20, units = "cm")


### Table 2####

#Gini coefficient
gini1= gini.wtd(df[df$survey=="HCES",]$mpce_sp_def_ind,
         df[df$survey=="HCES",]$pop_wgt)

gini2 = gini.wtd(df[df$survey=="PLFS",]$mpce_sp_def_ind,
         df[df$survey=="PLFS",]$pop_wgt)
tab = data.frame(survey=c("HCES","PLFS"),gini=c(gini1,gini2))
write.csv(tab,paste(path,
   "/Outputs/Main/Tables/table 2 gini.csv",sep=""))


#Overall Poverty
tab1=svyby(~povlic+povlmic+povumic, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")

write.csv(tab1,paste(path,
       "/Outputs/Main/Tables/table 2 poverty.csv",sep=""))

### Figure 10####

#Poverty by area
tab2=svyby(~povlic+povlmic+povumic, ~survey+urb, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")
tab2$urb=factor(tab2$urb, levels=c(0,1),labels=c("Rural","Urban"))
tab2 = tab2 %>% rename(Sector=urb)
tab2

means_long <- tab2 %>%
  pivot_longer(
    cols = c(povlic,povlmic,povumic),
    names_to = "variable",
    values_to = "mean"
  )

# Pivot the lower confidence intervals and clean the variable names
ci_lower_long <- tab2 %>%
  pivot_longer(
    cols = starts_with("ci_l."),
    names_to = "variable",
    values_to = "ci_lower"
  ) %>%
  mutate(variable = sub("ci_l\\.", "", variable))

# Pivot the upper confidence intervals and clean the variable names
ci_upper_long <- tab2 %>%
  pivot_longer(
    cols = starts_with("ci_u."),
    names_to = "variable",
    values_to = "ci_upper"
  ) %>%
  mutate(variable = sub("ci_u\\.", "", variable))

# Merge the long data frames by survey, sector, and variable
plot_data <- means_long %>%
  left_join(ci_lower_long, by = c("survey", "Sector", "variable")) %>%
  left_join(ci_upper_long, by = c("survey", "Sector", "variable"))

# Correct labels in poverty lines
plot_data$variable=factor(plot_data$variable,
                levels=c("povlic","povlmic","povumic"),
                labels=c("$3.0 PPP21","$4.2 PPP21","$8.3 PPP21"))

# Create the bar plot with error bars and facet by variable (rows) and area (columns)
ggplot(plot_data, aes(x = survey, y = mean, fill = survey)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  geom_text(aes(label = percent(mean, accuracy = 0.1), y = mean/2), 
            position = position_dodge(width = 0.7),
            color = "black", size = 3) +
  facet_grid(variable ~ Sector, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Sector",
    y = "Poverty Rate (%)",
    title = "Original and Imputed Poverty Rates (95% Confidence Intervals)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(paste(path,
             "/Outputs/Main/Figures/figure 10.png",sep=""),
       width = 30, height = 20, units = "cm")


##################
#Poverty by State
tab3=svyby(~povlic+povlmic+povumic, ~survey+state_name, design=svydf, 
           svymean,na.rm=TRUE,keep.var=FALSE)

# LIC
tab3_wide_lic <- tab3 %>%
  select(survey,state_name,statistic.povlic) %>%
  pivot_wider(names_from = survey, values_from =statistic.povlic)

tab3_wide_lic$HCES=100*tab3_wide_lic$HCES
tab3_wide_lic$PLFS=100*tab3_wide_lic$PLFS
tab3_wide_lic$Diff=with(tab3_wide_lic,PLFS-HCES)

#save table
write.csv(tab3_wide_lic,paste(path,
       "/Outputs/Intermediate/state_pov_lic_w.csv",sep=""))

#ranking plot
tab3_wide_lic <- tab3_wide_lic %>%
  mutate(
    hces_rank = rank(-HCES, ties.method = "first"),
    plfs_rank = rank(-PLFS, ties.method = "first")
  )

#rank correlation
cat("Rank correlation is: ",cor(tab3_wide_lic$hces_rank,tab3_wide_lic$plfs_rank))

ggplot(tab3_wide_lic, aes(x = hces_rank, y = plfs_rank)) +
  geom_point(size = 3) +
  geom_text(aes(label = state_name), vjust = -0.5, check_overlap = TRUE) +
  scale_x_continuous(breaks = 1:nrow(tab3_wide_lic)) +
  scale_y_continuous(breaks = 1:nrow(tab3_wide_lic)) +
  geom_abline(slope = 1, intercept = 0, size=1.3,
              linetype = "dashed", color = "gray") +
  coord_fixed() +
  labs(
    x = "HCES Ranking (1 = Highest Poverty Rate)",
    y = "PLFS Ranking (1 = Highest Poverty Rate)",
    title = "State Poverty Rankings ($3.0 PPP21)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8)   # Reduce axis text size
  )
ggsave(paste(path,
             "/Outputs/Intermediate/State Ranking lic.png",sep=""),
       width = 20, height = 20, units = "cm")

# LMIC

### Figure B 1####

tab3_wide_lmic <- tab3 %>%
  select(survey,state_name,statistic.povlmic) %>%
  pivot_wider(names_from = survey, values_from =statistic.povlmic)

tab3_wide_lmic$HCES=100*tab3_wide_lmic$HCES
tab3_wide_lmic$PLFS=100*tab3_wide_lmic$PLFS
tab3_wide_lmic$Diff=with(tab3_wide_lmic,PLFS-HCES)

write.csv(tab3_wide_lmic,paste(path,
                "/Outputs/Intermediate/state_pov_lmic_w.csv",sep=""))
#ranking plot
tab3_wide_lmic <- tab3_wide_lmic %>%
  mutate(
    hces_rank = rank(-HCES, ties.method = "first"),
    plfs_rank = rank(-PLFS, ties.method = "first")
  )

#rank correlation
cat("Rank correlation is: ",cor(tab3_wide_lmic$hces_rank,tab3_wide_lmic$plfs_rank))

ggplot(tab3_wide_lmic, aes(x = hces_rank, y = plfs_rank)) +
  geom_point(size = 3) +
  geom_text(aes(label = state_name), vjust = -0.5, check_overlap = TRUE) +
  scale_x_continuous(breaks = 1:nrow(tab3_wide_lmic)) +
  scale_y_continuous(breaks = 1:nrow(tab3_wide_lmic)) +
  geom_abline(slope = 1, intercept = 0, size=1.3,
              linetype = "dashed", color = "gray") +
  coord_fixed() +
  labs(
    x = "HCES Ranking (1 = Highest Poverty Rate)",
    y = "PLFS Ranking (1 = Highest Poverty Rate)",
    title = "State Poverty Rankings ($4.2 PPP21)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = .5)   # Reduce axis text size
  )
ggsave(paste(path,
             "/Outputs/Main/Figures/Figure B1 panel B.png",sep=""),
       width = 20, height = 20, units = "cm")

#Barplots

# LIC
tab3$statistic.povlic=100*tab3$statistic.povlic
ggplot(tab3, aes(y = as.factor(state_name), x = statistic.povlic, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(y = "State",
       x = "Intl. Poverty Rate at $3.0 2021 PPP (%)",
       fill = "Survey",
       title = "Actual and imputed poverty rates by state") +
  xlim(c(0,40))+
theme_minimal()
ggsave(paste(path,
             "/Outputs/Intermediate/Poverty State lic.png",sep=""),
       width = 30, height = 20, units = "cm")

# LMIC
tab3$statistic.povlmic=100*tab3$statistic.povlmic
ggplot(tab3, aes(
  y     = as.factor(state_name),
  x     = statistic.povlmic,
  fill  = survey
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    y     = "State",
    x     = "Intl. Poverty Rate at \n$4.2 (2021 PPP) (%)",
    fill  = "Survey",
    title = "Actual and Imputed Poverty Rates by State"
  ) +
  xlim(c(0,75))+
  theme_minimal()
ggsave(paste(path,
             "/Outputs/Main/Figures/Figure B1 panel A.png",sep=""),
       width = 30, height = 20, units = "cm")


####################
#Density plot of ratio 
#plfs.don is here the harmonized PLFS 2022 with all common variables between the
#different rounds of the PLFS to be used in stage 2
plfs.don=read_dta(paste(datapath,
      "/Data/Stage 2/Cleaned/IND_2022_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",sep=""))
#We bring the dataset containing the imputed consumption
plfs.imp=read_dta(paste(datapath,
          "/Data/Stage 1/Final/Imputed_PLFS_22_match.dta",sep=""))
plfs.imp=subset(plfs.imp,select=c(hhid,mpce_sp_def_ind))
plfs.don=merge(plfs.don,plfs.imp,by="hhid",all.x=TRUE)
rm(plfs.imp)
plfs.don=subset(plfs.don,!is.na(mpce_sp_def_ind))
#The ratio is calculated by dividing the imputed consumption by the
#previously deflated and temporaly adjusted 
#abbreviated consumption available in the PLFS.
plfs.don$ratio = with(plfs.don,
                      mpce_sp_def_ind/consumption_pc_adj)

### Figure 12####

#Ridge plot

#Quintiles of imputed consumption
plfs.don$quintile=xtile(plfs.don$mpce_sp_def_ind,n=5,wt=plfs.don$weight)
plfs.don$quintile=as.factor(plfs.don$quintile)
plfs.don$pop_wgt=with(plfs.don,hh_size * weight)

ggplot(plfs.don, aes(x = ratio, y = fct_rev(quintile), 
                     weight = pop_wgt, fill = quintile)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5, rel_min_height = 0.01) +
  labs(x = "Ratio",
       y = "Quintile",
       title = "Ridgeline Plot of MMRP to Abbreviate Consumption (2022-23)") +
  xlim(c(0, 7.5)) +
  theme_ridges() + 
  theme(legend.position = "none") 
ggsave(paste(path,
    "/Outputs/Main/Figures/Figure 12.png",sep=""),
       width = 30, height = 20, units = "cm")


#### Figure B4####

#HEATMAP OF DECILES

plfs.don$decile_mmrp=xtile(plfs.don$mpce_sp_def_ind,n=10,wt=plfs.don$pop_wgt)
plfs.don$decile_abbr=xtile(plfs.don$consumption_pc_adj,n=10,wt=plfs.don$pop_wgt)

des <- svydesign(ids = ~1, weights = ~pop_wgt, data = plfs.don)

#Cross-tabulate weighted counts
tab <- svytable(~decile_mmrp + decile_abbr, design = des)
heatmap_data <- as.data.frame(tab)

heatmap_data <- heatmap_data %>%
  mutate(rel_freq = Freq / sum(Freq))

#Plot heatmap
ggplot(heatmap_data, aes(x = decile_mmrp, y = decile_abbr, fill = rel_freq)) +
  geom_tile(color = "white") +
  #geom_text(aes(label = percent(rel_freq, accuracy = .1)), color = "white", size = 3) +
  scale_fill_viridis_c(
    option = "mako",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Cross-Decile Heatmap",
    x = "Deciles of imputed consumption",
    y = "Deciles of abbreviated consumption",
    fill = "Proportion of population"
  ) +
  theme_minimal()
ggsave(paste(path,
        "/Outputs/Annex/Figures/Figure B4.png",sep=""),
       width = 30, height = 20, units = "cm")


#### Figure 9####

plfs = plfs.don %>%
  mutate(welfare = log(mpce_sp_def_ind),
         weight=pop_wgt,
         survey="PLFS",
         ventile=xtile(welfare,n=20,wt=weight)) 

hces=read_dta(paste0(datapath,
             "/Data/Stage 1/Cleaned/HCES22_s2s.dta"))
hces$survey="HCES"
hces = hces %>%
  rename(welfare=mpce_sp_def_ind, weight=pop_wgt) %>%
  mutate(welfare=log(welfare),
         ventile=xtile(welfare,n=20,wt=weight))

df=rbind(subset(hces,select=c(survey,welfare,weight,ventile)),
         subset(plfs,select=c(survey,welfare,weight,ventile)))

# Dumbell plot

# 1) Weighted mean welfare by ventile & survey
avg_by_ventile <- df %>%
  group_by(survey, ventile) %>%
  summarise(mean_welfare = weighted.mean(welfare, weight, na.rm = TRUE),
            .groups = "drop")

# 2) Wide form and difference (HCES − PLFS)
wide_ventiles <- avg_by_ventile %>%
  pivot_wider(names_from = survey, values_from = mean_welfare) %>%
  mutate(diff = HCES - PLFS,
         diff_abs = abs(diff),
         xmid = (PLFS + HCES)/2)

#plot
ggplot(wide_ventiles, aes(y = ventile)) +
  # connecting line
  geom_segment(aes(x = PLFS, xend = HCES, yend = ventile),
               linewidth = 1, alpha = 0.6, color = "gray60") +
  # PLFS points
  geom_point(aes(x = PLFS, color = "PLFS"), size = 3) +
  # HCES points
  geom_point(aes(x = HCES, color = "HCES"), size = 3) +
  # label with differences
  geom_text(aes(x = xmid, label = sprintf("Δ=%.3f", diff)),
            vjust = -0.6, size = 3) +
  # survey colors
  scale_color_manual(values = c("PLFS" = "cyan", "HCES" = "salmon"),
                     name = "Survey") +
  scale_y_continuous(breaks = 1:20) +
  labs(
    title = "Average Log Consumption by Ventile: HCES (official) vs PLFS (imputed)",
    x = "Average Log Consumption", y = "Ventile"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(paste(path,
    "/Outputs/Main/Figures/Figure 9.png",sep=""),
       width = 25, height = 15, units = "cm")

