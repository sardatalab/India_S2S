# #Graphs and tables
# 
# #Bring States names
# states=read_excel(paste0(datapath,
#             "cleaned/Stage 1/Cleaned/2022 matching/states.xlsx"))
# 
# data.rec2=merge(data.rec2,states,by.x="statenum",by.y="state",all.x=TRUE)
# data.don=merge(data.don,states,by.x="statenum",by.y="state",all.x=TRUE)
# #######
# #######

#Poverty calculations

# 2. Subset and add survey identifier
lfs <- data.rec2 %>%
  filter(!is.na(welfare)) %>%
  select(district, urban, welfare, popwt) %>%
  mutate(survey = "LFS")

hies <- data.don %>%
  select(district, urban, welfare, popwt) %>%
  mutate(survey = "HIES")

# 3. Convert any labelled columns to plain numeric.
#    Here we assume welfare and popwt might be labelled:
lfs <- lfs %>%
  mutate(
    welfare = as.numeric(welfare),
    popwt         = as.numeric(popwt)
  )

hies <- hies %>%
  mutate(
    welfare = as.numeric(welfare),
    popwt         = as.numeric(popwt)
  )

# 4. Now you can safely row‐bind
df <- bind_rows(lfs, hies)

df=na.omit(df)

df$pov30 = ifelse(df$welfare*(12/365)/cpi21/icp21<3,1,0)
df$pov42 = ifelse(df$welfare*(12/365)/cpi21/icp21<4.2,1,0)
df$pov83 = ifelse(df$welfare*(12/365)/cpi21/icp21<8.3,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~popwt)

#Tables and graphs

###Figure 8a

#Density of predicted consumption
ggplot(df, aes(x = log(welfare), weight = popwt,
               fill = survey)) +
  geom_density(alpha = 0.4, adjust=1.5) +
  labs(x = "Log Consumption",
       y = "Density",
       title = "Original and Imputed Log Consumption by Survey (2019)")

ggsave(paste(path,
             "/Outputs/Main/Figures/figure 8a.png",sep=""),
       width = 30, height = 20, units = "cm")

####Figure 8b

#ECDF
line300 = log(3.0* (365/12)*icp21*cpi21)
line420 = log(4.2* (365/12)*icp21*cpi21)
line830 = log(8.3* (365/12)*icp21*cpi21)

df_ecdf <- df %>%
  group_by(survey) %>%
  arrange(log_consumption = log(welfare)) %>%
  mutate(cum_weight = cumsum(popwt),
         total_weight = sum(popwt),
         ecdf = cum_weight / total_weight)


ggplot(df_ecdf, aes(x = log(welfare), y = ecdf, color = survey)) +
  geom_step() +
  labs(x = "Log Consumption",
       y = "Density",
       title = "ECDF of Original and Imputed Log Consumption by survey (2022-23)")+
  geom_vline(xintercept = line300,linetype="dashed",size=0.5)+
  geom_vline(xintercept = line420,linetype="dashed",size=0.5)+
  geom_vline(xintercept = line830,linetype="dashed",size=0.5)+
  annotate("text", x = line300, y = 0, label = "3.0", vjust = 1.5,size=1)+
  annotate("text", x = line420, y = 0, label = "4.2", vjust = 1.5,size=1)+
  annotate("text", x = line830, y = 0, label = "8.3", vjust = 1.5,size=1)

ggsave(paste(path,
        "/Outputs/Main/Figures/figure 8b.png",sep=""),
       width = 30, height = 20, units = "cm")


# Table 2

#Gini coefficient
gin1= gini.wtd(df[df$survey=="HIES",]$welfare,
         df[df$survey=="HIES",]$popwt)

gin2 = gini.wtd(df[df$survey=="LFS",]$welfare,
         df[df$survey=="LFS",]$popwt)
tab = data.frame(survey=c("HIES","LFS"),gini=c(gin1,gin2))
write.csv(tab,paste(path,
   "/Outputs/Main/Tables/table 2 gini.csv",sep=""))


#Overall Poverty
tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")

write.csv(tab1,paste(path,
       "/Outputs/Main/Tables/table 2 poverty.csv",sep=""))

#Poverty by area
tab2=svyby(~pov30+pov42+pov83, ~survey+urban, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")
tab2$urban=factor(tab2$urban, levels=c(0,1),labels=c("Rural","Urban"))
tab2 = tab2 %>% rename(Sector=urban)
tab2

means_long <- tab2 %>%
  pivot_longer(
    cols = c(pov30, pov42, pov83),
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
                levels=c("pov30","pov42","pov83"),
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
tab3=svyby(~pov30+pov42+pov83, ~survey+district, design=svydf, 
           svymean,na.rm=TRUE,keep.var=FALSE)

#Reshape table and export
tab3_wide_30 <- tab3 %>%
  select(survey,district,statistic.pov30) %>%
  pivot_wider(names_from = survey, values_from =statistic.pov30)

tab3_wide_30$HCES=100*tab3_wide_30$HCES
tab3_wide_30$PLFS=100*tab3_wide_30$PLFS
tab3_wide_30$Diff=with(tab3_wide_30,PLFS-HCES)
#write.csv(tab3_wide_30,paste(path,
#                              "/Results/2022 matching/state_pov_30_w.csv",sep=""))

#ranking plot
tab3_wide_30 <- tab3_wide_30 %>%
  mutate(
    hces_rank = rank(-HCES, ties.method = "first"),
    plfs_rank = rank(-PLFS, ties.method = "first")
  )

#rank correlation
cat("Rank correlation is: ",cor(tab3_wide_30$hces_rank,tab3_wide_30$plfs_rank))

ggplot(tab3_wide_30, aes(x = hces_rank, y = plfs_rank)) +
  geom_point(size = 3) +
  geom_text(aes(label = district), vjust = -0.5, check_overlap = TRUE) +
  scale_x_continuous(breaks = 1:nrow(tab3_wide_30)) +
  scale_y_continuous(breaks = 1:nrow(tab3_wide_30)) +
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
             "/Results/2022 matching/State Ranking 3.0.png",sep=""),
       width = 20, height = 20, units = "cm")

# 4.2 line
tab3_wide_42 <- tab3 %>%
  select(survey,district,statistic.pov42) %>%
  pivot_wider(names_from = survey, values_from =statistic.pov42)

tab3_wide_42$HCES=100*tab3_wide_42$HCES
tab3_wide_42$PLFS=100*tab3_wide_42$PLFS
tab3_wide_42$Diff=with(tab3_wide_42,PLFS-HCES)

#write.csv(tab3_wide_42,paste(path,
#                             "/Results/2022 matching/state_pov_42_w.csv",sep=""))
#ranking plot
tab3_wide_42 <- tab3_wide_42 %>%
  mutate(
    hces_rank = rank(-HCES, ties.method = "first"),
    plfs_rank = rank(-PLFS, ties.method = "first")
  )

#rank correlation
cat("Rank correlation is: ",cor(tab3_wide_42$hces_rank,tab3_wide_42$plfs_rank))

ggplot(tab3_wide_42, aes(x = hces_rank, y = plfs_rank)) +
  geom_point(size = 3) +
  geom_text(aes(label = district), vjust = -0.5, check_overlap = TRUE) +
  scale_x_continuous(breaks = 1:nrow(tab3_wide_42)) +
  scale_y_continuous(breaks = 1:nrow(tab3_wide_42)) +
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
             "/Results/2022 matching/State Ranking 4.2.png",sep=""),
       width = 20, height = 20, units = "cm")


# #8.3 line
# tab3_wide_83 <- tab3 %>%
#   select(survey,state,statistic.pov83) %>%
#   pivot_wider(names_from = survey, values_from =statistic.pov83)
# 
# tab3_wide_83$HCES=100*tab3_wide_83$HCES
# tab3_wide_83$PLFS=100*tab3_wide_83$PLFS
# tab3_wide_83$Diff=with(tab3_wide_83,PLFS-HCES)
# 
# write.csv(tab3_wide_83,paste(path,
#                              "/Results/2022 matching/state_pov_83_w.csv",sep=""))
# #ranking plot
# tab3_wide_83 <- tab3_wide_83 %>%
#   mutate(
#     hces_rank = rank(-HCES, ties.method = "first"),
#     plfs_rank = rank(-PLFS, ties.method = "first")
#   )
# ggplot(tab3_wide_83, aes(x = hces_rank, y = plfs_rank)) +
#   geom_point(size = 3) +
#   geom_text(aes(label = state), vjust = -0.5, check_overlap = TRUE) +
#   scale_x_continuous(breaks = 1:nrow(tab3_wide_83)) +
#   scale_y_continuous(breaks = 1:nrow(tab3_wide_83)) +
#   geom_abline(slope = 1, intercept = 0, size=1.3,
#               linetype = "dashed", color = "gray") +
#   coord_fixed() +
#   labs(
#     x = "HCES Ranking (1 = Highest Poverty Rate)",
#     y = "PLFS Ranking (1 = Highest Poverty Rate)",
#     title = "State Poverty Rankings ($8.3 PPP21)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text = element_text(size = 8)   # Reduce axis text size
#   )
# ggsave(paste(path,
#              "/Results/2022 matching/State Ranking 8.3.png",sep=""),
#        width = 20, height = 20, units = "cm")



#Barplot
tab3$statistic.pov30=100*tab3$statistic.pov30
ggplot(tab3, aes(y = as.factor(district), x = statistic.pov30, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(y = "State",
       x = "Intl. Poverty Rate at $3.0 2021 PPP (%)",
       fill = "Survey",
       title = "Actual and imputed poverty rates by state") +
  xlim(c(0,40))+
theme_minimal()
ggsave(paste(path,
             "/Results/2022 matching/Poverty State 3.0.png",sep=""),
       width = 30, height = 20, units = "cm")

tab3$statistic.pov42=100*tab3$statistic.pov42
ggplot(tab3, aes(
  y     = as.factor(district),
  x     = statistic.pov42,
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
             "/Results/2022 matching/Poverty State 4.2 v2.png",sep=""),
       width = 30, height = 20, units = "cm")

# ggplot(tab3, aes(y = as.factor(district), x = statistic.pov83, fill = survey)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#   labs(y = "State",
#        x = "Intl. Poverty Rate at $8.3 2021 PPP (%)",
#        fill = "Survey",
#        title = "Actual and imputed poverty rates by state") +
#   xlim(c(0,1))
# theme_minimal()
# ggsave(paste(path,
#              "/Results/2022 matching/Poverty State 8.3.png",sep=""),
#        width = 30, height = 20, units = "cm")
# 



####################
#Density plot of ratio 
#plfs.don is the harmonized PLFS 2022 with all common variables between the
#different rounds of the PLFS
plfs.don=read_dta(paste(path,
               "/Data/PLFS/IND_2022_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta",sep=""))
#We bring the dataset containing the imputed consumption
plfs.imp=read_dta(paste(path,
               "/Results/2022 matching/Imputed_PLFS_22_match_1000_v9.dta",sep=""))
plfs.imp=subset(plfs.imp,select=c(hhid,welfare))
plfs.don=merge(plfs.don,plfs.imp,by="hhid",all.x=TRUE)
rm(plfs.imp)
plfs.don=subset(plfs.don,!is.na(welfare))
#The ratio is calculated by dividing the imputed consumption by the
#previously deflated and temporaly adjusted 
#abbreviated consumption available in the PLFS.
plfs.don$ratio = with(plfs.don,
                      welfare/consumption_pc_adj)
#Ratio Density
plfs.don$quintile=xtile(plfs.don$welfare,n=5,wt=plfs.don$weight)
plfs.don$quintile=as.factor(plfs.don$quintile)

#Ridge plot
ggplot(plfs.don, aes(x = ratio, y = fct_rev(quintile), 
                     weight = weight, fill = quintile)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5, rel_min_height = 0.01) +
  labs(x = "Ratio",
       y = "Quintile",
       title = "Ridgeline Plot of MMRP to Abbreviate Consumption (2022-23)") +
  xlim(c(0, 7.5)) +
  theme_ridges() + 
  theme(legend.position = "none") 
ggsave(paste(path,
             "/Results/PLFS matching/Ridgeplot of ratio by quintile.png",sep=""),
       width = 30, height = 20, units = "cm")



#HEATMAP OF DECILES

plfs.don$decile_mmrp=xtile(plfs.don$welfare,n=10,wt=plfs.don$weight)
plfs.don$decile_abbr=xtile(plfs.don$consumption_pc_adj,n=10,wt=plfs.don$weight)

des <- svydesign(ids = ~1, weights = ~weight, data = plfs.don)

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
             "/Results/Paper/Replies to reviewers/Heatmap deciles.png",sep=""),
       width = 30, height = 20, units = "cm")


