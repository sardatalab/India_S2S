#lfs 2019
lfs19=lfs.don
lfs19$hhid=NULL
lfs19=subset(lfs19,select=c(urban,popwt,welfare))
lfs19$survey="LFS_19_imp"
lfs19$urban=factor(lfs19$urban, levels=c(0,1),labels=c("Rural","Urban"))

#HIES 2019
hies19 = subset(hies.don,select=c(urban,popwt,welfare))
hies19$survey="HIES_19"
hies19$urban=factor(hies19$urban, levels=c(0,1),labels=c("Rural","Urban"))

#lfs 2016
lfs16.orig=read_dta(paste(datapath,
                          "/lfs2016_imputed_final_so_far.dta",
                       sep="")) 
lfs16.orig=subset(lfs16.orig,select=c(urban,popwt,welfare))
lfs16.imp=lfs16.orig
lfs16.imp$survey="LFS_16_imp"
lfs16.imp$urban=factor(lfs16.imp$urban, levels=c(0,1),labels=c("Rural","Urban"))

#hies 2016
hies16=read_dta(paste(datapath,"hies16ppp.dta",sep=""))
hies16$survey="HIES_16"
hies16$district=NULL
hies16=hies16 %>%
    rename(popwt=weight) %>%
    mutate(urban=factor(urban, levels=c(0,1),labels=c("Rural","Urban")))

#lfs 2023
lfs23.orig=read_dta(paste(datapath,
                          "lfs2023_imputed_final_so_far.dta",
                          sep="")) 
lfs23.orig=subset(lfs23.orig,select=c(urban,popwt,welfare))
lfs23.imp=lfs23.orig
lfs23.imp$survey="LFS_23_imp"
lfs23.imp$urban=factor(lfs23.imp$urban, levels=c(0,1),labels=c("Rural","Urban"))
###APPEND THREE ROUNDS 

lfs.all=bind_rows(lfs19,hies19,lfs16.imp,lfs23.imp)
lfs.all$welfare=lfs.all$welfare*(12/365)/cpi21/icp21 #convert to 2021 PPP

df=bind_rows(lfs.all,hies16)
df=na.omit(df)

df$pov30 = ifelse(df$welfare<3,1,0)
df$pov42 = ifelse(df$welfare<4.2,1,0)
df$pov83 = ifelse(df$welfare<8.3,1,0)

#Set as survey
svydf <- svydesign(ids = ~1, data = df, 
                   weights = ~popwt)

tab1=svyby(~pov30+pov42+pov83, ~survey, design=svydf, svymean,
           na.rm=TRUE,vartype = "ci")

#Poverty by sector
tab2=svyby(~pov30+pov42+pov83, ~survey+urban, design=svydf, 
           svymean,na.rm=TRUE,vartype = "ci")

#Poverty by sector
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
    left_join(ci_lower_long, by = c("survey", "urban", "variable")) %>%
    left_join(ci_upper_long, by = c("survey", "urban", "variable"))

# Correct labels in poverty lines
plot_data$variable=factor(plot_data$variable,
                          levels=c("pov30","pov42","pov83"),
                          labels=c("$3.0 PPP21","$4.2 PPP21","$8.3 PPP21"))

plot_data$survey=factor(plot_data$survey,
      levels=c("HIES_16","LFS_16_imp","HIES_19","LFS_19_imp","LFS_23_imp"),
      labels=c("HIES_16","LFS_16_imp","HIES_19","LFS_19_imp","LFS_23_imp"))


# Create the bar plot with error bars and facet by variable (rows) and area (columns)
ggplot(plot_data, aes(x = survey, y = mean, fill = survey)) +
    geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
   # geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
   #                  width = 0.2, 
   #                  position = position_dodge(width = 0.7)) +
    geom_text(aes(label = percent(mean, accuracy = 0.1), y = mean/2), 
              position = position_dodge(width = 0.7),
              color = "black", size = 3) +
    facet_grid(variable ~ urban, scales = "free_y") +
    scale_y_continuous(labels = percent) +
    labs(
        x = "Sector",
        y = "Poverty Rate (%)",
        title = "Original and Imputed Poverty Rates"
    ) +
    theme_minimal() +
    theme(legend.position = "none")



#ECDF
line300 = log(3.0* (365/12)*icp21*cpi21)
line420 = log(4.2* (365/12)*icp21*cpi21)
line830 = log(8.3* (365/12)*icp21*cpi21)

df_ecdf <- lfs.all %>%
    group_by(survey) %>%
    arrange(log_consumption = log(welfare)) %>%
    mutate(cum_weight = cumsum(popwt),
           total_weight = sum(popwt),
           ecdf = cum_weight / total_weight)


ggplot(df_ecdf, aes(x = log(welfare), y = ecdf, color = survey)) +
    geom_step() +
    labs(x = "Log Consumption",
         y = "Density",
         title = "ECDF of Imputed Log Consumption by survey (2016-23)")+
    geom_vline(xintercept = line300,linetype="dashed",size=0.5)+
    geom_vline(xintercept = line420,linetype="dashed",size=0.5)+
    geom_vline(xintercept = line830,linetype="dashed",size=0.5)+
    annotate("text", x = line300, y = 0, label = "3.0", vjust = 1.5,size=1)+
    annotate("text", x = line420, y = 0, label = "4.2", vjust = 1.5,size=1)+
    annotate("text", x = line830, y = 0, label = "8.3", vjust = 1.5,size=1)

ggsave(paste(path,
             "/Outputs/Main/Figures/ecdf_all.png",sep=""),
       width = 30, height = 20, units = "cm")



###################################
hies16=read_dta(paste(datapath,"hies16ppp.dta",sep=""))
hies16$survey="HIES_16"
hies16$district=NULL
hies16=hies16 %>%
    rename(popwt=weight)

lfs16.imp$welfare=with(lfs16.imp,welfare*(12/365)/cpi21/icp21)
lfs16.imp$district=NULL


df16=bind_rows(hies16,lfs16.imp)

df16=subset(df16,!is.na(popwt))

df16_ecdf <- df16 %>%
    group_by(survey) %>%
    arrange(log_consumption = log(welfare)) %>%
    mutate(cum_weight = cumsum(popwt),
           total_weight = sum(popwt),
           ecdf = cum_weight / total_weight)


ggplot(df16_ecdf, aes(x = log(welfare), y = ecdf, color = survey)) +
    geom_step() +
    labs(x = "Log Consumption",
         y = "Density",
         title = "ECDF of Imputed Log Consumption by survey (2016)")+
    geom_vline(xintercept = 3,linetype="dashed",size=0.5)+
    geom_vline(xintercept = 4.2,linetype="dashed",size=0.5)+
    geom_vline(xintercept = 8.3,linetype="dashed",size=0.5)+
    annotate("text", x = 3, y = 0, label = "3.0", vjust = 1.5,size=1)+
    annotate("text", x = 4.2, y = 0, label = "4.2", vjust = 1.5,size=1)+
    annotate("text", x = 8.3, y = 0, label = "8.3", vjust = 1.5,size=1)
