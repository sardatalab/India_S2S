#lfs: 2019
lfs19=lfs
lfs19$survey="LFS_19"
#2016
lfs16.orig=read_dta(paste(datapath,
                       "cleaned/lfs2016_clean.dta",
                       sep="")) 
lfs16.orig=subset(lfs16.orig,select=c(hhid,district, urban,popwt))

lfs16.sim=readRDS(paste(datapath,
      "cleaned/Stage 2/Final/Simulations_model_match_2016.rds",sep=""))
lfs16.sim$welfare=apply(lfs16.sim[,-1],
      1,geometric_mean,na.rm=TRUE)
lfs16.sim=subset(lfs16.sim,select = c("hhid","welfare"))

lfs16.imp=merge(lfs16.orig,lfs16.sim,by="hhid",
              all.x=TRUE)
lfs16.imp$survey="LFS_16"
lfs16.imp$hhid=NULL

#2023

lfs23.orig=read_dta(paste(datapath,
                          "cleaned/lfs2023_clean.dta",
                          sep="")) 
lfs23.orig=subset(lfs23.orig,select=c(hhid,district, urban,popwt))

lfs23.sim=readRDS(paste(datapath,
                        "cleaned/Stage 2/Final/Simulations_model_match_2023.rds",sep=""))
lfs23.sim$welfare=apply(lfs23.sim[,-1],
                        1,geometric_mean,na.rm=TRUE)
lfs23.sim=subset(lfs23.sim,select = c("hhid","welfare"))

lfs23.imp=merge(lfs23.orig,lfs23.sim,by="hhid",
                all.x=TRUE)
lfs23.imp$survey="LFS_23"
lfs23.imp$hhid=NULL

###APPEND THREE ROUNDS 

lfs.all=bind_rows(lfs19,lfs16.imp,lfs23.imp)


ggplot(lfs.all, aes(x = log(welfare), weight = popwt,
               color = survey)) +
    geom_density(alpha = 0.4, adjust=1.5) +
    labs(x = "Log Consumption",
         y = "Density",
         title = "Original and Imputed Log Consumption by Survey (2016-2023)")

ggsave(paste(path,
             "/Outputs/Main/Figures/figure 8a_all.png",sep=""),
       width = 30, height = 20, units = "cm")

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
             "/Outputs/Main/Figures/figure 8b_all.png",sep=""),
       width = 30, height = 20, units = "cm")


hies16=read_dta("C:/Users/wb562318/Downloads/hies16ppp.dta")
hies16$survey="HIES_16"
hies16$district=NULL
hies16=hies16 %>%
    rename(popwt=weight)

lfs16.imp$welfare=with(lfs16.imp,welfare*(12/365)/cpi21/icp21)
lfs16.imp$district=NULL


df16=bind_rows(hies16,lfs16.imp)

df16=subset(df16,!is.na(popwt))

ggplot(df16, aes(x = log(welfare), weight = popwt,
                    color = survey)) +
    geom_density(alpha = 0.4, adjust=1.5) +
    labs(x = "Log Consumption",
         y = "Density",
         title = "Original and Imputed Log Consumption by Survey (2016-2023)")


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
         title = "ECDF of Imputed Log Consumption by survey (2016-23)")+
    geom_vline(xintercept = 3,linetype="dashed",size=0.5)+
    geom_vline(xintercept = 4.2,linetype="dashed",size=0.5)+
    geom_vline(xintercept = 8.3,linetype="dashed",size=0.5)+
    annotate("text", x = 3, y = 0, label = "3.0", vjust = 1.5,size=1)+
    annotate("text", x = 4.2, y = 0, label = "4.2", vjust = 1.5,size=1)+
    annotate("text", x = 8.3, y = 0, label = "8.3", vjust = 1.5,size=1)
