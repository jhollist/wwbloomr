library(dplyr)
library(tidyr)
library(lubridate)
library(condprob2)
load("data2003.lakes")
dat <- data2003.lakes
dat_filt <- dat %>%
  filter(ParShortName == "Chlorophyll a" & DepthFromSurface == 1 |
           ParShortName == "Secchi Depth" & DepthFromSurface == 0 |
           ParShortName == "Temperature" & DepthFromSurface == 1) %>%
  select(ResultID,ActivityYear,StartDate,StationName,StationLocation,Longitude,
         Latitude,MaxDepth,ParShortName,Concentration,DepthFromSurface) %>%
  mutate(date = as_date(StartDate))

dat_filt_grp <- dat_filt %>% 
  group_by(date,StationLocation,ParShortName) %>%
  summarize(ww_id = unique(StationName),
            average_conc = mean(Concentration,na.rm = T),
            num_samples = n(),
            sd = sd(Concentration,na.rm = T),
            max_depth = unique(MaxDepth),
            long = unique(Longitude),
            lat = unique(Latitude)
          ) %>%
  spread(ParShortName,average_conc)

cc <- complete.cases(dat_filt_grp[,9:11])
dat_filt_grp_cc <- dat_filt_grp[cc,] %>% 
  data.frame()
dat_filt_grp_cc$chla_class <- dat_filt_grp_cc$Chlorophyll.a >= 104

xlm <- lm(log1p(dat_filt_grp_cc$Chlorophyll.a) ~ dat_filt_grp_cc$Secchi.Depth + 
            dat_filt_grp_cc$Temperature)
xrf <- randomForest(log1p(dat_filt_grp_cc$Chlorophyll.a) ~ dat_filt_grp_cc$Secchi.Depth + 
                      dat_filt_grp_cc$Temperature)
xrf2 <- randomForest(factor(dat_filt_grp_cc$chla_class) ~ dat_filt_grp_cc$Secchi.Depth + 
                      dat_filt_grp_cc$Temperature)
xrf2

#Work on this...
xcp<-condprob(dat_filt_grp_cc$Secchi.Depth,log1p(dat_filt_grp_cc$Chlorophyll.a),
              log1p(23),"gt","gte",ci=T,R=100)
