library(dplyr)
library(tidyr)
library(lubridate)
library(condprob2)
library(randomForest)
library(stringr)
library(ggplot2)
db_path <- "inst/extdata/WQM1.db"
wwdb <- src_sqlite(db_path)
wwdb_station <- tbl(wwdb,"Station") %>% 
  collect() %>%
  select(ww_id = StationName,LakeAcreage)

#devtools::install_github("jhollist/condprob2")
load("data2003.lakes")
dat <- data2003.lakes
dat_filt <- dat %>%
  filter(ParShortName == "Chlorophyll a" & DepthFromSurface == 1 & !is.na(Concentration) |
           ParShortName == "Secchi Depth" & DepthFromSurface == 0 & !is.na(Concentration) |
           ParShortName == "Temperature" & DepthFromSurface == 1 & !is.na(Concentration) ) %>%
  select(ResultID,ActivityYear,StartDate,StationName,StationLocation,Longitude,
         Latitude,MaxDepth,ParShortName,Concentration,DepthFromSurface) %>%
  mutate(date = as_date(StartDate))

dat_filt_grp <- dat_filt %>% 
  group_by(date,StationLocation,ParShortName) %>%
  summarize(ww_id = unique(StationName),
            average_conc = mean(Concentration,na.rm = T),
            #num_samples = n(),
            #sd = sd(Concentration,na.rm = T),
            max_depth = unique(MaxDepth),
            long = unique(Longitude),
            lat = unique(Latitude)
          ) %>%
  spread(ParShortName,average_conc)


#Read in nao
nao_url <- "http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table"
x<-read.table(nao_url,header=F,sep="\t")
colname<-c("year","1","2","3","4","5","6","7","8","9","10"
           ,"11","12")
x<-str_split_fixed(x[,1], "[ \\t]+",13)
x[x==""]<-NA
x<-data.frame(x[,1],apply(x[,2:13],2,as.numeric),stringsAsFactors = F)
names(x)<-colname
x<-gather(x,month,nao,-year)

#Join in with dat_filt_grp
dat_filt_grp$year <- as.character(year(dat_filt_grp$date))
dat_filt_grp$month <- as.character(month(dat_filt_grp$date))
dat_filt_grp <- full_join(dat_filt_grp,x,by=c("year","month")) %>%
  full_join(wwdb_station)

write.csv(dat_filt_grp,"jeffs_data.csv")

cc <- complete.cases(dat_filt_grp)
dat_filt_grp_cc <- dat_filt_grp[cc,] %>% 
  data.frame()
dat_filt_grp_cc$chla_class <- as.numeric(dat_filt_grp_cc$Chlorophyll.a >= 23)



xlm <- lm(log1p(dat_filt_grp_cc$Chlorophyll.a) ~ log1p(dat_filt_grp_cc$Secchi.Depth) + 
            log1p(dat_filt_grp_cc$Temperature) + dat_filt_grp_cc$nao) 
xrf <- randomForest(log1p(dat_filt_grp_cc$Chlorophyll.a) ~ log1p(dat_filt_grp_cc$Secchi.Depth) + 
                      log1p(dat_filt_grp_cc$Temperature) + dat_filt_grp_cc$nao)
xrf2 <- dat_filt_grp_cc %>%
  filter(month == 8 | month == 9) %>%
  randomForest(factor(chla_class) ~ Secchi.Depth + Temperature + 
                       LakeAcreage + max_depth, data=., 
                     importance = T)
#logistic
logistic <- glm(factor(chla_class) ~ Secchi.Depth + Temperature, data=dat_filt_grp_cc, family = "binomial")
pred <- predict(logistic, type="response")>0.5 # predicted values
obs <- dat_filt_grp_cc$chla_class == 1
table(pred,obs)

#negative binomial
install.packages("MASS")
library(MASS)
neg_binom <- glm.nb(factor(chla_class) ~ Secchi.Depth + Temperature, data=dat_filt_grp_cc)
pred <- predict(neg_binom, type="response")>0.5 # predicted values
obs <- dat_filt_grp_cc$chla_class == 1
table(pred,obs)

#Facet
gg <- dat_filt_grp_cc %>%
  filter(month == 8 | month == 9) %>%
  ggplot(aes(x=Secchi.Depth,y=Temperature)) +
  geom_point(aes(alpha=LakeAcreage))
gg

bloom_month <- dat_filt_grp_cc %>%
  group_by(month) %>%
  summarize(perc_bloom = sum(chla_class)/n())
plot(bloom_month$month,bloom_month$perc_bloom)

bloom_yr <- dat_filt_grp_cc %>%
  filter(month == 8 | month == 9) %>%
  group_by(year) %>%
  summarize(perc_bloom = sum(chla_class)/n())
plot(bloom_yr$year,bloom_yr$perc_bloom)




