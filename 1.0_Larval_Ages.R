

#Final code for Miller et al. Older and slower: unexpected effects of marine heatwaves on larval fish.
#06/04/2024 with R version 4.2.2 (2022-10-31 ucrt)

library(tidyverse)
library(readr)
library(ggplot2)
library(effects)
library(ggeffects)
library(lme4)
library(nlme)
library(AICcmodavg)
library(gvlma)
library(car)
library(rstudioapi)
library(performance)
library(MuMIn)
library(FSA) 


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####-----APPLIES AGE LENGTH KEY TO ALL-----####

larvae <- read.csv("1_larvalagecapdate.csv") #210 aged larvae

larvae <- larvae %>% 
  mutate(sl_mm = sl_micron/1000) %>% 
  mutate(fYEAR = as.factor(YEAR)) %>%
  filter(YEAR != 2016 & YEAR != 2008 & YEAR != 2011)


age_model.YEAR<- lm(sqrt(mean_age) ~ sl_micron * fYEAR, data = larvae) 
age_model.1 <- lm(sqrt(mean_age) ~ sl_micron + fYEAR, data = larvae) 

AIC(age_model.1, age_model.YEAR)

age_model.YEAR<- lm(sqrt(mean_age) ~ sl_micron * fYEAR, data = larvae) 

age_model.YEAR
summary(age_model.YEAR)
Anova(age_model.YEAR, type = 3)

gvmodel.1 <- gvlma(age_model.YEAR)
summary(gvmodel.1)
res_agemodel <- resid(age_model.YEAR)
plot(larvae$sl_micron, res_agemodel)
abline(h=0)

qqPlot(age_model.YEAR, main="QQ Plot")
ncvTest(age_model.YEAR)

larvae$pred.YEAR <- predict(age_model.YEAR, x=larvae$sl_micron, na.action = remove())

#PREDIcT FOR ALL CATcH

new_larvae <- read.csv("1_Pcod_LarvalLength.csv") 
unique(new_larvae$YEAR)

new_larvae <- new_larvae %>% 
    mutate(fYEAR = as.factor(YEAR))

larvae1 <- larvae
larvae <- new_larvae

larvae <- larvae %>% 
  mutate(sl_micron = FcorrLen_mm*1000) %>% 
  mutate(fYEAR = as.factor(YEAR))

larvae$pred_age2 <- (predict(age_model.YEAR, newdata = larvae))^2

larvae$pred_age <- (predict(age_model.YEAR, newdata = larvae))

conf.dist <- predict(age_model.YEAR, newdata = larvae, interval="confidence", level=.95) 
head(conf.dist)

pred.dist <- predict(age_model.YEAR, newdata = larvae,interval="prediction", level=.95) 
head(pred.dist)

#put those values with column names into two new matrices and plot

ages.new <- larvae

ages.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
ages.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]

ages.new <- ages.new %>% 
  mutate(lwr.conf = lwr.conf^2) %>% 
  mutate(upr.conf = upr.conf^2) %>% 
  mutate(lwr.pred = lwr.pred^2) %>% 
  mutate(upr.pred= upr.pred^2) 

ages.new$rand_age <- runif(2061, ages.new$lwr.conf, ages.new$upr.conf)

head(ages.new)

larvae$randage <- ages.new$rand_age

plot(larvae$pred_age2, larvae$randage)

larvae$new_id <- vector(mode="character", length=2061)
larvae$new_id  <- rep("p", each =2061)
mycols <- c("OBJECTID","new_id")
larvae$pred_id <- do.call(paste, c(larvae[mycols], sep = ""))
larvae$Unique.ID <- c(221:2281)

write.csv(larvae, "allpredages2061.YEARHAUL.csv")

#for cap date

larvae1 <- read.csv("1_larvalagecapdate.csv") 

larvae <- read.csv("allpredages2061.YEARHAUL.csv")

haul.data <- read.csv("1_HaulData.csv", header = TRUE)

larvae.w.haul <- left_join(haul.data, larvae, by = c("HAUL_ID" = "HAUL_ID"), na_matches = "never")

larvae.w.haul <-drop_na(larvae.w.haul) #n=2061

#plot(larvae.w.haul$YEAR.x, larvae.w.haul$YEAR.y)

#larvae.w.haul <- larvae.w.haul[,-c(18:24)]

larvae.w.haul  <- larvae.w.haul %>%
  separate(GMT_DATE_TIME, into = c("date", "time"), sep = " ")%>%
  mutate(capdate = as.Date(date, format = "%m/%d/%Y"))%>%
  mutate(hatch.date = capdate - randage) %>%
  mutate(capdate_d = as.numeric(strftime(capdate, format = "%j")))%>%
  mutate(hatch.date.noYear = format(hatch.date, "%b-%d")) %>%
  dplyr::select(- time) %>%
  mutate(hdate = as.numeric(strftime(hatch.date, format = "%j")))

# join larvae.w.haul and larvae1.

larvae_aged <- larvae1 %>%
  dplyr::select(Unique.ID, sl_micron, capdate, hatch.date, hatch.date.noYear, hdate, mean_age, YEAR, station) %>%
   mutate(age = mean_age)%>%
   mutate(FREQUENCY = vector('numeric', 210))%>%
   mutate(FREQUENCY = 1) %>%
   mutate(capdate = as.Date(capdate, format = "%m/%d/%Y")) %>%
   mutate(capdate_d = as.numeric(strftime(capdate, format = "%j")))%>%
   dplyr::select(-mean_age)%>% 
   mutate(hatch.date = as.Date(hatch.date, format = "%m/%d/%Y"))


 
larvae.w.haul.1 <- larvae.w.haul %>%
   dplyr::select(Unique.ID, sl_micron, capdate, capdate_d, hatch.date, hatch.date.noYear, hdate, randage, YEAR.x, FREQUENCY, station)%>%
   mutate(age = randage)%>%
   mutate(YEAR = YEAR.x)%>%
   dplyr::select(-YEAR.x, -randage)

larvae.w.haul.new <- larvae.w.haul.1[c('Unique.ID', 'sl_micron', 'capdate', 'capdate_d', 'hatch.date','hatch.date.noYear', 'hdate', 'YEAR', 'age', 'FREQUENCY', 'station')]

all_larvae_age_locale <- rbind(larvae_aged, larvae.w.haul.new)
 
all_larvae_age_locale$hw <- ifelse(all_larvae_age_locale$YEAR == 2015 |all_larvae_age_locale$YEAR == 2017 |
                                    all_larvae_age_locale$YEAR == 2019,  "During and after HW", "Before HW")
 
 
all_larvae_age_locale <- all_larvae_age_locale %>% 
   filter(YEAR != 2016 & YEAR != 2008 & YEAR != 2011)
 
write.csv(all_larvae_age_locale, "STATIONFINAL_all_larvae_agewlocale_ran.YEAR.csv")
 
##conf interval for the predicted ages for the model
 
####----Regression of Hatch date on Deep Temp JFMA----#####

hd <- c(117.95,136.84,125.51,124.02,112.76,106.92,101.07)
temp <- c(5.51,4.93,5.73,5.38,6.71, 5.97,6.89)

model <- lm(hd ~ temp)

summary(model)
Anova(model)
check_model(model)

#####------Model to evaluate HD across MHW------#####E

all_larvae_age_locale <- read.csv("STATIONFINAL_all_larvae_agewlocale_ran.YEAR.csv")

all_larvae_age_locale <- all_larvae_age_locale %>% 
  mutate(fYr = YEAR) %>% 
  mutate(fstation = station) %>% 
  mutate(scaledStat = c(scale(station)))

station <- all_larvae_age_locale %>%
  dplyr::group_by(fYr, station) %>%
  dplyr::select(hdate)%>%
  dplyr::summarise_all(funs(mean, sd, n = n()))
station

hdshift<- lmer(hdate  ~ hw + (1| fYr), data = all_larvae_age_locale, REML = FALSE)
hdshifta<- lmer(hdate  ~ hw + (1|scaledStat), data = all_larvae_age_locale, REML = FALSE)
hdshift1<- lmer(hdate  ~ hw + (1| fYr) + (1 |scaledStat), data = all_larvae_age_locale, REML = FALSE)
hdshift1b<- lmer(hdate  ~ hw + (1+ scaledStat| fYr), data = all_larvae_age_locale, REML = FALSE)
hdshift2<- lm(hdate  ~ hw, data = all_larvae_age_locale)

AIC(hdshift, hdshifta, hdshift1, hdshift1b, hdshift2)

hdshift<- lmer(hdate  ~ hw + (1| fYr)+ (1 |scaledStat), data = all_larvae_age_locale, REML = TRUE)
summary(hdshift)
hdshift
Anova(hdshift)

Anova(hdshift, type = 3)

check_model(hdshift)
plot(resid(hdshift))
plot(hdshift)
r.squaredGLMM(hdshift)


####-----IMPORT ALL PREDICTED AND AGED -INCLUDES PLOT OF JUST AGED--####

all_larvae_age_locale <- read.csv("STATIONFINAL_all_larvae_agewlocale_ran.YEAR.csv")
all_larvae_age_locale <- all_larvae_age_locale [, -c(1)]
all_larvae_age_locale$fYr <- as.factor(all_larvae_age_locale$YEAR)
all_larvae_age_locale$hw <- as.factor(all_larvae_age_locale$hw)
larvae <- all_larvae_age_locale


####----ALL DATA-------------####

color <- c("black", "lightsteelblue3")
  

all <- ggplot(data = all_larvae_age_locale, aes(x = hdate, group = fYr, fill=hw)) + 
  geom_density(adjust =1.5, alpha =0.4)+
  scale_fill_manual(values=color)+
  xlab("Hatch date for all fish") + 
  ylab("Density") +
  xlim(50, 160)+
  theme_classic() +
  theme(legend.position="")+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        legend.text = element_text(size = 18), legend.title = element_blank())

all 


larvae_aged <- all_larvae_age_locale %>%
  mutate(sl_mm = sl_micron/1000)%>%
  mutate(Unique.ID = as.numeric(Unique.ID)) %>% 
  filter(Unique.ID < 222)


age <- ggplot(data = larvae_aged, aes(x = hdate, group = fYr, fill=hw)) + 
  geom_density(adjust =1.5, alpha =0.4)+
  scale_fill_manual(values=color)+
  xlab("Hatch date for aged fish") + 
  ylab("Density") +
  xlim(50, 160)+
  theme_classic() +
  theme(legend.position="top")+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        legend.text = element_text(size = 18), legend.title = element_blank())
age
ggarrange(ncol = 1, age, all)
ggsave("hatchdates.jpg", plot = last_plot(), device = "jpg",
        width = 10, height = 8, units = "in", dpi = 300)



####---------- Model of size at age---------------#####

larvae2 <- read.csv("STATIONFINAL_all_larvae_agewlocale_ran.YEAR.csv") %>% 
  select(-1)

#all = 2262 fish, reduced = 2152. Aged fish range from 4.8 to 21 mm
sizeatage <- larvae2 %>%
  mutate(sl_mm = sl_micron/1000)%>%
  mutate(logsl_mm = log(sl_mm))%>%
  mutate(scaledSL = c(scale(sl_mm))) %>%
  mutate(scaledAge = c(scale(age))) %>%
  mutate(scaledYr = c(scale(YEAR))) %>%
  mutate(fYr = as.factor(YEAR)) %>%
  filter(sl_mm > 4.79) %>%
  filter(sl_mm < 22) %>%
  filter(age < 52) %>%
  mutate(totgr_mmday = sl_mm / age) %>%
  mutate(scaledHD = c(scale(as.numeric(hdate))))

individuals <- sizeatage %>% 
  filter(Unique.ID<221)

ids <-individuals  %>% 
  select(age, YEAR) %>% 
  dplyr::group_by(as.factor(YEAR)) %>% 
  dplyr::summarise_all(funs(mean, n = n()))

####--------MODEL OF SIZE AT AGE---------#####

sizeatage_1.0 <- lm(sl_mm ~ poly(scaledAge, degree =2) * hw, data = sizeatage)
sizeatage_1.0a<- lmer(sl_mm ~ poly(scaledAge, degree =2) * hw + (1| fYr), data = sizeatage, REML = FALSE)
sizeatage_1.0b<- lmer(sl_mm ~ poly(scaledAge, degree =2) * hw + (1| fYr) + (1|station), data = sizeatage, REML = FALSE)

AIC(sizeatage_1.0, sizeatage_1.0a,sizeatage_1.0b)
r.squaredGLMM(sizeatage_1.0a)

sizeatage_1.1<- lmer(sl_mm  ~ poly(scaledAge, degree =2) * hw + (1| fYr), data = sizeatage, REML = TRUE)
sizeatage_1.1
summary(sizeatage_1.1)
Anova(sizeatage_1.1)
check_model(sizeatage_1.1)
r.squaredGLMM(sizeatage_1.1)

Anova(sizeatage_1.1, type = 3)

plot(sizeatage_1.1) 
hist(residuals(sizeatage_1.1, type = "pearson"), xlab = "Standardized residuals", ylab = "Frequency", main = NULL)

sizeatage$res <- resid(sizeatage_1.1, type = "pearson")

###--------------Predict & Plot Effects-----------###

sizeatage$predgrowth <- predict(sizeatage_1.1)

mydf<- ggpredict(sizeatage_1.1, terms = c("scaledAge[all]", "hw"))

####------------Figure for MS---------####

tograph <- sizeatage
tograph$predict_Sl <- predict(sizeatage_1.1)
tograph$sl_mm <-tograph$predict_Sl
sizeatage_aged <- sizeatage %>% 
  filter(Unique.ID < 221)

lines <- c("solid", "dashed")

ggplot(NULL, mapping = aes(age, sl_mm)) + 
  geom_point(data = sizeatage, aes(x = age, y = sl_mm), color = c("lightgrey"), size = 1)+
  geom_smooth(size = 1.5, data =tograph, aes(x = age, y = sl_mm, color = hw, linetype = hw))+
  scale_color_manual(values=c("blue", "firebrick"))+
  ylim(4, 18)+
  labs(x = "Age, days", y = "TL at capture, mm")+
  theme_classic(base_size = 22, base_family = "")


ggsave("Sizeatagemodel.jpg", plot = last_plot(), device = "jpg",
       width = 10, height = 4, units = "in", dpi = 300)












