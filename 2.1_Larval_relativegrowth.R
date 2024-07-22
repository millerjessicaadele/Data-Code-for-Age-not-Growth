#07/22/2024 with R version 4.2.2 (2022-10-31 ucrt)
#Final code for Miller et al. "Age, not growth, explains larger body size of Pacific cod larvae during recent marine heatwaves"
#LMM to examine variation in daily growth before and since MHWs

if (!require(rstudioapi)) install.packages('rstudioapi')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(dplyr)) install.packages('dplyr')
if (!require(nlme)) install.packages('nmle')
if (!require(gvlma)) install.packages('gvlma')
if (!require(car)) install.packages('car')
if (!require(performance)) install.packages('performance')
if (!require(effects)) install.packages('effects')
if (!require(ggeffects)) install.packages('ggeffects')
if (!require(MuMin)) install.packages('MuMIn')
if (!require(FSA)) install.packages('FSA')
if (!require(ggpubr)) install.packages('ggpubr')

library(rstudioapi)
library(tidyverse)
library(dplyr)
library(nlme)
library(gvlma)
library(performance)
library(MuMIn)
library(car)
library(FSA) 
library(effects)
library(ggeffects)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

####----IMPORT GROWTH DATA----####

all_data <- read.csv("2_all_larvae_somatic_oto.csv") 

####------DATA PREPARATION FOR MODEL----####

all_data$mmpermmperdy <- all_data$daily_gr / all_data$BC_estimate

samplesizecheck <- all_data %>% 
  filter(dayoflife == 1) %>% 
  dplyr::group_by(yr_f) %>% 
  select(age) %>% 
  dplyr::summarise_all(funs(mean, n = n()))
samplesizecheck 

max(all_data$age)

dailygr.analysis <- all_data %>%
  dplyr::filter(age < 67) %>% 
  mutate(scaledGr = scale(daily_gr, center = FALSE, scale = TRUE)) %>%
  mutate(scaledDGr = scale(mmpermmperdy, center = FALSE, scale = TRUE)) %>%
  mutate(scaledBCSize = scale(BC_estimate, center = FALSE, scale = TRUE)) %>%
  mutate(scaledAge = scale(dayoflife)) %>%
  mutate(scaledTemp = scale(TempApplied)) %>%
  mutate(scaledHD = scale(as.numeric(hdate))) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Yearf = as.factor(YEAR))

max(dailygr.analysis$age)

a <-(unique(dailygr.analysis$Unique.ID))


####--------------OBSERVED VALUES--------------####

quantile(dailygr.analysis$hdate, na.rm = T,probs = c(.34,.66))

dailygr.analysis.1 <- dailygr.analysis%>%
  dplyr::mutate(hdate = as.numeric(hdate)) %>%
  dplyr::mutate(hcat = case_when(hdate >= 65 & hdate <= 105~ "Early",
                                 hdate > 105 & hdate <= 117 ~ "Middle",
                                 hdate > 117  & hdate <= 141 ~ "Late")) %>%
  mutate(hcat = as.factor(hcat))

sixto20dayplot <- dailygr.analysis.1 %>%
  filter(dayoflife <21 & dayoflife > 5)

toplot<- sixto20dayplot

color <- c("darkblue", "red")
lines <- c("solid", "dashed")

# Specify the order of the x-axis values
custom_order <- c("Early", "Middle", "Late")

# Convert the variable to a factor with custom order using forcats
sixto20dayplot$hcat <- fct_relevel(toplot$hcat, custom_order)

early <- ggplot(NULL, mapping = aes(x=TempApplied, y=mmpermmperdy, group = hw)) + 
  geom_point(data =  sixto20dayplot  , aes(x = TempApplied, y = mmpermmperdy), color = "lightgrey", alpha = 0.4)+
  geom_smooth(method = lm, data =  sixto20dayplot, aes(x=TempApplied, y=mmpermmperdy,  color = hw, fill = hw, linetype = hw))+
  theme_classic(base_size = 19)+
  theme(legend.position = "top")+
  scale_color_manual(values=color)+
  scale_fill_manual(values=color)+
  scale_linetype_manual(values = lines)+
  ylim(0,0.06)+
  ylab("Growth, mm/mm/day")+
  xlab("Temperature")+
  theme(legend.text = element_text(size = 14))

early

early + facet_wrap(vars(hcat))

mean(sixto20dayplot$sl_mm)
sd(sixto20dayplot$sl_mm)

ggsave("mmpermmperday6to20p.jpg", plot = last_plot(), device = "jpg",
       width = 8, height = 6, units = "in", dpi = 300)


thirtytofortydayplot <- dailygr.analysis.1 %>%
  filter(dayoflife <46 & dayoflife > 30)

mean(thirtytofortydayplot$sl_mm)
sd(thirtytofortydayplot$sl_mm)

toplot2<- thirtytofortydayplot #copy data to graph observations not predictions
toplot2$model2acor_predict <- toplot2$mmpermmperdy

late <- ggplot(NULL, mapping = aes(x=TempApplied, y=mmpermmperdy, group = hw)) + 
  geom_point(data = thirtytofortydayplot , aes(x = TempApplied, y = mmpermmperdy), color = "lightgrey", alpha = 0.4)+
  geom_smooth(method = lm,data =  thirtytofortydayplot, aes(x=TempApplied, y=mmpermmperdy,  color = hw, fill = hw, linetype = hw))+
  theme_classic(base_size = 19)+
  theme(legend.position = "top")+
  ylim(0,0.08)+
  scale_color_manual(values=color)+
  scale_fill_manual(values=color)+
  scale_linetype_manual(values = lines)+
  ylab("Growth, mm/mm/day")+
  xlab("Temperature")+
  ylim(0, 0.06)+
  theme(legend.text = element_text(size = 14))

late + facet_wrap(vars(hcat))

ggsave("mmpermmperdayday31to45p.jpg", plot = last_plot(), device = "jpg",
       width = 8, height = 6, units = "in", dpi = 300)



###------MODEL RELATIVE GROWTH ---------#####

model2.13<- lme(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD *hw, method = "ML",
                dailygr.analysis, random = ~dayoflife | Unique.ID)

model2.14 <- lm(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD * hw,
                dailygr.analysis)

AIC(model2.13, model2.14)

model2.1<- lme(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD, method = "ML",
                dailygr.analysis, random = ~dayoflife | Unique.ID)

model2.13<- lme(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD *hw, method = "ML",
                dailygr.analysis, random = ~dayoflife | Unique.ID)

AIC(model2.1, model2.13)

check_collinearity(model2.13)
check_collinearity(model2.1)

model2.1<- lme(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD, method = "REML",
               dailygr.analysis, random = ~dayoflife | Unique.ID)


model2.1_lme_acor <- update(model2.1,
                            correlation=corAR1(form=~dayoflife | Unique.ID))

AIC(model2.1, model2.1_lme_acor)

check_collinearity(model2.1_lme_acor)

summary(model2.1_lme_acor )
Anova(model2.1_lme_acor, type = 3)

#####--------Final Model for Daily Growth---------------######

model2<- lme(mmpermmperdy ~ scaledAge * scaledTemp* scaledHD, method = "REML",
             dailygr.analysis, random = ~dayoflife | Unique.ID)
summary(model2)
anova(model2)
Anova(model2)
plot(ACF(model2, resType = "normalized"))
r.squaredGLMM(model2)

model2_lme_acor <- update(model2, 
                          correlation=corAR1(form=~dayoflife | Unique.ID))

rand<- ranef(model2_lme_acor)
plot(rand)


plot(ACF(model2_lme_acor, resType = "normalized"))
summary(model2_lme_acor)
Anova(model2_lme_acor)
r.squaredGLMM(model2_lme_acor)

AIC(model2, model2_lme_acor)

check_collinearity(model2_lme_acor)

plot(model2_lme_acor) 
plot(dailygr.analysis$Yearf, resid(model2_lme_acor, type = "pearson"))
plot(dailygr.analysis$scaledTemp, resid(model2_lme_acor, type = "pearson"))
plot(dailygr.analysis$scaledHD, resid(model2_lme_acor, type = "pearson"))
plot(dailygr.analysis$scaledAge, resid(model2_lme_acor, type = "pearson"))
qqnorm(resid(model2_lme_acor, type = "pearson"))
qqline(resid(model2_lme_acor, type = "pearson"))
hist(residuals(model2_lme_acor, type = "pearson"), xlab = "Standardized residuals", ylab = "Frequency", main = NULL)


####---------------PREDICTED VALUES--------------####

a <- predict(model2_lme_acor)
dailygr.analysis$model2acor_predict <- predict(model2_lme_acor)
dailygr.analysis$hw <- as.factor(dailygr.analysis$hw)
plot(dailygr.analysis$model2acor_predict,a)


####--------------Age X Hatch Date with Data Points--------------#####

mydf2<- ggpredict(model2_lme_acor, terms = c("scaledAge","scaledHD", 'scaledTemp'))
mydf2$mmpermmperdy <- mydf2$predicted
mydf2$scaledAge <- mydf2$x
mydf2$scaledHD<- mydf2$group
mydf2$scaledTemp <- mydf2$facet

a <-mean(dailygr.analysis$dayoflife)
sd <-sd(dailygr.analysis$dayoflife)
mydf2$unscaledage <- mydf2$scaledAge*sd+a


facet.labs <- c("scaledTemp = -1", "scaledTemp = 0", "scaledTemp = 1")
names(facet.labs) <- c("-1", "0", "1")

labels <- c("-1" = "scaledTemp = -1", "0" = "scaledTemp = 0", "1" = "scaledTemp = 1")

dailygr.analysis.1 <- dailygr.analysis
dailygr.analysis.1$facet = case_when(dailygr.analysis.1$scaledTemp <= -0.05 & dailygr.analysis.1$scaledTemp >-1.62 ~ "-1",  
                                        dailygr.analysis.1$scaledTemp > -0.05 & dailygr.analysis.1$scaledTemp <= 0.50~ "0",
                                        dailygr.analysis.1$scaledTemp > 0.50 & dailygr.analysis.1$scaledTemp < 4.8 ~ "1") 

dailygr.analysis.1 <- dailygr.analysis.1[!is.na(dailygr.analysis.1$facet),]
  
dailygr.analysis.1$facet <-as.factor(dailygr.analysis.1$facet)


all <- ggplot(NULL, mapping = aes(scaledAge, mmpermmperdy)) + 
  geom_point(data = dailygr.analysis.1, aes(x = scaledAge, y = mmpermmperdy), color = "lightgrey")+
  geom_smooth(method = lm, data = mydf2, aes(x = scaledAge, y = mmpermmperdy, color = scaledHD, linewidth= 1))+
  scale_color_manual(name="Scaled HD", values=c( "lightgreen", "cyan4","darkgreen"))+
  scale_fill_manual(values=c( "lightgreen", "cyan4", "darkgreen"))+
  labs(x = "Scaled Age", y = "Growth, mm/mm/day")+
  ylim(0.005, 0.06)+
  ggtitle("") +
  facet_grid(.~facet, labeller = labeller(facet = labels)) +
  theme_classic(base_size =20, base_family = "")

all 

all + facet_wrap(vars(facet))

ggsave("Figure_Relgrowthmodelall.jpg", plot = last_plot(), device = "jpg",
       width = 12, height = 6, units = "in", dpi = 300)

###Actual DATA POINTS with LM fitted to predicted value & separated by MHW----#####

dailygr.analysis.2 <- dailygr.analysis

dailygr.analysis.2 <- dailygr.analysis.2%>%
  dplyr::mutate(hdate = as.numeric(hdate)) %>%
  dplyr::mutate(hcat = case_when(hdate >= 65 & hdate <= 105~ "Early",
                                 hdate > 105 & hdate <= 117 ~ "Middle",
                                 hdate > 117  & hdate <= 141 ~ "Late")) %>%
  mutate(hcat = as.factor(hcat))

dailygr.analysis.2$predgrowth <- predict(model2_lme_acor)
plot(dailygr.analysis.2$mmpermmperdy, dailygr.analysis.2$predgrowth)

min(dailygr.analysis.2$scaledTemp)
max(dailygr.analysis.2$scaledTemp)

dailygr.analysis.2$scaledT2 = case_when(dailygr.analysis.2$scaledTemp <= -0.05 & dailygr.analysis.2$scaledTemp >-1.62 ~ "-1",  
                                        dailygr.analysis.2$scaledTemp > -0.05 & dailygr.analysis.2$scaledTemp <= 0.50~ "0",
                                        dailygr.analysis.2$scaledTemp > 0.50 & dailygr.analysis.2$scaledTemp < 4.8 ~ "1") 
dailygr.analysis.2$scaledT2 <- as.factor(dailygr.analysis.2$scaledT2)

min(dailygr.analysis.2$TempApplied)
max(dailygr.analysis.2$TempApplied)
quantile(dailygr.analysis.2$TempApplied)

dailygr.analysis.2$T3 = case_when(dailygr.analysis.2$TempApplied <= 5 & dailygr.analysis.2$TempApplied >4 ~ "<5.0 °C",  
                                        dailygr.analysis.2$TempApplied > 5 & dailygr.analysis.2$TempApplied<= 5.7~ "5.0 to 5.7 °C",
                                        dailygr.analysis.2$TempApplied > 5.7 & dailygr.analysis.2$TempApplied < 10 ~ ">5.71 °C") 
dailygr.analysis.2$T3 <- as.factor(dailygr.analysis.2$T3)

# Specify the order of the x-axis values
custom_order.1 <- c("<5.0 °C", "5.0 to 5.7 °C", ">5.71 °C")

# Convert the variable to a factor with custom order using forcats
dailygr.analysis.2$T3 <- fct_relevel(dailygr.analysis.2$T3, custom_order.1)


beforedata <- dailygr.analysis.2 %>% 
  filter(hw == "before HW")

afterdata <- dailygr.analysis.2 %>% 
  filter(hw == "during and after HW")

# Specify the order of the x-axis values
custom_order <- c("Early", "Middle", "Late")

# Convert the variable to a factor with custom order using forcats
beforedata$hcat <- fct_relevel(beforedata$hcat, custom_order)
afterdata$hcat <- fct_relevel(afterdata$hcat, custom_order)

before <- ggplot(beforedata , mapping = aes(age, mmpermmperdy, color = hcat)) + 
  facet_wrap(~T3)+
  geom_point()+
  geom_smooth(method = lm, data = beforedata , aes(x = age, y = model2acor_predict, color = hcat, linewidth= 1))+
  scale_color_manual(name="Hatch date", values=c("darkblue", "orange", "darkolivegreen4"))+
  scale_fill_manual(values=c("darkblue", "orange", "darkolivegreen4"))+
  labs(x = "Age, days", y = "Growth, mm/mm/day")+
  ylim(0.005, 0.06)+
  xlim(10,80)+
  ggtitle("") +
 # facet_grid(.~facet, labeller = labeller(facet = labels)) +
  theme_classic(base_size =20, base_family = "")

before

ggsave("Figure_Relgrowthmodelbefore.jpg", plot = last_plot(), device = "jpg",
       width = 12, height = 6, units = "in", dpi = 300)


after <- ggplot(afterdata , mapping = aes(age, mmpermmperdy,color = hcat)) + 
  facet_wrap(~T3)+
  geom_point()+
  geom_smooth(method = lm, data = afterdata , aes(x = age, y = model2acor_predict, color = hcat, linewidth= 1))+
  scale_color_manual(name="Hatch date", values = c("darkblue", "orange", "darkolivegreen4"))+
  scale_fill_manual(values=c("darkblue", "orange", "darkolivegreen4"))+
  labs(x = "Age, days", y = "Growth, mm/mm/day")+
  ylim(0.005, 0.06)+
  xlim(10,70)+
  ggtitle("") +
  # facet_grid(.~facet, labeller = labeller(facet = labels)) +
  theme_classic(base_size =20, base_family = "")

after

ggsave("Figure_Relgrowthmodelafter.jpg", plot = last_plot(), device = "jpg",
       width = 12, height = 6, units = "in", dpi = 300)















