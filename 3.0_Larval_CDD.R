#07/22/2024 with R version 4.2.2 (2022-10-31 ucrt)
#Final code for Miller et al. "Age, not growth, explains larger body size of Pacific cod larvae during recent marine heatwaves"
#Cumulative Degree Days


if (!require(rstudioapi)) install.packages('rstudioapi')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(lubridate)) install.packages('lubridate')

library(rstudioapi)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


####----IMPORT ALL DATA WITH OTO AND SOMATIC GROWTH----####

all_data <- read.csv("2_all_larvae_somatic_oto.csv")


####------DATA PREP----####


dailygr.analysis <- all_data %>%
  mutate(scaledDGr = scale(mmpermmperdy, center = FALSE, scale = TRUE)) %>%
  mutate(scaledAge = scale(dayoflife)) %>%
  mutate(scaledTemp = scale(TempApplied)) %>%
  mutate(scaledHD = scale(as.numeric(hdate))) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Yearf = as.factor(YEAR))


####------CDD-------####

dailygr.analysis.cdd <- dailygr.analysis %>%
  select(Unique.ID, dayoflife, TempApplied)%>%
  mutate(dayoflife = as.numeric(dayoflife)) %>% 
  mutate(Unique.ID= as.numeric(Unique.ID))%>% 
  arrange(dayoflife)%>%
  group_by(Unique.ID) %>% 
  dplyr::mutate(CDD = cumsum(TempApplied))


dailygr.analysis.cdd <- left_join(dailygr.analysis, dailygr.analysis.cdd, by = c("Unique.ID" = "Unique.ID","dayoflife" = "dayoflife"), 
                 na_matches = "never")

dailygr.analysis.cdd <- dailygr.analysis.cdd %>%
  select(-TempApplied.y)
  
dailygr.analysis.cdd$month <- format(dailygr.analysis.cdd$Date, "%m")  

dailygr.analysis.cdd$month <- as.numeric(dailygr.analysis.cdd$month)

dailygr.analysis.cdd$DOY <- yday(dailygr.analysis.cdd$Date)

plot(dailygr.analysis.cdd$Date, dailygr.analysis.cdd$DOY)

write.csv(dailygr.analysis.cdd, "dailygr.analysis.cdd.doy.csv")




###############-------PLOTS----------#######################

dailygr.analysis.cdd <- read.csv("dailygr.analysis.cdd.doy.csv") %>% 
  mutate(Yearf = as.factor(Yearf)) %>% 
  mutate(hdate = as.numeric(hdate))  



###COLOR BY YEAR###
color2 = c("blue4",  "darkorchid4", "deepskyblue", "turquoise4",  "red",  "orange", "firebrick")
lines <- c("solid", "dashed")


library(ggplot2)
library(gridExtra)

ggplot(data = dailygr.analysis.cdd, aes(x=dayoflife, y=CDD, ggroup = Yearf, color = Yearf, linetype = hw)) +
  geom_smooth( size =2)+
  scale_color_manual(values=color2)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  xlab("Age")+
  theme(legend.text = element_text(size = 16))

ggsave("age_CDD.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)

