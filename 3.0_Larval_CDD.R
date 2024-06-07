#Final code for Miller et al. Older and slower: unexpected effects of marine heatwaves on larval fish.
#06/04/2024 with R version 4.2.2 (2022-10-31 ucrt)


library(rstudioapi)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


####----IMPORT ALL DATA WITH OTO AND SOMATIC GROWTH BUT NO 2008 or 2011----####

all_data <- read.csv("2_all_larvae_somatic_oto.csv")

#> Add standardized growth

all_data$mmpermmperdy <- all_data$daily_gr / all_data$BC_estimate 

####------DATA PREP----####


dailygr.analysis <- all_data %>%
  mutate(scaledGr = scale(daily_gr, center = FALSE, scale = TRUE)) %>%
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




ggplot(data = dailygr.analysis.cdd, aes(x=dayoflife, y=CDD, group = Unique.ID)) +
  geom_line()+
  theme_classic(base_size = 19)+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size = 14))


ggplot(data = dailygr.analysis.cdd, aes(x=dayoflife, y=CDD, group = hw, color = hw)) +
  geom_smooth()+
  scale_color_hue(direction = -1)+
  theme_classic(base_size = 19)+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size = 14))


#take CDD at end of life and size at capture

dailygr.analysis.lastcdd <- dailygr.analysis.cdd %>%
  dplyr::select(Unique.ID, dayoflife, CDD)%>%
  dplyr::mutate(dayoflife = as.numeric(dayoflife)) %>% 
  dplyr::mutate(Unique.ID= as.numeric(Unique.ID))%>%
  dplyr::mutate(CDD= as.numeric(CDD))%>%
  dplyr::group_by(Unique.ID) %>% 
  dplyr::summarise(dayoflife= max(dayoflife, na.rm=TRUE))

  
dailygr.analysis.lastcdd <- left_join(dailygr.analysis.lastcdd, dailygr.analysis.cdd, by = c("Unique.ID" = "Unique.ID","dayoflife" = "dayoflife"), 
                                  na_matches = "never")

write.csv(dailygr.analysis.lastcdd , "dailygr.analysis.lastcdd2.csv")


 
####----IMPORT LAST CDD TO GRAPH----#####


###COLOR BY YEAR###
color2 = c("blue4",  "darkorchid4", "deepskyblue", "turquoise4",  "red",  "orange", "firebrick")
lines <- c("solid", "dashed")

ggplot(data = dailygr.analysis.cdd , aes(x=CDD, y=mmpermmperdy, group = Yearf, color = Yearf, fill = Yearf, linetype = hw)) +
  geom_smooth(se = F, size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  ylab("Growth, mm/mm/day")+
  xlab("Cumulative degree day (CDD)")+
  theme(legend.text = element_text(size = 16))

ggsave("nrelgro_CDD.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)


####----Plot by Year----####


library(ggplot2)
library(gridExtra)


dailygr.analysis.lastcdd$Yearf <- as.factor(dailygr.analysis.lastcdd$Yearf)

ggplot(data = dailygr.analysis.lastcdd , aes(x=CDD, y=sl_mm, group = Yearf, color = Yearf, linetype = hw)) +
  geom_smooth(se = F, size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  ylab("Length at capture, mm")+
  xlab("Cumulative degree day (CDD)")+
  theme(legend.text = element_text(size = 16))

ggsave("TLatcapxcddatcap.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)







ggplot(data = dailygr.analysis.cdd , aes(x=dayoflife, y=mmpermmperdy, group = Yearf, color = Yearf, fill = Yearf, linetype = hw)) +
  geom_smooth(se = F, size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  ylab("Growth, mm/mm/day")+
  xlab("Age")+
  theme(legend.text = element_text(size = 16))

ggsave("relgro_age.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)


ggplot(data = dailygr.analysis.cdd , aes(x=CDD, y=cum_OR, group = Yearf, color = Yearf, fill = Yearf, linetype = hw)) +
  geom_smooth(fill = "lightgrey", size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  ylab("Cum OR")+
  xlab("Cumulative degree day (CDD)")+
  theme(legend.text = element_text(size = 16))




ggplot(data = dailygr.analysis.cdd, aes(x=CDD, y=BC_estimate, group = Yearf, color = Yearf, linetype = hw)) +
  geom_smooth(se = F, size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  ylim(5,15)+
  ylab("Total length, mm")+
  xlab("Cumulative Degree Days")+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size = 16))

ggsave("BC_CDD.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)


ggplot(data = dailygr.analysis.cdd, aes(x=dayoflife, y=BC_estimate, group = Yearf, color = Yearf, linetype = hw)) +
  geom_smooth(se = F, size =2)+
  scale_color_manual(values=color2)+
  scale_linetype_manual(values = lines)+
  ylab("Total length, mm")+
  xlab("Age")+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size = 16))

ggsave("BC_age.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)

ggsave("yearlegend.jpg", plot = last_plot(), device = "jpg",
       width = 12, height = 8, units = "in", dpi = 300)

ggplot(data = dailygr.analysis.cdd, aes(x=dayoflife, y=CDD, ggroup = Yearf, color = Yearf, linetype = hw)) +
  geom_smooth( size =2)+
  scale_color_manual(values=color2)+
  theme_classic(base_size = 20)+
  theme(legend.position = "top")+
  xlab("Age")+
  theme(legend.text = element_text(size = 16))

ggsave("age_CDD.jpg", plot = last_plot(), device = "jpg",
       width = 6, height = 4, units = "in", dpi = 300)

