
#Final code for Miller et al. Older and slower: unexpected effects of marine heatwaves on larval fish.
#06/04/2024 with R version 4.2.2 (2022-10-31 ucrt)


library(rstudioapi)
library(vctrs)
library(tidyverse)
library(lme4)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(plyr)
library(nlme)
library(car)
library(gvlma)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in


dailygr_alltemp_plus<- read.csv("2_individuals.csv") %>% 
  mutate(yr_f = as.factor(yr_f)) %>% 
  filter(YEAR != 2008 & YEAR != 2011)

unique(dailygr_alltemp_plus$YEAR)

samplesizecheck <- dailygr_alltemp_plus %>% 
  filter(dayoflife == 1)

graph_no08_11_surface <- dailygr_alltemp_plus %>% 
dplyr::filter(dayoflife >5)

ggplot(data = graph_no08_11_surface , aes(x= doy, y=TempApplied, group = yr_f, color = yr_f, fill = yr_f)) +
  geom_smooth(line =5)+
  theme_classic(base_size = 22)+
  scale_color_manual(values=c("blue",  "darkblue", "steelblue2", "turquoise4", "firebrick4","orange","#FF3300"))+
  scale_fill_manual(values = c( "blue",   "darkblue", "steelblue2", "turquoise4", "firebrick4","orange","#FF3300"))+
  theme(legend.position = "top")+
  ylab("Temperature")+
  xlab("Day of Year")+
  theme(legend.text = element_text(size = 22))

ggsave("Surface_ExpTemp.jpg", plot = last_plot(), device = "jpg",
       width = 8, height = 6, units = "in", dpi = 300)

graph_no08_11_deep <- dailygr_alltemp_plus  %>% 
  dplyr::filter(dayoflife <6)


ggplot(data = graph_no08_11_deep , aes(x= doy, y=TempApplied, group = yr_f, color = yr_f, fill = yr_f)) +
  geom_smooth()+
  theme_classic(base_size = 22)+
  scale_color_manual(values = c("blue",  "darkblue", "steelblue2", "turquoise4", "firebrick4","orange","#FF3300"))+
  scale_fill_manual(values = c( "blue",   "darkblue", "steelblue2", "turquoise4", "firebrick4","orange","#FF3300"))+
  theme(legend.position = "top")+
  ylab("Temperature")+
  xlab("Day of Year")+
  theme(legend.text = element_text(size = 22))

ggsave("Deep_ExpTemp.jpg", plot = last_plot(), device = "jpg",
       width = 8, height = 6, units = "in", dpi = 300)






