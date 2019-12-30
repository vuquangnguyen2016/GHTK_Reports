library(tidyverse)         
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(stringr)


# Import
rm(list = ls())
setwd("~/Dropbox/DataCenter/GHTK/GHTK_Reports/AnalysingVolume")
Source <- read_excel("Source_TrackingXe_Dec.xlsx")


pattern_date <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"

SourceD <- Source %>% 
  mutate(DateStamp = str_extract(Date, pattern_date)) %>% 
  group_by(DateStamp) %>% 
  summarise(TotalDelay=mean(Delay, na.rm = TRUE), RatioDelay=TotalDelay/15)


View(SourceD)

SourceCod <- Source %>% 
  group_by(Cod) %>% 
  summarise(TotalDelay=mean(Delay,na.rm=TRUE),RatioDelay=TotalDelay/15)
View(SourceCod)



ExportSheet_Q1<- list("Daily" = SourceD, "Cod" = SourceCod)
write_xlsx(ExportSheet_Q1,"ExportSheet_Tracking.xlsx")



plotD<-ggplot(data=SourceD, aes(x=DateStamp, y=RatioDelay)) +
  geom_segment( aes(x=DateStamp, xend=DateStamp, y=0, yend=RatioDelay), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + coord_flip() + 
  xlab("Ngay xe chay") +
  ylab("Ti le xe tre (Ratio Delay)")

ggsave("Plot Daily Delay Ratio.png")


plotCod<-ggplot(data=SourceCod, aes(x=Cod, y=RatioDelay)) +
  geom_segment( aes(x=Cod, xend=Cod, y=0, yend=RatioDelay), color="grey") +
  geom_point( color="blue", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + coord_flip() + 
  xlab("") +
  ylab("Ti le xe tre (Ratio Delay)")


ggsave("Plot Cod delay.png")
