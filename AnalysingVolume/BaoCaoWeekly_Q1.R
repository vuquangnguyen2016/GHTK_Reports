library(tidyverse)         
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(stringr)
library(viridis)

#--    Setting

# Import
rm(list = ls())
setwd("~/Dropbox/DataCenter/GHTK/GHTK_Reports/AnalysingVolume")
NameFile='hoa_don_cod_20191221093913_5dfd85d1-9fac-43d3-a150-5a1f0a0a026e.xlsx'

RawBill <-read_excel(NameFile,  sheet = "Hóa đơn COD")
CODlist<- read_excel("CodQ1.xlsx")


RawD1 <- right_join(RawBill,CODlist,by="Cod")

# Filter Volume ; Success ; Delay and 

# Rename Colume -> English

RawD1<-RawD1 %>% rename(Volume="SL ĐH", Success = "SL ĐH đã đối soát", Delay="SL ĐH delay", Returned="SL ĐH đã trả", TimeStamp="TG tạo", Money = "Tổng tiền thu") 

# filter theo CART
Extract_Cart <-RawD1 %>% 
  select(Cart, Cod,Volume, Success, Delay, Returned) %>% 
  group_by(Cart) %>%
  arrange(desc(Volume),desc(Success), Delay) %>% 
  summarise(
    TotalVolume=sum(Volume, na.rm = TRUE), 
    TotalSuccess=sum(Success,na.rm = TRUE), 
    TotalDelay=sum(Delay, na.rm = TRUE), 
    TotalReturned=sum(Returned,na.rm = TRUE),
    RatioSuccess = TotalSuccess/TotalVolume 
  )

#-------------- Dately

pattern_date <- "[0-9]{2}-[0-9]{2}-[0-9]{2}"
pattern_time <- "[0-9]{2}:[0-9]{2}"



RawD1<- RawD1 %>% 
  mutate(DatelyStamp = str_extract(TimeStamp,pattern_date))


Extract_Dately <- RawD1 %>% 
  group_by(DatelyStamp) %>% 
  summarise(
    TotalVolume_Dately = sum(Volume,na.rm = TRUE),
    TotalDelay_Dately = sum(Delay,na.rm = TRUE),
    TotalSuccess_Dately = sum(Success,na.rm = TRUE),
    TotalReturned_Dately = sum(Returned,na.rm = TRUE),
    Ratio_Success_Dately = TotalSuccess_Dately/TotalVolume_Dately    
  ) %>% drop_na()


Extract_Dately_Cart <- RawD1 %>% 
  group_by(DatelyStamp,Cart) %>% 
  summarise(
    TotalVolume_Dately = sum(Volume,na.rm = TRUE),
    TotalDelay_Dately = sum(Delay,na.rm = TRUE),
    TotalSuccess_Dately = sum(Success,na.rm = TRUE),
    TotalReturned_Dately = sum(Returned,na.rm = TRUE),
    Ratio_Success_Dately = TotalSuccess_Dately/TotalVolume_Dately    
  ) %>% drop_na()



#----------- Export dateframe

ExportSheet_Q1<- list("Cart" = Extract_Cart, "Dately" = Extract_Dately, "DatelyCart"= Extract_Dately_Cart  )

write_xlsx(ExportSheet_Q1,"ExportSheet_Q1.xlsx")

# Xuat Plot theo Cart
plotCart<-ggplot(data=Extract_Cart, aes(x=Cart, y=TotalVolume)) +
  geom_segment( aes(x=Cart, xend=Cart, y=0, yend=TotalVolume), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + coord_flip() + 
  xlab("Cart") +
  ylab("Tong san luong theo CART")

plotCart


plotDately<-ggplot(data=Extract_Dately, aes(x=DatelyStamp, y=TotalVolume_Dately)) +
  geom_segment( aes(x=DatelyStamp, xend=DatelyStamp, y=0, yend=TotalVolume_Dately), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + coord_flip() + 
  xlab("DatelyStamp") +
  ylab("Tong san luong theo Dately")


plotDately
