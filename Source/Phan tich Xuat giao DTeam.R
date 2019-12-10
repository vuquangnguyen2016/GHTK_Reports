# Import
library(dplyr)
library(ggplot2)
library(readxl)
library(hrbrthemes)
library(lubridate)
library(writexl)
library(tidyverse)

# ----------------------- Setup

rm(list = ls())

setwd("B:/Workstation/Dropbox/DataCenter/GHTK/GHTK_Reports/")
Sys.setlocale("LC_CTYPE", "English_United States.1258")



#---------------- Import
DataD <- read_excel("./Dataset/XuatGiao_November.xlsx", sheet = "Source")

# Drop missing value

  

DataD<-na.omit(DataD)
DataD<-DataD %>% filter(DataD$SoLuong>0)







# TInh san luong trung binh cua COD
DataD %>%
  group_by(MaCOD) %>%
  summarise_each(funs(sum), SoLuong) %>%
  arrange(desc(SoLuong)) -> DataDVolume
  
# Tinh trung binh san luong bill theo ngay
DataD %>%
  group_by(DateRec) %>%
  summarise_each(funs(sum), SoLuong) %>%
  arrange(desc(SoLuong))-> DataDDate

#------------ Shifts - Phien
# Represent it
DataD[,"Shifts"] <-  format(parse_date_time(DataD$DateRec, orders = "Ymd HMS"),"%H:%M:%S")


#-----------------
DataD %>%
  group_by(Shifts) %>%
  summarise_each(funs(sum),SoLuong) %>%
  arrange(desc(SoLuong)) -> DataDShifts
  
#------------ Export

sheets <- list("Source" = DataD, "Date" = DataDDate, "Volume" = DataDVolume, "Shifts" = DataDShifts)
write_xlsx(sheets, "./Export/Report_Xuatgiao_November.xlsx")


# Bieu do times-series



p <- ggplot(data = DataD2, aes(x=DateRec, y=SoLuong)) +
  geom_line( color="#69b3a2") + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=20, hjust=1)) 


p

#------------------------



p1<-ggplot(data=DataD3[1:10,], aes(x=MaCOD, y=SoLuong)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=SoLuong), vjust=-0.3, size=3.5)+
  theme_minimal()
p1


#--------------Report
DataD2 %>%
  summarise_each(funs(sum), SoLuong)

DataD3 %>%
  mutate(CODngay = mean(SoLuong,na.rm=FALSE)/20)



#------------------------------------------------ Exploring
DataD <- read.csv("B:/Workstation/Dropbox/DataCenter/GHTK/Python/DataD.csv")
summary(DataD$SoLuong)

DataD %>%
  group_by(DateRec) %>%
  arrange(desc(SoLuong)) -> DataD4
p<-ggplot(data = DataD4[1:6,], aes(x=DateRec, y=SoLuong))+
  geom_bar(stat="identity", fill="steelblue")
p
  
