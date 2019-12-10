library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyverse)

# ---------------------- Clean and Setup

rm(list = ls()) # Xoa du lieu 

setwd("B:/Workstation/Dropbox/DataCenter/GHTK/GHTK_Reports/")
Sys.setlocale("LC_CTYPE", "English_United States.1258")

#---------- Import data Pteam 16 - 22.11
DataP <- read_excel("./Dataset/DatasetPTeam_November.xlsx")

#----------- Missing Values
DataP <- na.omit(DataP)

#------------ Ratio Bill Pass and Success

DataP %>%
  mutate(RatioPass=BillPass/Volume, RationSuccess=BillSuccess/BillPass) -> DataP



# ------------- Report San luong (Volume) theo ngay


DataP %>%
  group_by(DateRec) %>%
  summarise_each(funs(sum),Volume) -> DataPDate



#-------------- Report San luong theo Shifts

DataP %>%
 group_by(Shifts) %>%
  summarise_each(funs(sum),Volume) -> DataPVolume

#-------------- Report Ti le ban giao va Thanh cong theo Shifts

DataP %>%
  group_by(Shifts) %>%
  summarise_each(funs(mean),RatioPass,RationSuccess)-> DataPRatio



#------------------------------- Ending













# Bieu do times-series


#--- Bieu do san luong theo ngay

p<-ggplot(aes(x = DateRec, y = Volume, group=1), data = DataPDate) + 
  geom_point()+ geom_line() 
p




#------------ Export Dataset


sheets <- list("Source" = DataP, "Date" = DataPDate, "Volume" = DataPVolume, "Ratio" = DataPRatio) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "./Export/Export_PTeam.xlsx")

