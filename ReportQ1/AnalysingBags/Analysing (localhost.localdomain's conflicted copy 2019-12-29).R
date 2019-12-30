library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)


rm(list = ls())
setwd("~/Dropbox/DataCenter/GHTK/GHTK_Reports/ReportQ1/AnalysingBags")

# -------- Kho BT2 -> BC NCT


DataB1 <- read_csv("./Data25122019/DataB1.csv")
DataB2 <- read_csv("./Data25122019/DataB2.csv")
DataNCT <- read_csv("./Data25122019/DataNCT.csv")
DataNVC <- read_csv("./Data25122019/DataNVC.csv")

DataTracking<-read_excel("./Data25122019/Tracking.xlsx",sheet = "Tracking")

pattern_time <- "[0-9]{2}:[0-9]{2}"
pattern_date <- "[0-9]{2}-[0-9]{2}-[0-9]{2}"

DataTracking_Ex <- DataTracking %>% 
  mutate(DatelyStamp = str_extract(Timestamp,pattern_date), TimeTracking=str_extract(Timestamp,pattern_time)) %>% 
  rename(delay = "Độ delay")

DataTracking_Ex2 <- DataTracking_Ex %>% 
filter(DatelyStamp == "19-12-25") %>% 
  group_by(Stops) %>% 
  summarise(TotalDelay=mean(delay,na.rm=TRUE),RatioDelay=TotalDelay/15, Total_Volume=sum(Volume,na.rm = TRUE))
# -------- Kho BT2 -> BC NVC


p <- ggplot(data=DataTracking_Ex2, aes(x=Stops, y=Total_Volume,group=1)) +
  geom_line(linetype="dashed",color="blue")+ geom_point()
p         


p <- ggplot(data = DataTracking_Ex2, aes(x = Stops,group=1)) + 
  geom_line(aes(y = Total_Volume, colour = "Tong San Luong")) + 

DataTracking_Ex %>%
  filter(DatelyStamp=='19-12-25') 

    



