# Import
library(dplyr)
library(ggplot2)
library(readxl)
library(hrbrthemes)
library(lubridate)


Source <- read_excel("B:/Workstation/Dropbox/DataCenter/GHTK/Python/XuatGiao_November.xlsx", 
                                sheet = "Source")
# View(XuatGiao_November)

# Drop missing value

  

Source<-na.omit(Source)
Source<-Source %>% filter(Source$SoLuong>0)
#data(Source)
View(Source)

# Export dataframe

write.csv(Source,"B:\\Workstation\\Dropbox\\DataCenter\\GHTK\\Python\\Source.csv", row.names = FALSE)


# Test
Source23<-Source %>% filter( Source$DateRec == "2019-11-23 14:00:00")
View(Source23)

# Filter theo ngay

Source %>%
  select(SoLuong, MaCOD) %>%
  arrange(desc(SoLuong)) %>%
  head(10)

# TInh san luong trung binh cua COD
Source %>%
  group_by(MaCOD) %>%
  summarise_each(funs(sum), SoLuong) %>%
  arrange(desc(SoLuong)) -> Source3
  
# Tinh trung binh san luong bill theo ngay
Source %>%
  group_by(DateRec) %>%
  summarise_each(funs(sum), SoLuong) %>%
  arrange(desc(SoLuong))-> Source2

#------------ Phien
# Represent it
Source[,"Phien"] <-  format(parse_date_time(Source$DateRec, orders = "Ymd HMS"),"%H:%M:%S")


#-----------------
Source %>%
  group_by(Phien) %>%
  summarise_each(funs(sum),SoLuong) %>%
  arrange(desc(SoLuong)) -> Source5
  

View(Source5)
# Plot Data

# Bieu do times-series



p <- ggplot(data = Source2, aes(x=DateRec, y=SoLuong)) +
  geom_line( color="#69b3a2") + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=20, hjust=1)) 


p

#------------------------



p1<-ggplot(data=Source3[1:10,], aes(x=MaCOD, y=SoLuong)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=SoLuong), vjust=-0.3, size=3.5)+
  theme_minimal()
p1


#--------------Report
Source2 %>%
  summarise_each(funs(sum), SoLuong)

Source3 %>%
  mutate(CODngay = mean(SoLuong,na.rm=FALSE)/20)



#------------------------------------------------ Exploring
Source <- read.csv("B:/Workstation/Dropbox/DataCenter/GHTK/Python/Source.csv")
summary(Source$SoLuong)

Source %>%
  group_by(DateRec) %>%
  arrange(desc(SoLuong)) -> Source4
p<-ggplot(data = Source4[1:6,], aes(x=DateRec, y=SoLuong))+
  geom_bar(stat="identity", fill="steelblue")
p
  
