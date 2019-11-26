library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyverse)
#-------------Encoding
setwd("B:/Workstation/Dropbox/DataCenter/GHTK/GHTK_Reports/")

Sys.setlocale("LC_CTYPE", "English_United States.1258")




#--------------
Data<-read_excel("./Dataset/hoa_don_cod_20191126132120_5ddcc460-c22c-45b4-8d4a-4fce0a0a3429.xlsx")

Data <- read_excel("B:/Workstation/Dropbox/DataCenter/GHTK/PTeam/hoa_don_cod_20191126132120_5ddcc460-c22c-45b4-8d4a-4fce0a0a3429.xlsx")

Data %>%
  group_by(Cod) %>%
  summarise_each(funs(sum),"SL ĐH",  "SL ĐH đã đối soát", "SL ĐH delay",  "SL ĐH đã trả") -> ReportPTeamNov

#------------- Export
Sheets <- list("Report PTeam thang 11" = ReportPTeamNov)
write_xlsx(Sheets,"B:/Workstation/Dropbox/DataCenter/GHTK/PTeam/Report PTeam November.xlsx")