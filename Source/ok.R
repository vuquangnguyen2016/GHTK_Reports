# https://www.analyticsvidhya.com/blog/2019/05/beginner-guide-tidyverse-most-powerful-collection-r-packages-data-science/

rm(list = ls())
library(readxl)
library(dplyr)


dataRaw <- read_excel("Dataset/hoa_don_cod_20191218171546_5df9fc52-da8c-4e03-8ef4-b0120a0a026e.xlsx", 
                      sheet = "Hóa đơn COD")

dataRaw  %>%
  select(`SL ĐH`, Cod) %>%
  filter(`SL ĐH` >10)  %>% head()
