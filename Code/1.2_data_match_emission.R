### 处理数据，合并数据
# 直接利用CNB数据中的leaching和volatisation数据进行作物分配
# 原因是经过比对，CNB一国农业氮排放的数据比自己计算的更大，说明更具细节

##Initial setting
library(Matrix)
library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(zoo)


# Delete
rm(list=ls()); gc()


# 0. 导入CNB、QCL数据和FABIO regions, items

CNB <- read_csv("~/Nitrogen Extended FABIO/Nutrient_Budget_Data/CNB.csv")  #Area, Nutrient Inputs
QCL <- read_csv("~/Nitrogen Extended FABIO/QCL/QCL.csv")   #Area, Crop Item
regions <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/regions.csv")
items <- read_csv("/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")


# 0.1 设置可修改参数

nutrients <- c("Cropland nitrogen",'Cropland phosphorus', 'Cropland potassium')
nutrient <- nutrients[1]  # 1 Refer to Crop nitrogen
ninputs <- unique(CNB$Item)
#1-5:"Synthetic Fertilizers" "Manure applied to Soils" "Atmospheric Deposition" "Crop Removal" "Biological Fixation"
#6-10："Leaching" "Volatilisation" "Input" "Outputs" "Soil nutrient budget"   
#ninput <- ninputs[1:2] # Refer to Synthetic Fertilizers and Manure applied to Soils
nemission <- ninputs[6:7] # Refer to leaching and volatilisation
cinfos <- c("Area harvested","Yield","Production")
cinfo <- cinfos[1]   #refer to Area harvested

gc()

# 1. 将CNB中的国家与QCL,FABIO对应

#1.0 获取CNB，QCL地区信息

QCL_regions <- unique(QCL$Area)
write.csv(QCL_regions,"~/QCL_regions.csv")


# 1.1 CNB，QCL 和 FABIO 地区协调

library(readxl)
CNB_FABIO_reigonmatch <- read_excel("~/Nitrogen Extended FABIO/Code/CNB_FABIO_reigonmatch.xlsx")
QCL_FABIO_reigonmatch <- read_excel("~/Nitrogen Extended FABIO/Code/QCL_FABIO_regionmatch.xlsx")


# 1.2 获取CNB中的有效信息

#进行区域匹配
CNB_FABIO <- CNB %>% 
  filter(Item ==  nemission[1]|Item == nemission[2]) %>% 
  filter(Element == paste0(nutrient)) %>% 
  merge(CNB_FABIO_reigonmatch, by.x = c('Area'), by.y = c('CNB'), all.x = TRUE) %>% 
  filter(is.na(area_code) == "FALSE") %>% 
  group_by(area_code,Year,Item) %>% 
  summarize(area = FABIO, area_code, Type = Item, Nutrient = Element, Year, Emission = sum(Value), Unit) %>% 
  distinct(area_code,Year,Emission,Unit) 

#由于多个国家都称为RoW,所以要使用distinct确保每一年，每一个区域的总氮投入是独特的。



# 2. 将QCL中的item与FABIO item 和item code进行match

QCL_FABIO_itemmatch <- read_excel("~/Nitrogen Extended FABIO/Code/QCL_FABIO_itemmatch.xlsx")

# 2.1 获取QCL中的有效信息

#进行区域匹配
QCL_FABIO <- QCL %>% 
  filter(Element == paste0(cinfo)) %>% 
  merge(QCL_FABIO_reigonmatch, by.x = c('Area'), by.y = c('QCL'), all.x = TRUE) %>% 
  filter(is.na(area_code) == "FALSE") %>% 
  group_by(area_code, Item, Year) %>% 
  summarise(area = FABIO, area_code, Year, Item, HA = sum(Value)) %>% 
  distinct(area_code, Year, Item, HA)


# 作物匹配
QCL_FABIO_item <- QCL_FABIO %>% 
  merge(QCL_FABIO_itemmatch, by = c('Item'), all.x = TRUE) %>% 
  filter(is.na(comm_code) == "FALSE") %>% 
  filter(Year <= 2020 & Year >= 1986) %>% 
  group_by(area_code, Year, comm_code) %>% 
  summarise(area_code, Year, comm_code, HA = sum(HA)) %>% 
  distinct(area_code, Year, comm_code, HA)
gc(0)

# 查看是否有缺失值
sum(is.na(QCL_FABIO_item))  #数据中没有缺失值，只是有的年份作物收割为0


# 将QCL,CNB和FABIO合并起来
QCL_CNB_FABIO <- QCL_FABIO_item %>% 
  merge(CNB_FABIO,by = c('area_code','Year'), all.x = TRUE) 
QCL_CNB_FABIO <- distinct(QCL_CNB_FABIO)
# write.csv(QCL_CNB_FABIO, "~/Nitrogen Extended FABIO/Code/QCL_CNB_FABIO.csv")



# 根据HA分配Leaching和volatilisation, 获得extension
Distribution <- QCL_CNB_FABIO %>% 
  group_by(area_code,Year,Item) %>% 
  mutate(Total_AH = sum(HA), ratio = HA/Total_AH, Emission = ratio*Emission)



# 创建extension
Leaching <- Distribution %>% 
  filter(Item == "Leaching") %>% 
  select(area_code, Year, comm_code, Leaching = Emission)
Volatilisation <- Distribution %>% 
  filter(Item == "Volatilisation") %>% 
  select(area_code, Year, comm_code, Volatilisation = Emission)
Extension <- merge(Leaching[,-1], Volatilisation[,-1], by = c('area_code','Year','comm_code'))
write.csv(Extension,"~/Nitrogen Extended FABIO/Results/Production_NFP_extension_CNB.csv")








