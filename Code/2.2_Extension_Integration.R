### Integrate CNB and self calculation NFP extension
### 选择两者中较大的值/平均值，并且统一整合为leaching和volatisation格式

##Initial setting
library(Matrix)
library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(zoo)
library(readr)


# Delete
rm(list=ls()); gc()


# Read both extensions 
# E1 is based on IPCC N model, where specific emission type are aggregated as volatilisation and leaching respectively
E1 <- fread("~/Nitrogen Extended FABIO/Results/Production_NFP_extension_0130.csv")
# E2 is data from CNB
E2 <- fread("~/Nitrogen Extended FABIO/Results/Production_NFP_extension_CNB.csv")

# Distinguish between leaching and volatilisation
# According to Data description from FAOSTATA, ammonia (NH3) leaching or runoff (Nwp) to water bodies
# volatilisation of nitrous oxides, nitrates and NOx


# Aggregate results for both extensions
E22 <- E2 %>% 
  group_by(area_code, Year) %>% 
  summarize(CNBL = sum(Leaching), CNBV = sum(Volatilisation))

E11 <- E1 %>% 
  group_by(area_code,Year) %>% 
  filter(between(Year, 1986,2020)) %>% 
  summarize(IPCCL = sum(NOx_NH3), IPCCV = sum(Nwp+NO2))

# Compare the two aggregated extensions 
comparison <- merge(E11,E22, by = c('area_code','Year'),all.x =TRUE)
com1 <- mutate(comparison, diff1 = CNBL-IPCCL, diff2 = CNBV-IPCCV)


# Statistics Summary
install.packages(c("psych")).
library(psych)
describe(mydata)


# Use the average of the two datasets 

IPCCE <- E1 %>% 
  mutate(IPCCL = NOx_NH3, IPCCV = Nwp+NO2,TN_IPCC = TN) %>% 
  filter(between(Year, 1986,2020)) %>% 
  select(area_code,Year,comm_code,IPCCL,IPCCV,TN_IPCC)
CNBE <- E2 %>% 
  mutate(CNBL = Leaching, CNBV = Volatilisation, TN_CNB = CNBL + CNBV) %>% 
  select(area_code,Year,comm_code, CNBL,CNBV,TN_CNB)

Extension <- merge(CNBE,IPCCE,by=c('area_code','Year','comm_code'),all.x = TRUE)
Extension_average <- Extension %>% 
  mutate(Leaching = (CNBL+IPCCL)/2, Volatilization = (CNBV+IPCCV)/2,TN = Leaching+Volatilization) %>% 
  select(area_code,Year,comm_code,Leaching, Volatilization,TN)

arrange(Extension_average, area_code, Year, comm_code)  

write.csv(Extension_average,"~/Nitrogen Extended FABIO/Results/Production_NFP_extension_average.csv")
