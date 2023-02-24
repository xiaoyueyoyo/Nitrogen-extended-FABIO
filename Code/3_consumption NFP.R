### Direct Nwp from household consumption ###


library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
# library(wbstats)

rm(list=ls()); 
gc()


# Read protein and population data from FB
FB <- fread("~/Nitrogen Extended FABIO/Food Balance/FB.csv")
protein <- FB %>% 
  as_tibble() %>% 
  filter(Element == "Protein supply quantity (g/capita/day)") %>% 
  select(-c("Area Code (M49)","Item Code (CPC)", "Element Code", "Year Code", "Flag")) 
population <- FB %>% 
  filter(Element == "Total Population - Both sexes") %>% 
  mutate(P = Value, PUnit = Unit) %>% 
  select(-c("Area Code (M49)","Item Code (CPC)", "Element Code", "Year Code","Flag","Value","Unit","Item","Item Code","Element"))


# Derive FB unique region and region code based on multiple variables
FB_region  <-  unique(protein[c("Area","Area Code")])
write.csv(FB_region, "~/FB_region.csv")
FB_FABIO_regionmatch <- read_xlsx("~/Nitrogen Extended FABIO/Code/FB_FABIO_regionmatch.xlsx")

# Derive FB unique item and item code 
FB_item <- unique(protein[c("Item","Item Code")])
write.csv(FB_item,"~/FB_item.csv")
FB_FABIO_itemmatch <- read_xlsx("~/Nitrogen Extended FABIO/Code/FB_FABIO_itemmatch.xlsx")



# Read protein and population data before 2010
# FBold <- fread("~/Nitrogen Extended FABIO/Code/FBold.csv")
# protein <- FBold %>% 
#  as_tibble() %>% 
#  filter(Element == "Protein supply quantity (g/capita/day)") %>% 
#  filter(between(Year, 1986,2009)) 
 
# FB_region  <-  unique(protein[c("Area")])  # 数据过少没有意义



# Derive food Waste data




# Derive NRemoval data





# Merge dataset 
merg <- merge(protein,population,by = c("Area Code","Area","Year"), all.x =TRUE)

NFP_consumption <- merg %>% 
  merge(FB_FABIO_regionmatch,by = c('Area Code')) %>% 
  merge(FB_FABIO_itemmatch, by = c('Item Code')) %>% 
  filter(is.na(item_code) == FALSE) %>% 
  filter(is.na(area_code) == FALSE) %>% 
  mutate(Nremoval = 0, Waste = 0, Nwp_c = 0.16*P*Value*(1-Waste)*(1-Nremoval)*365/1000, NUnit = "tonnes") %>% 
  select(area_code, Area = Area.y, Year, item_code, Item = Item.y,  protein = Value, population = P, Waste, Nremoval, Nwp_c,NUnit)
# Modify negative value
NFP_consumption$Nwp_c[NFP_consumption$protein<0]=0


# Extrapolate data before 2010

x <- data.frame(area_code = rep(unique(NFP_consumption$area_code),each = 35), Year = rep(seq(1986,2020,1),177))
aggprotein <- NFP_consumption %>% 
  group_by(Area,Year) %>% 
  summarise(area_code, Area, Year, pro = sum(protein)) %>% 
  merge(x, by=c('area_code','Year'),all.y = TRUE) %>% 
  distinct

extrapolate <- function(fill) {
  fit <- lm(pro~Year,fill)
  na <- fill[is.na(fill$pro), "Year", drop = FALSE] 
  newpro <- predict(fit,newdata=na)
  na <- cbind(na,pro=newpro)
  fill$pro[fill$Year %in% c(1986:2009,2020)] = na$pro  
  return(fill)
}

#preprotein <- data.frame()
#for (i in aggprotein$area_code){
#  fill <-  filter(aggprotein,area_code == i) 
#  extrapolate(fill) 
#  fill <-  rbind(preprotein,fill)
#}

# Split the long data based on area code, return a list of tibbles
data_list <- aggprotein %>%
   group_split(area_code)
# data_list <- split(aggprotein, f = aggprotein$area_code)   
newdata <- sapply(data_list,extrapolate)
as.data.frame(newdata)



# Save results
write.csv(NFP_consumption,"~/Nitrogen Extended FABIO/Results/NFP_consumption.csv")


# 目前问题
# 1. FAO蛋白质供应数据没有2010年之前的（之间找Martin和Stefan问）。原来的数据就是不全。
# 2. 没有food waste信息。没关系。
# 3. 没有nitrogen removal信息。没关系。


# Consumption footprint重要性
# 可以区分一国的氮污染主要来源：满足自身消费的生产，满足他国消费的生产，国民的直接消费
# 可以分析一国消费的氮污染影响：国民的消费对本国造成的污染，国民的消费对他国造成的污染

# 没有必要考虑food waste，10年之前，以及nitrogen removal了。因为主要是不同种类农产品消费趋势的比较，绝对值不存在意义。





