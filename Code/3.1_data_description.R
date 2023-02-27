###################################################################
##              Description of protein supply data               ##
###################################################################


# Initial setting 

library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)
library(DT)
# library(wbstats)
rm(list=ls()); 
gc()

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Read Food Balance data
FB <- fread("~/nitrogen_fabio/Data/FB.csv")


# Filter protein supply and population data from FB
protein <- FB %>% 
  as_tibble() %>% 
  filter(Element == "Protein supply quantity (g/capita/day)") %>% 
  select(-c("Area Code (M49)","Item Code (CPC)", "Element Code", "Year Code", "Flag")) 
population <- FB %>% 
  filter(Element == "Total Population - Both sexes") %>% 
  mutate(P = Value, PUnit = Unit) %>% 
  select(-c("Area Code (M49)","Item Code (CPC)", "Element Code", "Year Code","Flag","Value","Unit","Item","Item Code","Element"))


# Derive FB unique region and item 
# FB_region  <-  unique(protein[c("Area","Area Code")])
# FB_item <- unique(protein[c("Item","Item Code")])
# write.csv(FB_item,"~/FB_item.csv")
# write.csv(FB_region, "~/FB_region.csv")


# Read region match file and item match file to integrate FB and FABIO
FB_FABIO_regionmatch <- read_xlsx("~/nitrogen_fabio/Data/FB_FABIO_regionmatch.xlsx")
FB_FABIO_itemmatch <- read_xlsx("~/nitrogen_fabio/Data/FB_FABIO_itemmatch.xlsx")
items <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")

# Derive food Waste data
# Limited data availability 


# Derive NRemoval data
# Limited data availability 


# Merge dataset and derive household consumption NFP
merg <- merge(protein,population,by = c("Area Code","Area","Year"), all.x =TRUE)
# Nitrogen accounts for 16% in protein
nratio <- 0.16
NFP_hh <- merg %>% 
  merge(FB_FABIO_regionmatch,by = c('Area Code')) %>% 
  merge(FB_FABIO_itemmatch, by = c('Item Code')) %>% 
  merge(items, by.x = c('Item Code'), by.y = c('item_code')) %>% 
  filter(is.na(item_code) == FALSE) %>% 
  filter(is.na(area_code) == FALSE) %>% 
  mutate(Nremoval = 0, Waste = 0, total_protein = Value*P*1000*365/10e6, Nwp_hh = nratio*total_protein*(1-Waste)*(1-Nremoval), NUnit = "tonnes") %>% 
  select(area_code, area = Area.y, Year, item_code, comm_group, item = Item.y, unit_protein = Value, population = P, total_protein , Waste, Nremoval, Nwp_hh, NUnit)
# Modify negative value
NFP_hh$Nwp_hh[NFP_hh$protein<0]=0


# Graph1: Food protein supply of world from 2010-2020
graph1 <- NFP_hh %>% 
  group_by(comm_group,Year) %>% 
  summarise(food_protein = sum(total_protein))  %>% 
  mutate(lag = lag(food_protein),diff = food_protein-lag, rate = 100*diff/food_protein)
 
graph1 %>% 
  ggplot(aes(x=Year,y=food_protein,group = comm_group, fill = comm_group))+
  geom_area()+
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.position="none") +
  ggtitle("Food protein in the previous 10 years") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~comm_group)
















# Extrapolate protein supply data for missing years 
area_list <- unique(NFP_hh[c("area_code","area")])
rd <- nrow(area_list)
df <- data.frame(area_code = rep(area_list$area_code,each = 35), area = rep(area_list$area,each = 35), Year = rep(seq(1986,2020,1),rd))
# aggregate protein data for each region each year 
aggprotein <- NFP_hh %>% 
  group_by(area,Year) %>% 
  summarise(area_code, area, Year, pro = sum(protein),population = population*1000) %>% 
  merge(df, by=c('area_code','area','Year'),all.y = TRUE) %>% 
  distinct()




