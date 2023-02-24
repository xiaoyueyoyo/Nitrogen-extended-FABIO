###################################################################
##         Direct N Leaching from household consumption          ##
###################################################################


# Initial setting 
library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
# library(wbstats)
rm(list=ls()); 
gc()

# Read Food Balance data
FB <- fread("~/Nitrogen-extended-FABIO/Data/FB.csv")


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
FB_FABIO_regionmatch <- read_xlsx("~/Nitrogen-extended-FABIO/Data/FB_FABIO_regionmatch.xlsx")
FB_FABIO_itemmatch <- read_xlsx("~/Nitrogen-extended-FABIO/Data/FB_FABIO_itemmatch.xlsx")


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
  filter(is.na(item_code) == FALSE) %>% 
  filter(is.na(area_code) == FALSE) %>% 
  mutate(Nremoval = 0, Waste = 0, Nwp_hh = nratio*1000*P*Value*(1-Waste)*(1-Nremoval)*365/10e6, NUnit = "tonnes") %>% 
  select(area_code, area = Area.y, Year, item_code, item = Item.y,  protein = Value, population = P, Waste, Nremoval, Nwp_hh, NUnit)
# Modify negative value
NFP_hh$Nwp_hh[NFP_hh$protein<0]=0


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

# t <- aggprotein %>% 
#  filter(area_code == 276)
# Create an extrapolation function based on recurring averaged difference 

# Issue solved: Controlled decimal and solve negative values 
extrapolate <- function(t) {
  # replace na values in t$pro as 0
  t$pro[is.na(t$pro)] <-  0
  # derive difference sequence for t$pro using diff()
  difft <-  diff(t$pro)
   # get subset of nonzero values 
  x = difft[difft!=0] 
   # get subset from subset 
  y = x[2:(length(x)-1)]
  a = length(y)%/%2 
  forward <- round(mean(y[1:(a+1)]),2) # average of first half diffs
  backward <- round(mean(y[(a+1):length(y)]),2)  # average of second had diffs
  # Locate the position of non-zero values
  indx <- which(t$pro!=0)
  # Fill the previous prediction; fill the later prediction
  pre <- seq(t$pro[indx[1]]-forward,by = -forward, length.out = indx[1]-1)
  pro <- seq(t$pro[indx[length(indx)]]+backward,by = backward, length.out = 35-indx[length(indx)])
  t$pro[t$Year %in% seq(1986,by=1,length.out =indx[1]-1)] = rev(pre) 
  t$pro[t$Year %in% t$Year[(tail(indx,1)+1):35]]= pro
  return (t)
}


# Note: stat functions like mean(x, na.rm = TRUE) has option of na.rm = TRUE to exclude NA from calculation


# Create an extrapololation function based on linear modelling
# extrapolate <- function(fill) {
#  fit <- lm(pro~Year,fill)
#  na <- fill[is.na(fill$pro), "Year", drop = FALSE] 
#  newpro <- predict(fit,newdata=na)
#  na <- cbind(na,pro=newpro)
# fill$pro[fill$Year %in% c(1986:2009,2020)] = na$pro  
#  return(fill)
# }


# Split the long data based on area code, return a list of tibbles
data_list <- aggprotein %>%
   group_split(area_code)
# data_list <- split(aggprotein, f = aggprotein$area_code)   
newdata <- sapply(data_list,extrapolate)

# convert matrix of lists into dataframe
x = data.frame()
reform <- function(df) {
  df <- asplit(df,2)
  for (i in 1:dim(df)){
  a <- df[[i]]
  b <-  as.data.frame(do.call(cbind, a))
  x <- rbind(x,b)
  }
return(x)
}
predict <- reform(newdata)



# Distribute nitrogen according to crop harvested area


# Derive full year range household consumption N emission







# Save results
write.csv(NFP_consumption,"~/Nitrogen Extended FABIO/Results/NFP_consumption.csv")



# Issues:
# 1. Very limited protein supply data before 2010 from FAPSTAT.
# 2. Limited food waste data
# 3. Limited nitrogen removal data 

# Significance of Consumption footprint
# 1. Distinguish between direct emission from consumption and production nitrogen footprint
# 2. Analyse the impact of domestic food consumption preference on local nitrogen pollution






