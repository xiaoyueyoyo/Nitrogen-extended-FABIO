#########################################################################
##              Nitrogen extended FABIO based on CNB data              ##
#########################################################################


library(Matrix)
library(tidyverse)
library(data.table)
library(readxl)
library(xlsx)
library(dplyr)
library(zoo)
library('stringr')
# library(wbstats)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

#-------------------------------------------------------------------------
# Make initial settings
#-------------------------------------------------------------------------
setwd("~/Nitrogen Extended FABIO/EFP")
# read region classification from online
region <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/regions.csv")
regionclass <- fread('~/Nitrogen Extended FABIO/Code/regionclass.csv')
regions <- merge(region,regionclass, by=c('area_code','area'))





# read commodity classification
# items <- fread("~/Nitrogen Extended FABIO/Data/items.csv")
items <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$area_code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$area, each = nrcom),
                    continent = rep((regions$continent), each = nrcom),
                    item_code = rep(items$item_code, nrreg),
                    comm_code = rep(items$comm_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = c(items$comm_group[1:14], rep("others", 30), items$comm_group[45:47], rep("others", 5), 
                              rep("nonfood",10), rep("others", 2), items$comm_group[65:80], rep("nonfood",9), 
                              rep("alc",4), rep("nonfood",15), "dairy", "dairy", "eggs", "nonfood", 
                              rep("meat", 6), rep("nonfood", 4), "fish, seafood"))



#-------------------------------------------------------------------------
# Calculate detailed nitrogen footprints for all years and all countries
#-------------------------------------------------------------------------


consumption = "food"
allocation = "value" 
yr  <-  2020 # 以一年为例
years <- c(1990,1995,2000,2005,2010,2015,2020)
years <- 1986:2020

# Read data from shared directory: /
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds")   
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
index_full <- as.data.table(reshape::expand.grid.df(index, data.table(year = years)))


# Add Population
faop <- fread("~/Nitrogen Extended FABIO/Code/FAOSTAT_population.csv")
pop <- fread("~/Nitrogen Extended FABIO/Code/pop_FABIO_region match.csv")
popm <- merge(faop,pop, by = ('Area Code (M49)'),all.x = TRUE)
popm1 <- popm %>% 
  mutate(population = 1000*Value) %>% 
  select(Year, area_code, population) %>% 
  filter(area_code != '999')
world <- read_xlsx("~/Nitrogen Extended FABIO/Code/world.xlsx", n_max = 35)


#Add extension to FABIO 
P <- fread("~/Nitrogen Extended FABIO/Results/Production_NFP_extension_average.csv")
P <- merge(index_full, P[,-1], by.x = c("comm_code", "code", "year"), by.y = c("comm_code", "area_code", "Year"), all.x = TRUE, sort = FALSE)
# setkey(P, Year, code, comm_code)



for(yr in years){
  
  
  Li <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/",yr,"_L_",allocation,".rds"))
  Xi <- as.vector(X[, as.character(yr)])  
  Yi <- as.matrix(Y[[as.character(yr)]])  
  Pi <- P[year == yr,]
  popi <- popm1[Year == yr,]
  
  Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
  Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$area_code)]
  Y_codes$region = regions$region[match(Y_codes$iso3c,regions$iso3c)]
  Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)
  
  colnames(Yi) <- Y_codes$iso3c
  Yi <- Yi[, Y_codes$fd == "food"]
  
  
  ext <- Pi[,.(Leaching, Volatilization,TN)]/ as.vector(Xi)
  ext[!is.finite(ext)] <- 0
  
  
  
  

  
  #----------------------------------------------------------
  # Calculate nitrogen emissions summary for country and year 
  #----------------------------------------------------------
  
  MP <- t(ext) %*% as.matrix(Li)
  # ext is nitrogen pollution extension, k = 4, 123 commodities *192 regions。
  # *is element-wise (here row-wise) multiplication. Matrix multiplication is %*%
  FP_country <- t(MP %*% as.matrix(Yi))   
  gc()
  FP_summary <- FP_country %>% 
    as_tibble() %>% 
    mutate(area = regions$area, region = regions$region, iso3c = regions$iso3c, area_code = regions$area_code) 
  worldi  <-  world[yr-1986+1,]
  FP_summary1 <- FP_summary %>% 
    merge(popi, by = c('area_code'))   # Without all.x = TRUE, exclude ROW and missing data countries 
  app = data.frame(area_code = "1000", Leaching = sum(FP_summary1$Leaching),
                   Volatilization = sum(FP_summary1$Volatilization),
                   TN = sum(FP_summary1$TN),area = "World", region = "World", iso3c = "WRD",
                   Year = yr, population = as.numeric(worldi[1]))    
  FP_summary2 <- rbind(FP_summary1, app) %>% 
    mutate(pcTN = TN/population) 
  gc()
  
  
  # extract region and world information 
  TN_region <- FP_summary2 %>% 
    group_by(region) %>% 
    summarise(region,Year,Leaching = sum(Leaching),Volatilization = sum(Volatilization), 
              TN = sum(TN), TNpc = sum(pcTN)) %>% 
    distinct()
  
  
  # Task: New regions groups: by income, development stage, big countries, world...
  
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------
  # Calculate nitrogen emission flows from country to country: See country exchange
  #--------------------------------------------------------------------------------
  
  
  PFP <-  as.matrix(Li %*% Yi)  
  a <- rep(regions$iso3c, each = 123)   
  b <- rep(items$item, 192)  
  rownames(PFP) <-  a
  # TN flow
  EFP_TN <- ext$TN*PFP
  # use predefined agg() function to aggregate results based on column names
  TNflow <- t(agg(t(EFP_TN)))
  gc()
  TNflow_summary <- TNflow %>% 
    as_tibble() %>%
    mutate(area = regions$area, area_code = regions$area_code, region = regions$region, NFP = colSums(TNflow), TPP = rowSums(TNflow), DP = diag(TNflow), FFP = NFP-DP,  EP = TPP-DP, NI = NFP-TPP) 
  # NFP:total nitrogen footprint of a country in a year
  # TPP: total national nitrogen pollution due to domestic and foreign consumption in a year
  # DP: domestic footprint/domestic pollution due to domestic consumption
  # Net importing countries: Foreign Footprint(FFP) = NFP - DP (cause emissions abroad) > EP = TPP - DP (emissions due to exports) -> NFP>TPP
  NI_top15 <- select(slice(arrange(TNflow_summary,-NI),1:15),area,area_code,NI) # List of net importing countries (cause pollution abroad larger than pollution due to exports)
  NE_top15 <- select(slice(arrange(TNflow_summary,NI),1:15),area,area_code,NI) # List of net exporting countries (domestic pollution due to exports larger than pollution abroad due to imports)
  Nexchange <- rbind(NI_top15,NE_top15)
  gc()
  
  
  # Leaching flow
  EFP_Leaching <- ext$Leaching*PFP
  Leachingflow <- t(agg(t(EFP_Leaching)))
  Leachingflow_summary <- Leachingflow %>% 
    as_tibble() %>%
    mutate(area = regions$area,  area_code = regions$area_code, region = regions$region, NFP = colSums(Leachingflow), TPP = rowSums(Leachingflow), DP = diag(Leachingflow), FFP = NFP-DP,  EP = TPP-DP, NI = NFP-TPP)
  NIL_top15 <- select(slice(arrange(Leachingflow_summary,-NI),1:15),area,area_code,NI)
  NEL_top15 <- select(slice(arrange(Leachingflow_summary,NI),1:15),area,area_code,NI)
  NLexchange <- rbind(NIL_top15,NEL_top15)
  gc()
  
  
  # Volatilization flow
  EFP_Volatilization <- ext$Volatilization*PFP
  Vflow <- t(agg(t(EFP_Volatilization)))
  Vflow_summary <-Vflow %>% 
    as_tibble() %>%
    mutate(area = regions$area, area_code = regions$area_code, region = regions$region, NFP = colSums(Vflow), TPP = rowSums(Vflow), DP = diag(Vflow), FFP = NFP-DP,  EP = TPP-DP, NI = NFP-TPP)
  NIV_top15 <- select(slice(arrange(Vflow_summary,-NI),1:15),area,area_code,NI)
  NEV_top15 <- select(slice(arrange(Vflow_summary,NI),1:15),area,area_code, NI)
  NVexchange <- rbind(NIV_top15,NEV_top15)
  gc()

  # Combing Leaching, Volatilization and TN 
  Nexchange_list <- list(Nexchange, NLexchange, NVexchange)      
  Nexchange_sum <- Nexchange_list%>%
    reduce(full_join, by=c('area','area_code') )%>% 
  rename(TNflow = NI.x, Leaching = NI.y, Volatile = NI) %>% 
  merge(popi, by = 'area_code') %>% 
    mutate(pcNflow = TNflow/population) %>% 
    arrange(-TNflow)
    

  #----------------------------------
  # Create directory and save results  
  #----------------------------------
  
  setwd("~/Nitrogen Extended FABIO/EFP")
  newdir <- paste0(yr)
  dir.create(newdir)
  setwd(paste0("~/Nitrogen Extended FABIO/EFP/",yr))
  # country summary 
  #write.xlsx(FP_summary2,paste0(yr,"_summary.xlsx"))
  # region summary
  write.xlsx(TN_region,paste0(yr,"_region.xlsx"))
  # TN flow
  # write.xlsx(TNflow, paste0(yr,"_TNflow.xlsx"))
  # TN, leaching, volatilization exchange country pairs
  write.xlsx(Nexchange_sum, paste0(yr,"_Nexchange.xlsx"))
  
  
  
  #---------------------------------------------------------------------------------
  # Calculate nitrogen emission flows from countries to a country consumption sector 
  #---------------------------------------------------------------------------------
  
  #select top 5 countries with highest total nitrogen emission and 3 countries with highest per capita emissions
  ctrlist1 <- select(slice(arrange(FP_summary1,-TN),1:5),iso3c)
  ctrlist2 <- select(slice(arrange(FP_summary2, -pcTN),1:3),iso3c)
  ctrlist <- as.data.table(distinct(rbind(ctrlist1,ctrlist2)))
  
  
  #ctrlist <- c('CHN','USA','GBR','BRA','AUS','HUN','AUT','HKG')
  #reglist <- c('OECD Europe','Middle East')
  #class <- c('developing countries','developed countries')
  
  for (j in 1:nrow(ctrlist)){
    
    PFP <- as.matrix(t(t(Li) * as.vector(as.matrix(Yi[,as.character(ctrlist[j])]))))
    c <- rep(items$comm_group, 192)  
    colnames(PFP) <-  c   
    rownames(PFP) <- a
    #EFP_Nwp <- ext$Nwp*PFP
    #EFP_NOx_NH3 <- ext$NOx_NH3*PFP
    #EFP_NO2 <- ext$NO2*PFP
    EFP_TN <- ext$TN*PFP
    
    # aggregate
    EFP_TN <- t(agg(t(EFP_TN)))
    TNcon <- agg(EFP_TN)
    gc()
    write.xlsx(TNcon, paste0(ctrlist[j],'_',yr,'_TNcon.xlsx'))
  }
  
  
  #write.csv(EFP_Nwp, paste0(yr,"_Nwp",".csv"))
  #write.csv(EFP_NOx_NH3, paste0(yr,"_NOx_NH3",".csv"))
  #write.csv(EFP_NO2, paste0(yr,"_NO2",".csv"))
  #write.csv(EFP_TN, paste0(yr,"_TN",".csv"))
  
}


#------------------------------------------------------------------
#  Add consumption footprint (after 2010) to the final footprint  #
#------------------------------------------------------------------

NFP_consumption <- fread("~/Nitrogen Extended FABIO/Results/NFP_consumption.csv")

NFPci <- NFP_consumption[Year == yr, ]









