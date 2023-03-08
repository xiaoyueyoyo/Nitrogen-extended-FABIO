################################
# Footprint analysis and Plot  #
################################

# 0. Initial Setting 
agg <- function(x) {  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x)}

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
#install.packages("plotly")
library(plotly)
library(ggrepel)
library(dplyr)
library(viridis)
library(data.table)
library(readxl)
library(xlsx)

rm(list=ls()); gc()

regions <- fread("~/nitrogen_fabio/Data/regions.csv")
items <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")
years <- c(1986:2020)


# 1. Read main data-------------------------------------------------------------
FPcontinent <- fread("~/nitrogen_fabio/Results/FPcontinent_summary_IPCC.csv") # IPCC model 
# FPcontinent <- fread("~/nitrogen_fabio/Results/FPcontinent_summary.csv") # IPCC,CNB average 
FPcountry <- fread("~/nitrogen_fabio/Results/FPcountry_summary_IPCC.csv")
Nexchange <- fread("~/nitrogen_fabio/Results/Nexchange_summary.csv")
food_emission <- readRDS("~/nitrogen_fabio/Results/food_emission_summary.rds")

# 2. Read other data------------------------------------------------------------
# 2.1 Nitrogen utilization efficiency 
NUE <- fread("~/nitrogen_fabio/Data/NUE.csv")
NUE <- NUE %>% merge(regions,by="area_code") %>% 
  group_by(Year,continent) %>% 
  mutate(avNUE = mean(NUE),na.rm = TRUE) %>% 
  select(continent, Year, avNUE) %>% 
  distinct()
# 2.2 World Economic data including commodity prices, oil price, gdppc, income share 
world_eco <- fread("~/nitrogen_fabio/Data/wrdecon.csv")
# 2.3 Regional economic data 
country_eco <- fread("~/nitrogen_fabio/Data/areaecon.csv")



# 3. Dynamics of continent and world Nitrogen footprint ------------------------

# 3.1 Data cleaning ------------------------------------------------------------ 

FPcontinent <- FPcontinent%>% 
  filter(Year != "1990" & Year!="1992") %>%  # filter out 1990 1992 abnormal values
  select(-V1,-X) %>%  # deselect unnecessary variables 
  merge(NUE, by = c("Year","continent")) 
FPworld <- FPcontinent %>% 
  group_by(Year) %>% # Generate summary rows for world data 
  summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'World')) 
FPall <- bind_rows(FPworld,FPcontinent)  # Bind summary rows 
FPall$avNUE[FPall$continent=="World"] <- FPall$avNUE/11 # change summed avNUE to avNUE; table(FPworld$Year) = 11
FPall$TN <- round(FPall$TN/10e3,2)  # Unit transformation
# Unit: 10e3(k) ton, 10e6(M) kg, 10e9(G) g. 
# Other units remain ton, population pc 

# 3.2 Generate graph of world dynamics -----------------------------------------
world <- merge(filter(FPworld,continent == "World"), world_eco, by="Year")
plot <- ggplot(data=world, aes(x=TN,y=avNUE))+
  geom_point(color = "blue",shape = 1)+
  geom_line(color="blue",     # Line color
            size=2,          # Line size
            alpha=0.1,
            linetype = 1)
plot1 <- plot+
  geom_text(data=  filter(world,Year %in% c(1986,2000,2010,2020)),
            aes(label = Year),
            nudge_y = 1)+  # nudge size should depend on the value 
  geom_point(data=  filter(world,Year %in% c(1986,2000,2010,2020)), 
             shape = 2,size = 0.5)
plot1

 
ggplot(data = FPworld, aes(x=Year, y=World)) + 
  geom_line()  

ggplot(data = FPworld, aes(x=Year, y=TN)) + 
  geom_area(aes(fill = continent))  


# 4. Dynamics of nitrogen footprint of important countries ---------------------

# 4.1 Data Cleaning 
country <- merge(FPcountry, country_eco[,c("area_code","Year","p10","gdppc")],
                 by = c("area_code","Year")) # Merge with economic data 


# 4.2 Plot




# 5. Nitrogen exchange ---------------------------------------------------------

# 5.1 Data Cleaning 



# 5.2 Plot 


# 6. Food Emissions ------------------------------------------------------------

# 6.1 Data Cleaning


# 6.2 Plot 





