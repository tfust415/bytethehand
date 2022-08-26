library(readxl)
library(tigris)
library(foreign)
library(tidyverse)
library(readr)
library(dplyr)
library(sf)

#Shared Tables to load - saving table in R format - will have to update with yearly additions
#base files to load

#Supplementary homicide report
spss_supp_homi <- read.spss('C:/Users/Taylor/Documents/Murder Accountability/SHR76_20.sav', to.data.frame = TRUE) #reads SPSS file because there are columns not in the non-spss files - UPDATED 8/4/2022

supp_homi <- read_csv('C:/Users/Taylor/Documents/Murder Accountability/SHR76_20.csv') #reads in the "regular" supplementary homicide report - UPDATED 8/4/2022

#This is for the map
spss_supp_homi$CNTYFIPS <- gsub(" ", "", spss_supp_homi$CNTYFIPS, fixed = TRUE)   #subbing out space for fips codes for mapping

supp_homi_fips <- supp_homi %>%
  left_join(select(spss_supp_homi, ID, 'county_fips'=CNTYFIPS), by='ID') #joining the FIPS code from the spss file - base table

saveRDS(supp_homi_fips, 'supp_homi_fips_20.rds')

#from tigris counties - as SF

all_counties <- counties(state = NULL, class="sf" , cb=TRUE, resolution="500k", year=NULL) #class can be SP or SF

saveRDS(all_counties, "all_counties.rds")

#state fips list for filtering county maps - taken from census - joins for county map
state_fips <- read_tsv('C:/Users/Taylor/Documents/Murder Accountability/cen_fips.txt')

saveRDS(state_fips, "state_fips.rds")



#supp_homi_fips_19 <- read_rds("supp_homi_fips.rds")
#supp_homi_fips_20 <- read_rds("supp_homi_fips_20.rds")%>%
#  select(ID, CNTYFIPS, State, Solved, Year, VicAge, VicSex, VicRace,OffAge, OffSex,OffRace,Weapon,county_fips)
