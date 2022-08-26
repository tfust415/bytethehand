#Data checks - 2020

supp_homi_fips <- read_rds('supp_homi_fips_20.rds')%>%
  select(ID, CNTYFIPS, State, Solved, Year, VicAge, VicSex, VicRace,OffAge, OffSex,OffRace,Weapon,county_fips)%>%
  mutate(VicAgeRange=case_when(VicAge == 0 ~ "<1",
                               VicAge>=0 & VicAge <10 ~ "1-9",
                               VicAge>=10 & VicAge <20 ~ "10-19",
                               VicAge>=20 & VicAge <30 ~ "20-29",
                               VicAge>=30 & VicAge <40 ~ "30-39",
                               VicAge>=40 & VicAge <50 ~ "40-49",
                               VicAge>=50 & VicAge <60 ~ "50-59",
                               VicAge>=60 & VicAge <70 ~ "60-69",
                               VicAge>=70 & VicAge <555 ~ "70+",
                               VicAge==999 ~ "Unknown"))%>%
  mutate(OffAgeRange=case_when(OffAge == 0 ~ "<1",
                               OffAge>=0 & OffAge <10 ~ "1-9",
                               OffAge>=10 & OffAge <20 ~ "10-19",
                               OffAge>=20 & OffAge <30 ~ "20-29",
                               OffAge>=30 & OffAge <40 ~ "30-39",
                               OffAge>=40 & OffAge <50 ~ "40-49",
                               OffAge>=50 & OffAge <60 ~ "50-59",
                               OffAge>=60 & OffAge <70 ~ "60-69",
                               OffAge>=70 & OffAge <555 ~ "70+",
                               OffAge==999 ~ "Unknown"))


#Need to systematically check the filters and sums



#No filter check

#All Cases - 2020, 2015 looks good

check1 <- supp_homi_fips %>%
  filter(Year==2015)%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check1)

#Solved cases - 2016 looks good
check2 <- supp_homi_fips %>%
  filter(Year==2016)%>%
  filter(Solved=='Yes')%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check2)

#unsolved cases - 2010 looks good
check3 <- supp_homi_fips %>%
  filter(Year==2010)%>%
  filter(Solved=='No')%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check3)

#all cases - 2012 - weapon filter
check4 <- supp_homi_fips%>%
  filter(Year==2012)%>%
  filter(Weapon=='Handgun - pistol, revolver, etc')%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check4)

#solved - 2011-2014 - weapon filter multiple - good
check5 <- supp_homi_fips%>%
  filter(Year %in% c(2012,2013,2014))%>%
  filter(Solved=='Yes')%>%
  filter(Weapon %in% c('Handgun - pistol, revolver, etc', "Rifle"))%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check5)

#unsolved 1992-6 - weapon filter - good
check6 <- supp_homi_fips%>%
  filter(Year %in% c(1993,1994,1995,1996))%>%
  filter(Solved=='No')%>%
  filter(Weapon %in% c('Knife or cutting instrument', "Rifle"))%>%
  mutate(count=1)%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check6)

#all - weapon - state 1980 - good
check7 <- supp_homi_fips%>%
  filter(Year %in% c(1980))%>%
  filter(Weapon %in% c('Handgun - pistol, revolver, etc', "Rifle"))%>%
  mutate(count=1)%>%
  filter(State=='Illinois')%>%
  group_by(CNTYFIPS)%>%
  mutate(county_count=sum(count))%>%
  select(CNTYFIPS,county_fips, Year, county_count)%>%
  unique()

rm(check7)

#solved - weapon - state - victim

check8 <- 