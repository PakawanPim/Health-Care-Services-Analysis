library(dplyr)
library(tidyverse)

directory <- 'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/'
MedicarePrice <- read.csv(paste(directory,'Commercial and Medicare Prices for Professional Services/av_commed_basket_price_ratio_puf.csv',sep = ''))
HCC_AgeGender <- read.csv(paste(directory,'Health Care Cost and Utilization Report/HCC_AgeGender.csv',sep = ''))
HCC_CDHP <- read.csv(paste(directory,'Health Care Cost and Utilization Report/HCCUR2018_CDHP.csv',sep = ''))
HCC_States <- read.csv(paste(directory,'Health Care Cost and Utilization Report/HCCUR2018_HL_State.csv',sep = ''))
HMI <- read.csv(paste(directory,'Healthy Marketplace Index (HMI)/hmi_2020_combined_puf.csv',sep = ''))
GDP <- read.csv(paste(directory,'World_GDP/API_GDP.csv',sep = ''))

## For GDP ##
# filter the U.S. GDP from GDP file
US_GDP_1 <- filter(GDP, Country.Code == 'USA')
# Drop columns: we want year 2014-2017
US_GDP_2 <- select(US_GDP_1,-2,-3:-58,-63:-65)
names(US_GDP_2)[1] <- 'Country'
names(US_GDP_2)[2] <- '2014'
names(US_GDP_2)[3] <- '2015'
names(US_GDP_2)[4] <- '2016'
names(US_GDP_2)[5] <- '2017'
# GDP
US_GDP <- US_GDP_2 %>% gather('Year', 'GDP', 2:5)
write.csv(US_GDP,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/US_GDP.csv',row.names = FALSE)

## For HMI ##
HMI_1 <- select(HMI, -2, -13:-15)
HMI_2 <- filter(HMI_1, year %in% c('2014','2015','2016','2017') & geo_level == 'State')
HMI_3 <- filter(HMI_2, metric_type == 'Index' & moment == 'Mean' & index_name %in% c('Price','Use','Spending') )
names(HMI_3)[1] <- 'Year'
sum(is.na(HMI_3))
write.csv(HMI_3,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/HMI.csv',row.names = FALSE)

## For HealthCare cost - Age_Gender ##
HCC_AG1 <- select(HCC_AgeGender,-4:-5,-11,-13:-16,-18,-21:-30 )
HCC_AG1$Age[HCC_AG1$Age == '8-??.?.'] <- '4-8'
HCC_AG1$Age[HCC_AG1$Age == '13-?.?.'] <- '9-13'
HCC_AG2 <- filter(HCC_AG1, Age != 'All Ages' & Gender != 'All Genders' & HCCI_category != 'Total' & Year != '2018')
HCC_AG2[is.na(HCC_AG2)] <- 0
write.csv(HCC_AG2,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/HCC_AG.csv',row.names = FALSE)

## For HealthCare cost - CDHP (Consumer driven health plan) ##
HCC_planType1 <- select(HCC_CDHP, -3,-5,-10,-12:-15,-17,-20:-29)
HCC_planType2 <- filter(HCC_planType1, HCCI_category != 'Total' & Year != '2018')
HCC_planType2[is.na(HCC_planType2)] <- 0
write.csv(HCC_planType2,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/HCC_planType.csv',row.names = FALSE)

## For HealthCare cost - States ##
HCC_S1 <- select(HCC_States, -8:-10,-12,-15:-24)
HCC_S2 <- filter(HCC_S1, HCCI_category != 'Total' & Year != '2018')
names(HCC_S2)[1] <- 'state'
HCC_S2[is.na(HCC_S2)] <- 0
write.csv(HCC_S2,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/HCC_States.csv',row.names = FALSE)

## For Medicare Price ##
MedCare1 <- select(MedicarePrice, -9:-10)
MedCare2 <- filter(MedCare1, specialty != 'All')
sum(is.na(MedCare2))
write.csv(MedCare2,'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/MedCarePrice.csv',row.names = FALSE)

