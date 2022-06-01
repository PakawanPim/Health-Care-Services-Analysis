library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
source("C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/STAT515/hw.R")

directory <- 'C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Cleaned/'

MedicarePrice <- read.csv(paste(directory,'MedCarePrice.csv',sep = ''))
US_GDP<-read.csv(paste(directory,'US_GDP.csv',sep = ''))
HMI<-read.csv(paste(directory,'HMI.csv',sep = ''))
HCC_States<-read.csv(paste(directory,'HCC_States.csv',sep = ''))
HCC_plan_type<-read.csv(paste(directory,'HCC_planType.csv',sep = ''))
HCC_AgeGender <- read.csv('C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/AIT664/Project/Datasets/Health Care Cost and Utilization Report/HCC_AgeGender.csv')

## Combine US_GDP with HMI by year
US_GDP_HMI<-merge(US_GDP,HMI, by= 'Year')

## Combine medical price with HMI by state

Medical_Price_HMI<-merge(MedicarePrice,HMI, by=c('state'))

##Combine HCC state and HMI with US_GDP by year and state
HCC_States_HMI_GDP<-merge(HCC_States,US_GDP_HMI, by=c('state','Year'))

##Filter HCC_States
New_HCC_States <- filter(HCC_States, Year %in% c('2017'))

##Combine New_HCC_States with Medical Price
New_HCC_States_Medical_Price<-merge(New_HCC_States,MedicarePrice,by=("state"))

##Drop CBSA in the geo_level
New_HCC_States_Medical_Price2<-filter(New_HCC_States_Medical_Price,geo_level=="State")
sum(is.na(New_HCC_States_Medical_Price2))

## Clean HealthCare cost - Age_Gender ##
HCC_AG1 <- select(HCC_AgeGender,-4:-5,-11,-13:-16,-18,-21:-30 )
HCC_AG1$Age[HCC_AG1$Age == '8-??.?.'] <- '4-8'
HCC_AG1$Age[HCC_AG1$Age == '13-?.?.'] <- '9-13'
HCC_AG2 <- filter(HCC_AG1, Age != 'All Ages' & Gender != 'All Genders' & HCCI_category != 'Total' & Year != '2018')
HCC_AG2[is.na(HCC_AG2)] <- 0

## Data Exploratory ##
### Hypothesis:

#(1) In a year with higher GDP, the HMI's index value will be higher.

# Use Index by year and service Boxplot
HMI_UseIndex <- filter(HMI,index_name == 'Use')
HMI_UseIndex$Year <- as.character(HMI_PriceIndex$Year)

ggplot(HMI_UseIndex, aes(Year, index_value, group = Year))+
  geom_boxplot()+
  facet_wrap(~service_category)+
  labs(y = 'Use Index')+
  ggtitle("HMI Use index by year and service category")
  
# Spending Index by year and service Boxplot
HMI_SpendIndex <- filter(HMI,index_name == 'Spending')
HMI_SpendIndex$Year <- as.character(HMI_PriceIndex$Year)

ggplot(HMI_SpendIndex, aes(Year, index_value, group = Year))+
  geom_boxplot()+
  facet_wrap(~service_category)+
  labs(y = 'Spending Index')+
  ggtitle("HMI Spending index by year and service category")

# GDP vs HMI
GDP_HMI <- filter(US_GDP_HMI,service_category == 'Overall' & index_name != 'Price')%>% 
  group_by(Year, GDP, index_name) %>% summarise(Mean_Index = mean(index_value))

ggplot(GDP_HMI, aes(GDP, Mean_Index))+
  geom_point(aes(color = factor(Year)))+
  facet_grid(~index_name)+
  labs(y = 'Average Index Value')+
  ggtitle("GDP and Index value by index type")+hw

# Correlation between GDP and Mean Use Index from 2014 to 2017
GDP_HMI_price <- filter(GDP_HMI,index_name == 'Use')
cor(GDP_HMI_price$GDP,GDP_HMI_price$Mean_Index)

# Correlation between GDP and Mean Spending Index from 2014 to 2017
GDP_HMI_spend <- filter(GDP_HMI,index_name == 'Spending')
cor(GDP_HMI_spend$GDP,GDP_HMI_spend$Mean_Index)

# HMI by States
library(usmap)
# Price index by US State from 2013 to 2017 
plot_usmap(data = HMI_UseIndex, values = "index_value", color = "white") + 
  labs(title = "Spending per member by US State in 2017") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Spending index by US State from 2013 to 2017 
plot_usmap(data = HMI_SpendIndex, values = "index_value", color = "white") + 
  labs(title = "Spending per member by US State in 2017") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

#(2) The high price of health care services might cause less total spending on health care per person. 
# Spending per member by US State in 2017 (Use original data from HCC_states)
plot_usmap(data = New_HCC_States, values = "spending_per_member", color = "white") + 
  labs(title = "Spending per member by US State in 2017") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Average price per service by US State in 2017 (p.s. no states 'AK' and 'MN' in merged dataset)
plot_usmap(data = New_HCC_States_Medical_Price2, values = "avg_price_pfs", color = "white") + 
  labs(title = "Average price per service by US State in 2017") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Plot relationship between average price per services and spending per member 
# group by different types of Medicare
ggplot(New_HCC_States_Medical_Price2, aes(avg_price_pfs,spending_per_member, color = specialty))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_grid(~specialty)+
  labs(x = 'Average price per service', y = 'Spending per member')+
  ggtitle('Relationship between 
          average price per service vs spending per member')+hw

# Plot number of claims vs everage price pfs
ggplot(New_HCC_States_Medical_Price2, aes(avg_price_pfs,esi_total_claims))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = 'Average Price', y='Count of claims' )+
  ggtitle('Everage price per service vs Number of claims')+
  hw

# number of claims by states (US map)

#(3) The huge cost of spending on health insurance is more likely to be from people whose age is older than 55 years old. 
# Delete all rows in subcategory that start with 'All'
Hcc_age  <-  filter(HCC_AG2, !grepl('All', HCC_AG2$HCCI_subcategory))

# Spending on Health Care Services Group By Gender from 2014-2017
ggplot(Hcc_age, aes(Age,spending_per_member, fill = Gender))+
  geom_bar(stat="identity",
           position= position_dodge())+
  labs(y = 'Spending per Member ($)')+
  scale_x_discrete(limits = c("0-3", "4-8","9-13","14-18","19-25","26-44","45-54","55-64"))+
  ggtitle("Spending on Health Care Services Group By Gender 
          from 2014-2017")+hw

# Spending on Health Care Services Group By Service Categories from 2014-2017
ggplot(Hcc_age, aes(Age,spending_per_member, fill = HCCI_category))+
  geom_bar(stat="identity",
           position= position_dodge())+
  labs(y = 'Spending per Member ($)')+
  scale_x_discrete(limits = c("0-3", "4-8","9-13","14-18","19-25","26-44","45-54","55-64"))+
  ggtitle("Spending on Health Care Services Group 
  By Service Categories from 2014-2017")+
  scale_fill_manual("service categories",values=c('yellowgreen','lightgoldenrod','lightsalmon','skyblue'))+
  hw

ggplot(Hcc_age, aes(Age,spending_per_member, fill = HCCI_category))+
  geom_bar(stat="identity",
           position= position_dodge())+
  labs(y = 'Spending per Member ($)')+
  scale_x_discrete(limits = c("0-3", "4-8","9-13","14-18","19-25","26-44","45-54","55-64"))+
  ggtitle("Spending on Health Care Services Group By Gender 
          from 2014-2017")+hw
ggplot(Hcc_age, aes(Age,spending_per_member))+
  geom_bar(stat="identity",
           position= position_dodge())+
  labs(y = 'Spending per Member ($)')+
  scale_x_discrete(limits = c("0-3", "4-8","9-13","14-18","19-25","26-44","45-54","55-64"))+
  ggtitle("Spending on Health Care Services Group By Gender 
          from 2014-2017")+hw
# Spending on Health Care Services By Year
Hcc_age$Year <- as.character(Hcc_age$Year)
ggplot(Hcc_age, aes(Year,spending_per_member))+
  geom_boxplot()+
  labs(y= 'Spending per Member ($)')+
  ggtitle("Spending on Health Care Services by Year from 2014-2017")+
  scale_fill_manual("service categories",values=c('yellowgreen','lightgoldenrod','lightsalmon','skyblue'))+
  hw

## Analysis ##
# Hypothesis Testing # One-way ANOVA

# (1) GDP vs HMI
# Compute the analysis of variance
gdp_index_aov <- aov(GDP ~ Mean_Index, data = GDP_HMI)
# Summary of the analysis
summary(gdp_index_aov)

# (2) New_HCC_States_Medical_Price2
# Compute the analysis of variance
HCprice_aov <- aov(avg_price_pfs ~ spending_per_member, data = New_HCC_States_Medical_Price2)
# Summary of the analysis
summary(HCprice_aov)

# (3) No need to do hypothesis test, because the graph can easily tell the relationship


## Prepare data for linear regression ##
# Combine Data
Hcc_age2017 <- filter(Hcc_age, Year == '2017')

age_clean <- Hcc_age2017[,c(-5,-7,-9:-12)]

age_clean$HCCI_category[age_clean$HCCI_category == "IP"] <- 'Inpatient'
age_clean$HCCI_category[age_clean$HCCI_category == "OP"] <- 'Outpatient'
age_clean$HCCI_category[age_clean$HCCI_category == "PH"] <- 'Professional'
age_clean$HCCI_category[age_clean$HCCI_category == "RX"] <- 'Prescription Drugs'

# HMI
HMI_1 <- HMI[,-11] %>% filter(index_name != 'Price' & service_category != 'Overall' & Year == '2017')
HMI_modify <- spread(HMI_1,'index_name','index_value')
HMI_mod1 <- HMI_modify %>% group_by(service_category) %>% mutate(mean_Use_Index = mean(Use)) 
HMI_mod2 <- HMI_mod1 %>% group_by(service_category) %>% mutate(mean_Spending_Index = mean(Spending))
HMI_mod3 <- select(HMI_mod2,service_category,mean_Use_Index,mean_Spending_Index)
names(HMI_mod3)[1] <- 'HCCI_category'

# Left join age and indexes by HCCI_category
age_hmi <- merge(age_clean,HMI_mod3,by = 'HCCI_category')
age_hmi[is.na(age_hmi)] <- 0

# Delete duplicate rows
unique(age_hmi)
Data <- age_hmi %>% distinct(HCCI_category, HCCI_subcategory, Age,Gender, .keep_all = TRUE)

# reorder columns
Data <- Data[,c(2,3,1,4,6,7,8,5)]
names(Data)[3] <-'Cat' 
names(Data)[4] <-'SubCat' 

# create dummy variables: One-Hot Encoding
library(caret)
#
Dat <- dummyVars(" ~ .", data = Data)
Data_transformed <- data.frame(predict(Dat, newdata = Data))

glimpse(Data_transformed)

Correlation <- cor(Data_transformed)
corrplot(Correlation,method = 'color',order="hclust",tl.cex = .5)

#*** Below is another way to create dummy variabls***
Data$Male <- ifelse(Data$Gender == 'Male',1,0)

## Modeling ##
# Linear Regression #

# Try pre-process: center ans scaling

# Perform Linear Regression Model
linear_model <- lm(spending_per_member~., data = Data_transformed)
summary(linear_model)
plot(linear_model)

# filter out high correlated variables
tooHigh <- findCorrelation(Correlation, .8)
corrPred <- names(Data_transformed)[tooHigh]
Datafiltered <- Data_transformed[, -tooHigh]
Datafiltered <- Datafiltered[,c(-8,-34)]

# Perform Linear Regression Model
linear_model2 <- lm(spending_per_member~., data = Datafiltered)
summary(linear_model2)
plot(linear_model2)






