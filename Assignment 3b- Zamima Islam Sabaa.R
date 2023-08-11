
library(haven)
library(dplyr)
library(tidyverse)

#Loading all the consumer spending datasets and the CPI dataset

set2006<- read_sav("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/shs2006.sav") %>% mutate(year=2006)
set2005<- read_sav("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/shs2005.sav") %>% mutate(year=2005)
set2007<- read_sav("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/SHS2007.sav") %>% mutate(year=2007) 
set2008<- read_sav("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/SHS2008.sav")  %>% mutate(year=2008) 
set2009<- read_sav("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/shs2009.sav") %>% mutate(year=2009)

cpi <- read.csv("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ836 - A. Econometrics/Assignm/Assignment 3b- Zamima Islam Sabaa/data/cpi.csv")

#Binding all the consumer spending datasets into 1 

mainset <- bind_rows(set2005,set2006,set2007,set2008,set2009)


#Cleaning the consumer spending datasets #########################################################

#Renaming the required variables 

mainset <-
  mainset %>% 
  rename(province = 'PROVINCP',
         household_size = 'HHSZTOTP',
         household = 'CASEID',
         rooms = 'NUMRMP',
         type = 'TYPDWELP',
         tenure = 'TENURYRP',
         age_reference = 'RPAGEGRP',
         sex_reference = 'RPSEX',
         Food = 'F001',             #Total expenditure for food.
         Shelter = 'G001',            #Total expenditure for shelter.
         Clothing = 'J001',          #Total expenditure for clothing.
         Transportation = 'K001')     #Total expenditure for transportation.

# Grouping each year each province and each household size (filter for only full year)

mainset <- mainset %>% filter(province !=0, #filters out the masked provinces
                              FYPYFLAG == 1) #filters households that have at least one full-year member

grouped_hh_mean <- mainset %>% 
  group_by(year,province,household_size) %>% 
  summarize(
    Food_mean = mean(Food, na.rm = T),
    Shelter_mean = mean(Shelter, na.rm = T),
    Clothing_mean = mean(Clothing, na.rm = T),
    Transportation_mean = mean(Transportation, na.rm = T))

grouped_hh_sd <- mainset %>% 
  group_by(year,province,household_size) %>% 
  summarize(
    Food_sd = sd(Food, na.rm = T),
    Shelter_sd = sd(Shelter, na.rm = T),
    Clothing_sd = sd(Clothing, na.rm = T),
    Transportation_sd = sd(Transportation, na.rm = T))

grouped_final <- grouped_hh_mean %>% 
  left_join(grouped_hh_sd, by=c('year','province', 'household_size'))


# Transform consumer spending dataset from wide to long for later matching with cpi ######################

mainset_long <-
  mainset %>% select(household, 
                     year, 
                     household_size, 
                     province,
                     rooms, 
                     type, 
                     tenure, 
                     age_reference, 
                     sex_reference,
                     Food, 
                     Shelter, 
                     Transportation, 
                     Clothing) %>% gather(key='good_type',   #new column name with the goods 
                                          value='expenditure', #new column that gives the spending from the goods
                                          c(Food, Shelter, Transportation, Clothing #the columns to gather
                                          ))

# Cleaning the CPI data ######################################################### 

#renaming provinces for matching 

cpi <- cpi %>% 
  rename(province= 'GEO',
         year= 'REF_DATE',
         price='VALUE',
         good_type= 'Products.and.product.groups')

cpi<- cpi %>% mutate(good_type = ifelse(good_type == 'Clothing and footwear', 'Clothing', good_type))

cpi_final <- cpi %>%
  filter(province != 'Canada') %>% 
  mutate(province = case_when(
    province== 'Newfoundland and Labrador' ~ 10,
    province== 'Prince Edward Island' ~ 11,
    province== 'Nova Scotia' ~ 12,
    province== 'New Brunswick' ~ 13,
    province== 'Quebec' ~ 24,
    province== 'Ontario' ~ 35,
    province== 'Manitoba' ~ 46,
    province== 'Saskatchewan' ~ 47,
    province== 'Alberta' ~ 48,
    province== 'British Columbia' ~ 59,
    province== 'Yukon/Northwest Territories/Nunavut' ~ 60
    ))


cpi_final$province <- as.numeric(cpi_final$province)
mainset_long$province <- as.numeric(mainset_long$province)

matched_final <- cpi %>% left_join(mainset_long, by=c('year','province', 'good_type'))

matched_final <- filter(matched_final, good_type %in% c('Food', 'Shelter', 'Clothing', 'Transportation'))
matched_final1 <- matched_final %>% select(year,
                        province,
                        good_type,
                        price,
                        household,
                        household_size,
                        age_reference,
                        sex_reference,
                        expenditure)

matched_final1 <- matched_final1 %>%  
  mutate(province = case_when(
  province== 10 ~ 'Newfoundland and Labrador',
  province== 11 ~ 'Prince Edward Island',
  province== 12 ~ 'Nova Scotia',
  province== 13 ~ 'New Brunswick',
  province== 24 ~ 'Quebec',
  province== 35 ~ 'Ontario',
  province== 46 ~ 'Manitoba',
  province== 47 ~ 'Saskatchewan',
  province== 48 ~ 'Alberta',
  province== 59 ~ 'British Columbia'
  ))


grouped_final <- grouped_final %>% 
  mutate(province = case_when(
    province== 10 ~ 'Newfoundland and Labrador',
    province== 11 ~ 'Prince Edward Island',
    province== 12 ~ 'Nova Scotia',
    province== 13 ~ 'New Brunswick',
    province== 24 ~ 'Quebec',
    province== 35 ~ 'Ontario',
    province== 46 ~ 'Manitoba',
    province== 47 ~ 'Saskatchewan',
    province== 48 ~ 'Alberta',
    province== 59 ~ 'British Columbia'  ))

  

#Exporting the following:
#1. matched dataset that has the demographic data, consumption data (by commodity) and price data (for those commodities) for a set of goods for households in a set of provinces for the year 2005-2009
#2. table giving the means and standard deviations of spending in each consumption category in each year, in each province, and for each household size 

write.csv(matched_final1,file= 'MAtched Dataset of Household Expenditure & Prices.csv')
write.csv(grouped_final, file = 'Mean and Standard Deviation Household Expenditure by year, province, and each household size.csv')

#Notes: All the datasets contain annual expenditure figures




