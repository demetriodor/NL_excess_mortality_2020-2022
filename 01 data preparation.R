# Getting and preparing NL data on COVID-19 and mortality and population
# Libraries and functions -------------------------------------------------
library(tidyverse)
library(cbsodataR)
library(xts)
library(utils)
library(httr)
library(zoo)
# Get weekly mortality data from CBS -------------------------------------------------
# Get the mortality dataset
all.deaths <- cbs_get_data('70895ned') %>% # specify the dataset id
  cbs_add_label_columns() %>%
  mutate (year = as.numeric(as.character(substr(Perioden, 1, 4))), # extract the year
          week = as.numeric(as.character(substr(Perioden, 7,8))), # extract the week
          year.month = format(strptime(paste(year, week, 1), format = "%Y %W %u"),"%Y.%m"), # make a year.month index
          filter_n = as.character(substr(Perioden, 5,6)), # will be used for filtering out
          deaths = Overledenen_1, # rename column
          n_days = trimws(gsub("\\(", "", substr(Perioden_label, 14, 15))), # this gets the number of days in the week
          n_days = as.numeric(as.character(ifelse (nchar(n_days)==0, 7, n_days))), # usually 7, but not always for weeks 0, 1, 52 and 53; rescale
          week = case_when(week == 0 ~ 1, week == 53 ~ 52, TRUE ~ week) # recode weeks 0 and 53 to 1 and 52
  ) %>% 
  filter(filter_n != 'JJ')  # remove yearly totals

# Make function to filter and aggregate the data
data.prep <- function (sex, age_group){
  deaths.temp <- all.deaths %>% 
    filter (Geslacht == sex, LeeftijdOp31December == age_group) %>% 
    dplyr::select (year, week, year.month, deaths, n_days) %>% 
    mutate (index = paste0(year, '.', week)) %>% 
    group_by (index) %>% # this is needed for weeks 1 and 52
    summarize(year=mean(year), week=mean(week), deaths = sum(deaths), year.month = first (year.month), n_days = sum(n_days)-7) %>%  
    mutate (deaths.s = deaths * 7/(n_days+7)) %>% 
    dplyr::filter (year < 2023) %>% 
    #dplyr::select (-index)  %>% 
    arrange(year, week)
} 

# Apply the function to get all deaths
deaths <- data.prep(sex = 1100, age_group = 10000) # all deaths

# Now get deaths for specific sex and age groups
deaths.m <- data.prep(sex = 3000, age_group = 10000) # men only   
deaths.w <- data.prep(sex = 4000, age_group = 10000) # women only   
deaths.80plus <- data.prep(sex = 1100, age_group = 21700) # all, 80+ deaths
deaths.65to80 <- data.prep(sex = 1100, age_group = 53950) # all, 65 to 80 deaths
deaths.0to65 <- data.prep(sex = 1100, age_group = 41700) # all, 0 to 65 deaths
deaths.m.80plus <- data.prep(sex = 3000, age_group = 21700) # men, 80+ deaths
deaths.m.65to80 <- data.prep(sex = 3000, age_group = 53950) # men, 65 to 80 deaths
deaths.m.0to65 <- data.prep(sex = 3000, age_group = 41700) # men, 0 to 65 deaths
deaths.w.80plus <- data.prep(sex = 4000, age_group = 21700) # women, 80+ deaths
deaths.w.65to80 <- data.prep(sex = 4000, age_group = 53950) # women, 65 to 80 deaths
deaths.w.0to65 <- data.prep(sex = 4000, age_group = 41700) # women, 0 to 65 deaths

# Get annual population data from CBS -------------------------------------------------
# alternative dataset for total population data
all.pop2 <- cbs_get_data('37296eng') %>%
  cbs_add_label_columns() %>%
  mutate (pop = TotalPopulation_1 / 1e6, # divide by a million
          pop.m = Males_2 / 1e6, # men
          pop.w = Females_3 / 1e6, # women
          pop.0to20 = YoungerThan20Years_10,
          pop.20to40 = k_20To40Years_11,
          pop.40to65 = k_40To65Years_12,
          pop.65to80 = k_65To80Years_13,
          pop.80plus = k_80YearsOrOlder_14,
          year = as.numeric(as.character(Periods_label))) %>%
  filter (year > 1994, year < 2023) 

# Get total population
pop <- all.pop2[,c("pop","year")] # all population

# Population per sex
pop.m<-all.pop2[,c("pop.m","year")] # men
pop.w<-all.pop2[,c("pop.w","year")] # women

# Population per age group
pop.0to20 <- all.pop2[,c("pop.0to20","year")] 
pop.20to40 <- all.pop2[,c("pop.20to40","year")] 
pop.40to65 <- all.pop2[,c("pop.40to65","year")] 
pop.65to80 <- all.pop2[,c("pop.65to80","year")] 
pop.80plus <- all.pop2[,c("pop.80plus","year")] 

# We gotta do some work to match the age categories from the mortality dataset
pop.0to65 <- left_join(pop.0to20, pop.20to40, by = 'year') 
pop.0to65 <- left_join(pop.0to65, pop.40to65, by = 'year') %>% 
  mutate (pop.0to65 = pop.0to20 + pop.20to40 + pop.40to65) %>% 
  dplyr::select (year, pop.0to65) 

# Get weather data --------------------------------------------------------
library(devtools)
devtools::install_github("bvhest/KNMIr")
library(KNMIr)

# get data for the measurement station Gilze-Rijen (id=350) from Januari 1st, 2016 up to the most recent date provided by the KNMI (which is 'today'-1 day).
nlw <-get_daily_data(stationID = 260, #De Bilt 
                 from = "20110101",
                 to = "20221231") %>% 
  # only keep the following variables:
  subset_data(variables = c("TN", "TX")) %>%
  # change the codes for these variables into more readable names:
  rename_KNMI_column_names() %>%
  select(datum, minTemp, maxTemp)
head(nlw)

# aggregate at week level
nlw.w <- nlw %>%
  read.zoo()  %>%
  apply.weekly(mean, na.rm=T) %>%
  transform (year = as.numeric(as.character(strftime(.,format = "%Y"))),
             week = as.numeric(as.character(strftime(.,format = "%V"))),
             date = as.character(attributes(.)$index))

# manually clean up some week number issues
nlw.w <- nlw.w[nlw.w$week!=53,]
nlw.w[nlw.w$date=='2011-01-02',]$week<-0
nlw.w <- nlw.w[nlw.w$week!=0,]
nlw.w[nlw.w$date=='2012-01-01',]$year<-2011
nlw.w[nlw.w$date=='2017-01-01',]$year<-2016
nlw.w[nlw.w$date=='2022-01-02',]$year<-2021
nlw.w$id <- paste0(nlw.w$year, '.', trimws(nlw.w$week))
table(nlw.w$week)

# transform variables 
nlw.w  = data.frame(nlw.w) %>% 
  mutate (temp.max = round(as.numeric(as.character(maxTemp)), 2),
          temp.min = round(as.numeric(as.character(minTemp)), 2),
          t.min2 = case_when (temp.min>3 ~ 0, TRUE ~ 3-temp.min),
          t.min = case_when (temp.min>0 ~ 0, TRUE ~ 0-temp.min),
          t.max = case_when (temp.max<20 ~ 0, TRUE ~temp.max-20),
          index = id
  ) %>% 
  dplyr::select (index, temp.max, temp.min, t.min2, t.min, t.max) 

# Merge, save and subset the data  -------------------------------------------------
# General dataset
d <- left_join (deaths, pop, by = 'year') %>%  # add total population
  left_join(., pop.80plus, by='year') %>%  # add population above 80
  left_join(., pop.65to80, by='year') %>%  # add population 65 to 80
  left_join(., pop.0to65, by='year') %>%  # add population 65 to 80
  left_join(., nlw.w, by='index') %>% 
  mutate (deaths.pc = deaths / pop/1e06, # calculate deaths per capita
          share.80plus = pop.80plus/pop/1e06*100, # calculate share of 80+
          share.65to80 = pop.65to80/pop/1e06*100, # calculate share of 65 to 80
          share.65plus = share.80plus + share.65to80, # calculate share of 65+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d, './data/nl_mortality_pop_weather_1995-2022.csv')
save(d, file = './data/nl_mortality_pop_weather_1995-2022.RData')

# Gender-specific data
d.m <- left_join (deaths.m, pop.m, by = 'year') %>%  # add total population
  left_join(., nlw.w, by='index') %>% 
  left_join(., d[,c('index', 'share.80plus')], by='index') %>%  # add population above 80
  mutate (deaths.pc = deaths / pop.m / 1e06, # calculate deaths per capita
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.m, './data/nl_men_mortality_pop_1995-2022.csv')
save(d.m, file = './data/nl_men_mortality_pop_1995-2022.RData')

d.w <- left_join (deaths.w, pop.w, by = 'year') %>%  # add total population
  left_join(., nlw.w, by='index') %>% 
  left_join(., d[,c('index', 'share.80plus')], by='index') %>%  # add population above 80
  mutate (deaths.pc = deaths / pop.w / 1e06, # calculate deaths per capita
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.w, './data/nl_women_mortality_pop_1995-2022.csv')
save(d.w, file = './data/nl_women_mortality_pop_1995-2022.RData')

# Age-specific data
d.80plus <- left_join (deaths.80plus, pop.80plus, by = 'year') %>%  # add total population
  left_join(., nlw.w, by='index') %>% 
  mutate (deaths.pc = deaths / pop.80plus, # calculate deaths per capita
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.80plus, './data/nl_80plus_mortality_pop_1995-2022.csv')
save(d.80plus, file = './data/nl_80plus_mortality_pop_1995-2022.RData')

d.65to80 <- left_join (deaths.65to80, pop.65to80, by = 'year') %>%  # add total population
  left_join(., nlw.w, by='index') %>% 
  mutate (deaths.pc = deaths / pop.65to80, # calculate deaths per capita
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.65to80, './data/nl_65-80_mortality_pop_1995-2022.csv')
save(d.65to80, file = './data/nl_65-80_mortality_pop_1995-2022.RData')

d.0to65 <- left_join (deaths.0to65, pop.0to65, by = 'year') %>%  # add total population
  left_join(., nlw.w, by='index') %>% 
  mutate (deaths.pc = deaths / pop.0to65, # calculate deaths per capita
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.0to65, './data/nl_0-65_mortality_pop_1995-2022.csv')
save(d.0to65, file = './data/nl_0-65_mortality_pop_1995-2022.RData')

### THE END