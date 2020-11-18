library(tidyverse)
library(lubridate)

calc.totals <- function(this.df) {
  aggregate.df <- this.df %>% 
    group_by(date, age_range, sex) %>% 
    summarise(county = 'Total',  
              caseCount = sum(caseCount), 
              deathCount = sum(deathCount), 
              hospitalizedCount = sum(hospitalizedCount)) %>%
    ungroup() 
  
  this.df <- bind_rows(this.df, aggregate.df)
  
  aggregate.df.1 <- this.df %>% 
    group_by(county, date) %>% 
    summarise(sex = 'Total',
              age_range = 'Total', 
              caseCount = sum(caseCount), 
              deathCount = sum(deathCount), 
              hospitalizedCount = sum(hospitalizedCount)) %>%
    ungroup()
  
  aggregate.df.2 <- this.df %>% 
    group_by(county, date, sex) %>% 
    summarise(age_range = 'Total', 
              caseCount = sum(caseCount), 
              deathCount = sum(deathCount), 
              hospitalizedCount = sum(hospitalizedCount)) %>%
    ungroup()
  
  aggregate.df.3 <- this.df %>% 
    group_by(county, date, age_range) %>% 
    summarise(sex = 'Total', 
              caseCount = sum(caseCount), 
              deathCount = sum(deathCount), 
              hospitalizedCount = sum(hospitalizedCount)) %>%
    ungroup()
  
  this.df <- bind_rows(this.df, aggregate.df.1, aggregate.df.2, aggregate.df.3)
  
  this.df <- this.df %>% group_by(county, sex, age_range) %>% 
    mutate(aggregateCaseCount = cumsum(caseCount), 
           aggregateDeathCount = cumsum(deathCount), 
           aggregateHospitalizedCount = cumsum(hospitalizedCount)) %>%
    ungroup()
  
  return(this.df)
}

load('population.rda')

ohio.df <- read_csv('https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv')

names(ohio.df) <- c('county', 'sex', 'age_range',
                    'onset_date', 'death_date', 'admission_date',
                    'caseCount', 'deathCount', 'hospitalizedCount')

ohio.df <- ohio.df %>% filter(!(county %in% c('Grand Total', 'Out')))
ohio.df$onset_date <- mdy(ohio.df$onset_date)

ohio.df <- ohio.df %>% 
  group_by(county, sex, age_range) %>%
  complete(onset_date = seq.Date(min(ohio.df$onset_date), max(ohio.df$onset_date), by = 'day')) %>%
  ungroup() %>%
  select(-c(death_date, admission_date)) %>%
  replace(is.na(.), 0) %>%
  rename(date = onset_date) %>%
  ungroup()

ohio.df <- calc.totals(ohio.df)

ohio.df <- ohio.df[ohio.df$sex %in% unique(population$sex) & 
                     ohio.df$age_range %in% unique(population$age_range) &
                     ohio.df$county %in% population$county, ]

normalized.df <- ohio.df %>% 
  group_by(county, sex, age_range, date) %>%
  mutate(population = population$pop[population$sex == .data$sex & 
                                       population$age_range == .data$age_range & 
                                       population$county == .data$county]) %>%
  mutate(caseCount = round(caseCount * 1000000 / population),
         deathCount = round(deathCount * 1000000 / population),
         hospitalizedCount = round(hospitalizedCount * 1000000 / population),
         aggregateCaseCount = round(aggregateCaseCount * 1000000 / population),
         aggregateDeathCount = round(aggregateDeathCount * 1000000 / population),
         aggregateHospitalizedCount = round(aggregateHospitalizedCount * 1000000 / population)) %>%
  ungroup()

save(ohio.df, normalized.df, file = 'data.rda')