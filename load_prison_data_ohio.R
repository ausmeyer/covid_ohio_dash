library(tabulizer)
library(purrr)
library(tidyverse)

load('population.Rda')

prison_df <- extract_tables('https://coronavirus.ohio.gov/static/DRCCOVID-19Information.pdf')

#prison_colnames <- trimws(pmap(as_tibble(t(as.data.frame(prison_df[2])[1:11, ])), paste))
prison_colnames <- c('institution', 'staff.positive', 'staff.deaths', 
                     'staff.recovered', 'units.quarantined', 'inmates.quarantined', 
                     'housing.type', 'inmates.isolation', 'inmates.positive', 
                     'inmate.deaths.probable', 'inmate.deaths.confirmed', 
                     'inmate.pending.results', 'inmates.recovered')

df_2 <- as.data.frame(prison_df[[2]])[-c(1:5), ] %>%
  select_if(function(x) !(all(x == ''))) %>%
  as_tibble(., .name_repair = ~ prison_colnames)

df_3 <- as.data.frame(prison_df[[3]][prison_df[[3]][,1] != 'Totals', ]) %>%
  select_if(function(x) !(all(x == ''))) %>%
  as_tibble(., .name_repair = ~prison_colnames)

prison_df <- bind_rows(df_2, df_3)

# prison_county <- list('Allen Oakwood Correctional Institution' = 'Allen',
#                       'Belmont Correctional Institution' = 'Belmont',
#                       'Chillicothe Correctional Institution' = 'Ross',
#                       'Correctional Reception Center' = 'Pickaway',
#                       'Dayton Correctional Institution' = 'Montgomery',
#                       "Franklin Medical Center" = "Franklin",
#                       "Grafton Correctional Institution" = "Lorain",
#                       "Lake Erie Correctional Institution" = "Ashtabula",
#                       "Lebanon Correctional Institution" = "Warren",
#                       "London Correctional Institution" = "Madison",
#                       "Lorain Correctional Institution" = "Lorain",
#                       "Madison Correctional Institution" = "Madison",
#                       "Mansfield Correctional Institution" = "Richland",
#                       "Marion Correctional Institution" = "Marion",
#                       "Noble Correctional Institution" = "Noble",
#                       "North Central Correctional Complex" = "Marion",
#                       "Northeast Ohio Correctional Center" = "Mahoning",
#                       "Northeast Reintegration Center" = "Cuyahoga",
#                       "Ohio Reformatory for Women" = "Union",
#                       "Ohio State Penitentiary" = "Mahoning",
#                       "Pickaway Correctional Institution" = "Pickaway",
#                       "Richland Correctional Institution" = "Richland",
#                       "Ross Correctional Institution" = "Ross",
#                       "Southeastern Correctional Institution" = "Fairfield",
#                       "Southern Ohio Correctional Facility" = "Scioto",
#                       "Toledo Correctional Institution" = "Lucas",
#                       "Trumbull Correctional Institution" = "Trumbull",
#                       "Warren Correctional Institution" = "Warren",
#                       "Totals" = "Totals")

prison_county <- list('AOCI' = 'Allen',
                      'BECI' = 'Belmont',
                      'CCI' = 'Ross',
                      'CRC' = 'Pickaway',
                      'DCI' = 'Montgomery',
                      "FMC" = "Franklin",
                      "GCI" = "Lorain",
                      "LAECI" = "Ashtabula",
                      "LECI" = "Warren",
                      "LOCI" = "Madison",
                      "LORCI" = "Lorain",
                      "MACI" = "Madison",
                      "MANCI" = "Richland",
                      "MCI" = "Marion",
                      "NCI" = "Noble",
                      "NCCC" = "Marion",
                      "NEOCC" = "Mahoning",
                      "NERC" = "Cuyahoga",
                      "ORW" = "Union",
                      "OSP" = "Mahoning",
                      "PCI" = "Pickaway",
                      "RICI" = "Richland",
                      "RCI" = "Ross",
                      "SCI" = "Fairfield",
                      "SOCF" = "Scioto",
                      "TOCI" = "Lucas",
                      "TCI" = "Trumbull",
                      "WCI" = "Warren",
                      "Totals" = "Totals")

prison_df <- bind_cols(prison_df, tibble(county = unlist(prison_county[prison_df$institution], use.names = F)))
names(prison_df) <- make.names(names(prison_df))

prison_summary <- prison_df %>% 
  filter(county != 'Totals') %>%
  group_by(county) %>% 
  summarise(caseCount = sum(as.numeric(inmates.positive)), 
            deathCount = sum(as.numeric(inmate.deaths.confirmed)))

population_summary <- population %>% 
  filter(sex != 'Total', age_range != 'Total') %>% 
  group_by(county) %>% 
  summarise(pop = sum(pop))

normalized_prison_summary <- prison_summary %>% 
  group_by(county) %>%
  mutate(caseCount = round(caseCount * 1000000 / 
                             population_summary$pop[population_summary$county == .data$county]),
         deathCount = round(deathCount * 1000000 / 
                              population_summary$pop[population_summary$county == .data$county]))

save(prison_df, prison_summary, normalized_prison_summary, file = 'prison.rda')