rm(list = ls())

calc.totals <- function(this.df) {
  aggregate.df <- this.df %>% 
    group_by(age_range, sex) %>% 
    summarise(county = 'Total',  
              pop = sum(pop))
  
  this.df <- bind_rows(this.df, aggregate.df)
  
  aggregate.df.1 <- this.df %>% 
    group_by(county) %>% 
    summarise(sex = 'Total',
              age_range = 'Total', 
              pop = sum(pop))
  
  aggregate.df.2 <- this.df %>% 
    group_by(county, sex) %>% 
    summarise(age_range = 'Total', 
              pop = sum(pop))
  
  aggregate.df.3 <- this.df %>% 
    group_by(county, age_range) %>% 
    summarise(sex = 'Total', 
              pop = sum(pop))
  
  this.df <- bind_rows(this.df, aggregate.df.1, aggregate.df.2, aggregate.df.3)
  
  return(this.df)
}

est <- get_estimates(geography = 'county', 
                     state = 'OH', 
                     breakdown = c('SEX', 'AGEGROUP'), 
                     breakdown_labels = T, 
                     product = 'characteristics')

est$NAME <- str_remove_all(est$NAME, ' County, Ohio')
est <- est[est$SEX != 'Both sexes', ]
est <- est[est$AGEGROUP %in% as.character(unique(est$AGEGROUP))[2:19], ]
est$AGEGROUP <- str_remove_all(est$AGEGROUP, 'Age ')
est$AGEGROUP <- str_remove_all(est$AGEGROUP, ' years and older')
est$AGEGROUP <- str_remove_all(est$AGEGROUP, ' years')

counties <- c()
sexes <- c()
nums <- c()
ages <- c()

split.ages <- str_split_fixed(est$AGEGROUP, ' to ', n = 2)
low.ages <- c('0 to 4', '5 to 9', '10 to 14', '15 to 19')

for(i in 1:nrow(est)) {
  if(est$AGEGROUP[i] == low.ages[4] & est$SEX[i] == 'Female') {
    # if(est$NAME[i] == 'Stark') {
    #   print(counties[length(counties)])
    #   print(ages[length(ages)])
    # }
    counties <<- c(counties, est$NAME[i], est$NAME[i])
    sexes <<- c(sexes, est$SEX[i - 1], est$SEX[i - 2])
    nums <<- c(nums, 
               est$value[i - 7] + est$value[i - 5] + est$value[i - 3] + est$value[i - 1], 
               est$value[i - 6] + est$value[i - 4] + est$value[i - 2] + est$value[i])
    ages <<- c(ages, 
               paste(split.ages[i - 7, 1], '-', split.ages[i - 1, 2], sep = ''), 
               paste(split.ages[i - 6, 1], '-', split.ages[i, 2], sep = ''))
    # if(est$NAME[i] == 'Stark') {
    #   print(counties[length(counties)])
    #   print(ages[length(ages)])
    # }
  }

  else if((i %% 4) == 0 & !(est$AGEGROUP[i] %in% low.ages)) {
    counties <<- c(counties, est$NAME[i], est$NAME[i])
    sexes <<- c(sexes, est$SEX[i - 3], est$SEX[i - 2])
    nums <<- c(nums, est$value[i - 3] + est$value[i - 1], est$value[i - 2] + est$value[i])
    ages <<- c(ages, paste(split.ages[i - 3, 1], '-', split.ages[i - 1, 2], sep = ''), paste(split.ages[i - 2, 1], '-', split.ages[i, 2], sep = ''))
  }
}

population <- data.frame(county = counties, sex = sexes, age_range = ages, pop = nums, stringsAsFactors = F)
population$age_range[population$age_range == '80-'] <- '80+'
population <- population %>% mutate_if(is.factor, as.character)
population <- calc.totals(population)

save(population, file = 'population.Rda')