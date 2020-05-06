suppressWarnings(load('data.rda'))
suppressWarnings(load('prison.rda'))

options(spinner.color="#6699cc")

all.choices <- c(unique(ohio.df$county))
all.choices <- all.choices[all.choices != 'Total']
all.choices <- c('Total', all.choices)

all.ages <- c('Total', '0-19', '20-29', '30-39', 
              '40-49', '50-59', '60-69', 
              '70-79', '80+', 'Unknown')
all.sexes <- c('Total', 'Female', 'Male', 'Unknown')
all.series <- list('Daily Cases' = 'caseCount', 
                   'Daily Hospitalizations' = 'hospitalizedCount',
                   'Daily Deaths' = 'deathCount',
                   'Aggregate Cases' = 'aggregateCaseCount',
                   'Aggregate Hospitalizations' = 'aggregateHospitalizedCount',
                   'Aggregate Deaths' = 'aggregateDeathCount')

map.series <- list('Daily Cases' = 'caseCount', 
                   'Daily Hospitalizations' = 'hospitalizedCount',
                   'Daily Deaths' = 'deathCount')

all.transformations <- list('Linear' = 'none', 
                            'Log10' = 'log10')

colors <- iwanthue(length(unique(ohio.df$county)), random = F)
colors.list <- list()
sapply(1:length(unique(ohio.df$county)), function(x) colors.list[unique(ohio.df$county)[x]] <<- colors[x])