calculateValueBoxData <- function(these.data, local.series, plotly.settings) {
  
  if(as.logical(these.data$normalize))
    local.df <- normalized.df[normalized.df$county %in% these.data$counties, ]
  else
    local.df <- ohio.df[ohio.df$county %in% these.data$counties, ]
  
  if('Total' %in% these.data$counties)
    these.data$counties <- 'Total'
  if('Total' %in% these.data$sexes)
    these.data$sexes <- 'Total'
  if('Total' %in% these.data$ages)
    these.data$ages <- 'Total'
  
  local.df <- local.df %>% 
    filter(county %in% these.data$counties,
           sex %in% these.data$sexes,
           age_range %in% these.data$ages,
           date >= min(date) + these.data$pushtime[1] - 1,
           date < min(date) + these.data$pushtime[2])
  
  if(as.logical(these.data$normalize)) {
    local.df <- local.df %>% 
      group_by(county, age_range, sex) %>%
      summarise(sum.cases = sum(.data[[local.series]]) * population[1] / 1000000, pops = population[1]) %>%
      ungroup()
    
    value <- round(sum(local.df$sum.cases) * 1000000 / sum(local.df$pops))
  }
  else
    value <- sum(local.df[[local.series]])
  
  return(value)
}

renderValueBoxCases <- function(these.data, plotly.settings = F) {
  these.data <- constructSettings(these.data, plotly.settings)
  
  if(as.logical(these.data$normalize))
    subtitle <- 'Cases per Million'
  else
    subtitle <- 'Total Cases'
  
  valueBox(value = calculateValueBoxData(these.data, 'caseCount', plotly.settings), 
           subtitle = subtitle,
           icon = icon('thermometer', lib = "font-awesome"),
           color = 'yellow')
}

renderValueBoxHosp <- function(these.data, plotly.settings = F) {
  these.data <- constructSettings(these.data, plotly.settings)
  
  if(as.logical(these.data$normalize))
    subtitle <- 'Hospitalizations per Million'
  else
    subtitle <- 'Total Hospitalizations'
  
  valueBox(value = calculateValueBoxData(these.data, 'hospitalizedCount', plotly.settings), 
           subtitle = subtitle,
           icon = icon('hospital-o', lib = 'font-awesome'),
           color = 'red')
}

renderValueBoxDeaths <- function(these.data, plotly.settings = F) {
  these.data <- constructSettings(these.data, plotly.settings)
  
  if(as.logical(these.data$normalize))
    subtitle <- 'Deaths per Million'
  else
    subtitle <- 'Total Deaths'
  
  valueBox(value = calculateValueBoxData(these.data, 'deathCount', plotly.settings), 
           subtitle = subtitle,
           icon = icon('frown-o', lib = 'font-awesome'),
           color = 'purple')
}