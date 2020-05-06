calculateValueBoxData <- function(these.data, local.series, plotly.settings) {
  these.data <- constructSettings(these.data, plotly.settings)
  
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
           age_range %in% these.data$ages)
  
  value <- sum(local.df[[local.series]])
}

renderValueBoxCases <- function(these.data, plotly.settings = F) {
  valueBox(value = calculateValueBoxData(these.data, 'caseCount', plotly.settings), 
           subtitle = "Cases",
           icon = icon('thermometer', lib = "font-awesome"),
           color = 'yellow')
}

renderValueBoxHosp <- function(these.data, plotly.settings = F) {
  valueBox(value = calculateValueBoxData(these.data, 'hospitalizedCount', plotly.settings), 
           subtitle = "Hospitalizations",
           icon = icon('hospital-o', lib = 'font-awesome'),
           color = 'red')
}

renderValueBoxDeaths <- function(these.data, plotly.settings = F) {
  valueBox(value = calculateValueBoxData(these.data, 'deathCount', plotly.settings), 
           subtitle = "Deaths",
           icon = icon('frown-o', lib = 'font-awesome'),
           color = 'purple')
}