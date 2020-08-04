renderMap <- function(these.data) {
  cty_sf <- counties_sf("longlat")
  ohio_sf <- cty_sf[cty_sf$state == 'Ohio', ]
  
  s <- these.data$map.series3
  
  if(as.logical(these.data$normalize3)) {
    local.df <- normalized.df[normalized.df$sex %in% these.data$sexes3 & normalized.df$age_range %in% these.data$ages3, ]
    #this.prison_summary <- normalized_prison_summary
  }
  else {
    local.df <- ohio.df[ohio.df$sex %in% these.data$sexes3 & ohio.df$age_range %in% these.data$ages3, ]
    #this.prison_summary <- prison_summary
  }
  
  if('Total' %in% these.data$ages3)
    local.df <- local.df[local.df$age_range == 'Total', ]
  if('Total' %in% these.data$sexes3)
    local.df <- local.df[local.df$sex == 'Total', ]
  
  ohio.summary.df <- local.df %>% 
    group_by(county) %>% 
    filter(date >= min(date) + these.data$map.smooth3[1] - 1,
           date < min(date) + these.data$map.smooth3[2]) %>%
    summarise(caseCount = sum(caseCount), 
              deathCount = sum(deathCount), 
              hospitalizedCount = sum(hospitalizedCount)) %>%
    ungroup()
  
  #if(as.logical(these.data$prisoners3)) {
  #  tmp.df <- ohio.summary.df[na.omit(match(this.prison_summary$county, ohio.summary.df$county)), ]
  #  tmp.df$caseCount <- tmp.df$caseCount - this.prison_summary$caseCount
  #  tmp.df$deathCount <- tmp.df$deathCount - this.prison_summary$deathCount
  #  
  #  ohio.summary.df$caseCount[ohio.summary.df$county %in% tmp.df$county] <- tmp.df$caseCount
  #  ohio.summary.df$deathCount[ohio.summary.df$county %in% tmp.df$county] <- tmp.df$deathCount
  #}
  
  todays.ohio.df <- ohio.summary.df[match(as.character(ohio_sf$name), ohio.summary.df$county), ]
  ohio_sf <- bind_cols(ohio_sf, todays.ohio.df)
  ohio_sf$mid <- ohio_sf$geometry
  
  if(s == 'caseCount') {
    this.legend.title <- 'Number of COVID-19 cases'
    tooltip.label <- 'Cases:'
  }
  if(s == 'hospitalizedCount') {
    this.legend.title <- 'Number of COVID-19 hospitalized'
    tooltip.label <- 'Hospitalizations:'
  }
  if(s == 'deathCount') {
    this.legend.title <- 'Number of COVID-19 deaths'
    tooltip.label <- 'Deaths:'
  }
  
  if(s == 'caseCount' & as.logical(these.data$normalize3)) {
    this.legend.title <- 'COVID-19 cases per million'
    tooltip.label <- 'Cases per million:'
  }
  if(s == 'hospitalizedCount' & as.logical(these.data$normalize3)) {
    this.legend.title <- 'COVID-19 hospitalizations per million'
    tooltip.label <- 'Hospitalizations per million:'
  }
  if(s == 'deathCount' & as.logical(these.data$normalize3)) {
    this.legend.title <- 'COVID-19 deaths per million'
    tooltip.label <- 'Deaths per million:'
  }
  
  tooltip.func <- function(dat) {
    this.list <- unlist(lapply(1:nrow(dat), function(i) paste('County:', dat$name[i], '\n', 
                                                              tooltip.label, as.character(dat[[s]][i]))))
    return(this.list)
  }
  
  p <- ggplot(ohio_sf) + 
    geom_sf(colour = "white") +
    geom_sf_interactive(aes(geometry = mid,
                            fill = .data[[s]],
                            data_id = name,
                            tooltip = tooltip.func(ohio_sf))
    ) +
    theme_map(24) +
    theme(
      legend.title.align = 0.5,
      legend.text.align = 0.5,
      legend.justification = c(0, 0),
      legend.position = c(0.65, 0.0)
    ) +
    labs(fill = this.legend.title)
  
  if(these.data$transformation3 == 'none')
    p <- p +
    scale_fill_continuous_sequential(
      palette = "Blues",
      rev = TRUE,
      na.value = "grey90",
      guide = guide_colorbar(
        direction = "horizontal",
        label.position = "bottom",
        title.position = "top",
        barwidth = grid::unit(5.0, "in"),
        barheight = grid::unit(0.5, "in")
      )
    )
  
  if(these.data$transformation3 == 'log10')
    p <- p +
    scale_fill_continuous_sequential(
      trans = 'log10',
      palette = "Blues",
      rev = TRUE,
      na.value = "grey90",
      guide = guide_colorbar(
        direction = "horizontal",
        label.position = "bottom",
        title.position = "top",
        barwidth = grid::unit(5.0, "in"),
        barheight = grid::unit(0.5, "in")
      )
    )
  return(p)
}