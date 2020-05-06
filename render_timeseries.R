renderTimeSeries <- function(these.data, these.colors, plotly.settings = F) {
  
  if(plotly.settings) {
    these.data <- list(
      counties = these.data$counties2,
      highlights = these.data$highlights2,
      series = these.data$series2,
      transformation = these.data$transformation2,
      ages = these.data$ages2,
      sexes = these.data$sexes2,
      align = these.data$align2,
      num_align = these.data$num_align2,
      exponentials = these.data$exponentials2,
      normalize = these.data$normalize2,
      smooth = these.data$smooth2
    )
  }
  else{
    these.data <- list(
      counties = these.data$counties1,
      highlights = these.data$highlights1,
      series = these.data$series1,
      transformation = these.data$transformation1,
      ages = these.data$ages1,
      sexes = these.data$sexes1,
      align = these.data$align1,
      num_align = these.data$num_align1,
      exponentials = these.data$exponentials1,
      normalize = these.data$normalize1,
      smooth = these.data$smooth1
    )
  }
  
  s <- these.data$series
  
  highlights <- these.data$highlights
  
  # pick local data to use from and normalization options
  if(as.logical(these.data$normalize)) {
    local.df <- normalized.df[normalized.df$county %in% these.data$counties, ]
    this.prison_summary <- normalized_prison_summary
  }
  else {
    local.df <- ohio.df[ohio.df$county %in% these.data$counties, ]
    this.prison_summary <- prison_summary
  }
  
  # smooth data if requested
  local.df <- local.df %>% 
    group_by(county, sex, age_range) %>%
    select(c(date, all_of(s))) %>% 
    arrange(date) %>%
    mutate(!!s := round(rollmeanr(.data[[s]], as.numeric(these.data$smooth), fill = NA)))
  
  # generate alignment of data and create exponential growth guides
  if(as.logical(these.data$align)) {
    
    start_dates <- local.df %>%
      group_by(county) %>%
      summarise(start_date = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE))
    
    if(nrow(start_dates) > 1)
      local.df <- local.df[order(local.df$county), ][unlist(sapply(1:nrow(start_dates), 
                                                                   function(x) 
                                                                     local.df$date[local.df$county == start_dates$county[x]] >= start_dates$start_date[x])), ]
    
    doubling.df <- local.df[local.df$sex %in% these.data$sexes & local.df$age_range %in% these.data$ages, ]
    
    if(length(highlights) > 0)
      doubling.df <- doubling.df[doubling.df$county %in% highlights, ]
    
    if(nrow(start_dates) > 1)
      minimums <- doubling.df[order(doubling.df$county), ][unlist(lapply(1:nrow(start_dates), 
                                                                         function(x) 
                                                                           doubling.df$date[doubling.df$county == start_dates$county[x]] == start_dates$start_date[x])), ]
    
    if(as.logical(these.data$exp)) {
      low <- min(minimums[[s]])
      high <- max(minimums[[s]])
      
      if(low == 0)
        low <- 1
      if(high == 0)
        high <- 1
      
      start <- 10^mean(c(log10(low), log10(high)))
      
      date_seq <- 0:(max(doubling.df$date) - min(doubling.df$date))
      ys <- lapply(c(2, 3, 5, 7), function(x) doubling_time(start, x, date_seq))
      
      exp.df <- tibble(date = rep(min(doubling.df$date) + days(date_seq), 4),
                       y = unlist(ys),
                       ds = c(rep('2 days', length(date_seq)),
                              rep('3 days', length(date_seq)),
                              rep('5 days', length(date_seq)),
                              rep('7 days', length(date_seq))))
      
      exp.df$date <- exp.df$date - min(doubling.df$date)
    }
    
    local.df <- local.df %>% group_by(county) %>% mutate(date = date - min(date))
  }
  
  # get only the requested set of data for local use
  local.df <- local.df[local.df$sex %in% these.data$sexes & local.df$age_range %in% these.data$ages, ]
  
  # get only the local colors
  local.colors <- unlist(these.colors[unique(local.df$county)])
  
  # set colors for non-highlighted counties to gray
  if(length(highlights) > 0) {
    sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
  }
  
  # establish plot canvas
  p <- ggplot()
  
  # define base sizes
  base.size <- 14
  point.size <- 3.5
  line.size <- 1.5
  font.size <- 16
  
  # change sizes for plotly
  if(plotly.settings) {
    base.size <- 12
    point.size <- 2.0
    line.size <- 1.0
    font.size <- 13
  }
  
  if(these.data$transformation != 'none')
    p <- p + scale_y_continuous(trans = these.data$transformation)
  
  p <- p + theme_minimal_hgrid(base.size, rel_small = 1) +
    theme(legend.position = "bottom",
          legend.justification = "right",
          legend.text = element_text(size = base.size),
          legend.box.spacing = unit(0, "pt"),
          legend.title = element_blank(),
          axis.title = element_text(size = font.size),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))
    ) 
  
  if(plotly.settings)
    p <- p + theme(legend.position = 'none')
  
  plottable.df <- local.df[!(local.df$county %in% highlights), ]
  
  if(s == 'caseCount')
    tooltip.label <- 'Daily Cases:'
  if(s == 'hospitalizedCount')
    tooltip.label <- 'Daily Hospitalizations:'
  if(s == 'deathCount')
    tooltip.label <- 'Daily Deaths:'
  if(s == 'aggregateCaseCount')
    tooltip.label <- 'Aggregate Cases:'
  if(s == 'aggregateHospitalizedCount')
    tooltip.label <- 'Aggregate Hospitalizations:'
  if(s == 'aggregateDeathCount')
    tooltip.label <- 'Aggregate Deaths:'
  
  if(s == 'caseCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Daily Cases per million:'
  if(s == 'hospitalizedCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Daily Hospitalizations per million:'
  if(s == 'deathCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Daily Deaths per million:'
  if(s == 'aggregateCaseCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Aggregate Cases per million:'
  if(s == 'aggregateHospitalizedCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Aggregate Hospitalizations per million:'
  if(s == 'aggregateDeathCount' & as.logical(these.data$normalize))
    tooltip.label <- 'Aggregate Deaths per million:'
  
  if(length(these.data$sexes) == 1 & length(these.data$ages) == 1) {
    tooltip.func <- function(dat) {
      this.list <- unlist(lapply(1:nrow(dat), function(i) paste('County:', dat$county[i], '\n',
                                                                'Date:', dat$date[i], '\n',
                                                                tooltip.label, as.character(dat[[s]][i]))))
      return(this.list)
    }
    
    p <- p +
      geom_line(data = plottable.df,
                aes(x = date, 
                    y = .data[[s]], 
                    color = county),
                size = line.size, 
                alpha = 0.3) + 
      geom_point(data = plottable.df,
                 aes(x = date, 
                     y = .data[[s]], 
                     color = county,
                     fill = county,
                     text = tooltip.func(plottable.df)),
                 size = point.size, 
                 alpha = 0.6) +
      xlab('') +
      scale_color_manual(
        name = NULL,
        values = local.colors
      ) +
      scale_fill_manual(
        name = NULL,
        values = local.colors
      ) +
      guides(
        color = guide_legend(
          nrow = ceiling(length(unique(local.df$county)) / 5),
          override.aes = list(
            linetype = c(rep(0, length(unique(local.df$county)))),
            shape = c(rep(21, length(unique(local.df$county))))
          )
        )
      ) 
  }
  
  if(length(these.data$sexes) > 1 & length(these.data$counties) == 1) {
    tmp.col <- unlist(these.colors)
    names(tmp.col) <- NULL
    tmp.col <- sample(tmp.col, length(these.data$sexes))
    
    tooltip.func <- function(dat) {
      this.list <- unlist(lapply(1:nrow(dat), function(i) paste('County:', these.data$counties[1], '\n', 
                                                                'Date:', dat$date[i], '\n',
                                                                'Sex:', as.character(dat[['sex']][i]), '\n',
                                                                tooltip.label, as.character(dat[[s]][i]))))
      return(this.list)
    }
    
    p <- p +
      geom_line(data = plottable.df,
                aes(x = date, 
                    y = .data[[s]], 
                    color = sex),
                size = line.size, 
                alpha = 0.3) + 
      geom_point(data = plottable.df,
                 aes(x = date, 
                     y = .data[[s]], 
                     color = sex,
                     fill = sex,
                     text = tooltip.func(plottable.df)),
                 size = point.size, 
                 alpha = 0.6) +
      xlab('') +
      scale_fill_manual(name = NULL, values = tmp.col) +
      scale_color_manual(name = NULL, values = tmp.col) +
      guides(
        color = guide_legend(
          nrow = ceiling(length(unique(local.df$sex)) / 5),
          override.aes = list(
            linetype = c(rep(0, length(unique(local.df$sex)))),
            shape = c(rep(21, length(unique(local.df$sex))))
          )
        )
      ) 
  }
  
  if(length(these.data$ages) > 1 & length(these.data$counties) == 1) {
    tmp.col <- unlist(these.colors)
    names(tmp.col) <- NULL
    tmp.col <- sample(tmp.col, length(these.data$ages))
    
    tooltip.func <- function(dat) {
      this.list <- unlist(lapply(1:nrow(dat), function(i) paste('County:', these.data$counties[1], '\n', 
                                                                'Date:', dat$date[i], '\n',
                                                                'Age Range:', as.character(dat[['age_range']][i]), '\n',
                                                                tooltip.label, as.character(dat[[s]][i]))))
      return(this.list)
    }
    
    p <- p +
      geom_line(data = plottable.df,
                aes(x = date, 
                    y = .data[[s]], 
                    color = age_range),
                size = line.size, 
                alpha = 0.3) + 
      geom_point(data = plottable.df,
                 aes(x = date, 
                     y = .data[[s]], 
                     color = age_range,
                     fill = age_range,
                     text = tooltip.func(plottable.df)),
                 size = point.size, 
                 alpha = 0.6) +
      xlab('') +
      scale_fill_manual(name = NULL, values = tmp.col) +
      scale_color_manual(name = NULL, values = tmp.col) +
      guides(
        color = guide_legend(
          nrow = ceiling(length(unique(local.df$age_range)) / 5),
          override.aes = list(
            linetype = c(rep(0, length(unique(local.df$age_range)))),
            shape = c(rep(21, length(unique(local.df$age_range))))
          )
        )
      )
  }
  
  if(as.logical(these.data$exp)) {
    this.max.x <- max(exp.df$date)
    this.max.y.multi <- 1.1
    this.size <- 6
    this.increment <- 0.5
    
    if(plotly.settings) {
      this.max.x <- max(exp.df$date) * 0.9
      this.max.y.multi <- 1.2
      this.size <- 4
      this.increment <- 0.25
    }
    
    p <- p + geom_line(data = exp.df,
                       aes(x = date,
                           y = y,
                           group = ds),
                       color = 'gray50',
                       alpha = 0.8,
                       size = line.size * 0.9,
                       linetype = "dashed") +
      annotate("text",
               x = this.max.x * 0.99,
               y = this.max.y.multi * max(exp.df$y[exp.df$ds == '1 day']),
               label = "doubling in day",
               size = this.size,
               hjust = 1,
               vjust = 0,
               color = 'gray50',
               alpha = 1) +
      annotate("text",
               x = this.max.x,
               y = this.max.y.multi * max(exp.df$y[exp.df$ds == '2 days']),
               label = "doubling in 2 days",
               size = this.size - this.increment * 1,
               hjust = 1,
               vjust = -0.25,
               color = 'gray50',
               alpha = 1) +
      annotate("text",
               x = this.max.x,
               y = this.max.y.multi * max(exp.df$y[exp.df$ds == '3 days']),
               label = "doubling in 3 days",
               size = this.size - this.increment * 2,
               hjust = 1,
               vjust = -0.25,
               color = 'gray50',
               alpha = 1) +
      annotate("text",
               x = this.max.x,
               y = this.max.y.multi * max(exp.df$y[exp.df$ds == '5 days']),
               label = "doubling in 5 days",
               size = this.size - this.increment * 3,
               hjust = 1,
               vjust = -0.25,
               color = 'gray50',
               alpha = 1) +
      annotate("text",
               x = this.max.x,
               y = this.max.y.multi * max(exp.df$y[exp.df$ds == '7 days']),
               label = "doubling in 7 days",
               size = this.size - this.increment * 4,
               hjust = 1,
               vjust = -0.25,
               color = 'gray50',
               alpha = 1)
  }
  
  if(length(highlights) > 0) {
    highlights.df <- local.df[local.df$county %in% highlights, ]
    p <- p + geom_line(data = highlights.df,
                       aes(x = date,
                           y = .data[[s]],
                           color = county),
                       size = line.size,
                       alpha = 0.8) + 
      geom_point(data = highlights.df,
                 aes(x = date, 
                     y = .data[[s]], 
                     color = county,
                     fill = county,
                     text = tooltip.func(highlights.df)),
                 size = point.size, 
                 alpha = 0.6)
  }
  
  if(as.logical(these.data$align))
    p <- p + xlab(paste('Days since alignment number'))
  
  if(s == 'caseCount')
    this.legend.title <- 'Daily number of COVID-19 cases'
  if(s == 'hospitalizedCount')
    this.legend.title <- 'Daily number of COVID-19 hospitalized'
  if(s == 'deathCount')
    this.legend.title <- 'Daily number of COVID-19 deaths'
  if(s == 'aggregateCaseCount')
    this.legend.title <- 'Aggregate number of COVID-19 cases'
  if(s == 'aggregateHospitalizedCount')
    this.legend.title <- 'Aggregate number of COVID-19 hospitalized'
  if(s == 'aggregateDeathCount')
    this.legend.title <- 'Aggregate number of COVID-19 deaths'
  
  if(s == 'caseCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Daily COVID-19 cases per million'
  if(s == 'hospitalizedCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Daily COVID-19 hospitalizations per million'
  if(s == 'deathCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Daily COVID-19 deaths per million'
  if(s == 'aggregateCaseCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Aggregate COVID-19 cases per million'
  if(s == 'aggregateHospitalizedCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Aggregate COVID-19 hospitalizations per million'
  if(s == 'aggregateDeathCount' & as.logical(these.data$normalize))
    this.legend.title <- 'Aggregate COVID-19 deaths per million'
  
  p <- p + ylab(this.legend.title)
  
  return(p)
}
