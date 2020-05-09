renderTimeSeries <- function(these.data, these.colors, plotly.settings = F) {
  
  these.data <- constructSettings(these.data, plotly.settings)
  
  s <- these.data$series
  
  highlights <- these.data$highlights
  
  # pick local data to use from and normalization options
  if(as.logical(these.data$normalize)) {
    local.df <- normalized.df %>% filter(county %in% these.data$counties)
    this.prison_summary <- normalized_prison_summary
  }
  else {
    local.df <- ohio.df %>% filter(county %in% these.data$counties)
    this.prison_summary <- prison_summary
  }
  
  # smooth data if requested
  local.df <- local.df %>% 
    group_by(county, sex, age_range) %>%
    select(c(date, all_of(s))) %>% 
    arrange(date) %>%
    mutate(!!s := round(rollmeanr(.data[[s]], as.numeric(these.data$smooth), fill = NA))) %>%
    ungroup()
  
  # get only the requested set of data for local use
  local.df <- local.df %>%
    filter(sex %in% these.data$sexes,
           age_range %in% these.data$ages)
  
  s1.df <- local.df %>%
    filter(date < (min(date) + these.data$pushtime[1]))
  s2.df <- local.df %>%
    filter(date >= (min(date) + these.data$pushtime[2] - 1))
  
  local.df <- local.df %>%
    filter(date >= (min(date) + these.data$pushtime[1] - 1) &
             date < (min(date) + these.data$pushtime[2]))
  
  # find starting location for alignments or exponentials
  if(these.data$num_align > 0 | as.logical(these.data$exp)) {
    local.df.exp <- local.df %>%
      arrange(-desc(county)) %>%
      group_by(county) %>%
      mutate(start = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE)) %>%
      ungroup()
    
    if(as.logical(these.data$drop)) {
      doubling.df <- local.df.exp %>%
        filter(date >= start &
                 sex %in% these.data$sexes &
                 age_range %in% these.data$ages)
    }
    else if (these.data$num_align == 0) {
      doubling.df <- local.df.exp %>%
        filter(date >= start &
                 date < (min(date, na.rm = TRUE) + these.data$pushtime[2]) &
                 sex %in% these.data$sexes &
                 age_range %in% these.data$ages)
    } else {
      doubling.df <- local.df.exp %>%
        filter(date >= start &
                 sex %in% these.data$sexes &
                 age_range %in% these.data$ages)
    }
    
    if(length(highlights) > 0)
      doubling.df <- doubling.df %>% filter(county %in% highlights)
    
    minimums <- doubling.df %>%
      arrange(-desc(county)) %>%
      group_by(county) %>%
      filter(date == min(date)) %>%
      ungroup()
  }
  
  # create exponential growth guides
  if(as.logical(these.data$exp)) {
    low <- min(minimums[[s]])
    high <- max(minimums[[s]])
    
    if(low == 0)
      low <- 1
    if(high == 0)
      high <- 1
    
    start <- 10 ^ mean(c(log10(low), log10(high)))
    
    date_seq <- 0:(max(doubling.df$date) - min(doubling.df$date))
    
    ys <- lapply(c(2, 3, 5, 7), function(x) doubling_time(start, x, date_seq))
    
    if(!as.logical(these.data$drop) & these.data$num_align > 0) {
      dates <- rep(date_seq + these.data$pushtime[1] - 1, 4)
    } else {
      dates <- rep(min(doubling.df$date) + days(date_seq), 4)
    }
    
    exp.df <- tibble(date = dates,
                     y = unlist(ys),
                     ds = c(rep('2 days', length(date_seq)),
                            rep('3 days', length(date_seq)),
                            rep('5 days', length(date_seq)),
                            rep('7 days', length(date_seq))))
    
    # cap the exponential growth guides at the maximum for the current y-values
    exp.df <- exp.df %>%
      filter(y <= max(local.df[[s]]))
  }
  
  # align the data after above calculations
  if(!as.logical(these.data$drop) & these.data$num_align > 0) {
    local.df <- local.df.exp %>%
      filter(date >= start) %>%
      group_by(county) %>%
      mutate(date = date - min(date) + these.data$pushtime[1] - 1) %>%
      ungroup()
  } else if (these.data$num_align > 0) {
    local.df <- local.df.exp %>%
      filter(date >= start) %>%
      group_by(county) %>%
      mutate(date = date - min(date)) %>%
      ungroup()
    if(as.logical(these.data$exp)) {
      exp.df <- exp.df  %>%
        mutate(date = date - min(date)) %>%
        ungroup()
    }
  }
  
  # get only the local colors
  local.colors <- unlist(these.colors[unique(local.df$county)])
  
  # set colors for non-highlighted counties to gray
  if(length(highlights) > 0) {
    sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
  }
  
  # define base sizes
  base.size <- 16
  point.size <- 3.5
  line.size <- 1.5
  font.size <- 18
  
  # change sizes for plotly
  if(plotly.settings) {
    base.size <- 12
    point.size <- 2.0
    line.size <- 0.8
    font.size <- 13
  }
  
  # establish plot canvas
  p <- ggplot()
  
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
  
  # make spare data sets to gray out point not in select time frame
  if(!as.logical(these.data$drop) & these.data$num_align == 0){
    p <- p + geom_line(data = s1.df,
                       aes(x = date,
                           y = .data[[s]],
                           group = interaction(county, age_range, sex)),
                       color = "#DEDEDE",
                       fill = "#DEDEDE",
                       size = line.size,
                       alpha = 0.4) +
      geom_point(data = s1.df %>% filter(date < max(date)),
                 aes(x = date,
                     y = .data[[s]],
                     group = interaction(county, age_range, sex)),
                 color = "#DEDEDE",
                 fill = "#DEDEDE",
                 size = point.size,
                 alpha = 0.4)
    
    p <- p + geom_line(data = s2.df,
                       aes(x = date,
                           y = .data[[s]],
                           group = interaction(county, age_range, sex)),
                       color = "#DEDEDE",
                       fill = "#DEDEDE",
                       size = line.size,
                       alpha = 0.4) +
      geom_point(data = s2.df %>% filter(date > min(date)),
                 aes(x = date,
                     y = .data[[s]],
                     group = interaction(county, age_range, sex)),
                 color = "#DEDEDE",
                 fill = "#DEDEDE",
                 size = point.size,
                 alpha = 0.4)
  }
  
  plottable.df <- local.df %>% filter(!(county %in% highlights))
  
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
          nrow = ceiling(length(unique(local.df$county)) / 6),
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
    label.df <- exp.df %>% 
      group_by(ds) %>%
      summarise(date = max(date),
                y = max(y),
                label = ds[1]) %>%
      ungroup()
    
    p <- p + geom_line(data = exp.df,
                       aes(x = date,
                           y = y,
                           group = ds),
                       color = 'gray50',
                       alpha = 0.8,
                       size = line.size * 0.9,
                       linetype = "dashed")
    
    if(plotly.settings) {
      this.subtract <- (max(exp.df$date) - min(exp.df$date)) * 0.04
      p <- p + geom_text(data = label.df,
                         aes(x = date - this.subtract, 
                             y = y, 
                             label = label),
                         size = 5,
                         color = 'gray50')
    }
    else {
      p <- p + geom_text_repel(data = label.df,
                               aes(x = date, 
                                   y = y, 
                                   label = label),
                               size = 6,
                               hjust = 0,
                               color = 'gray50')
    }
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
  
  if(these.data$num_align > 0) {
    p <- p + xlab('Days since alignment number passed')
  }
  
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
