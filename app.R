#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library('shiny')
library('shinyjs')
library('shinyWidgets') 
library('shinydashboard')
library('shinythemes')
library('plotly')
library('tidyverse')
library('lubridate')
library('cowplot')
library('lemon')
library('colorspace')
library('scales')
library('shinycssloaders')
library('sf')
library('albersusa')
library('hues')
library('zoo')
library('ggiraph')

set.seed(5)
options(spinner.color="#6699cc")
suppressWarnings(load('data.rda'))
suppressWarnings(load('prison.rda'))

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

input.settings <- c()

doubling_time <- function(N0, d0, ts) {
  N0 * 2 ^ (ts / d0)
}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(includeHTML(("google-analytics.html"))),
  useShinyjs(),
  
  navbarPage(theme = shinytheme("yeti"), 
             collapsible = TRUE,
             "Ohio COVID-19 tracker", id="nav",
             # Sidebar with a slider input for number of bins 
             tabPanel("Basic Plot", 
                      sidebarPanel(width = 4,
                                   div(style = 'margin-top: -15px; margin-bottom: -5px',
                                       fluidRow(
                                         column(12,
                                                pickerInput("countyChoice1", 
                                                            h4("Included Counties"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.choices,
                                                            selected = all.choices[-1]
                                                )
                                         ),
                                         column(12,
                                                pickerInput("highlightSet1", 
                                                            h4("Highlighted Counties"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.choices,
                                                            selected = NULL)
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("seriesChoice1", 
                                                            h4("Data"), 
                                                            choices = all.series,
                                                            selected = all.series[4]))),
                                       fluidRow(
                                         column(12,
                                                pickerInput("transformation1", 
                                                            h4("y-axis"), 
                                                            choices = all.transformations,
                                                            selected = all.transformations[1])
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                radioButtons("normalize1", 
                                                             h4("Normalize by Population"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("ageRange1", 
                                                            h4("Ages"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.ages,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("sex1", 
                                                            h4("Sex"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.sexes,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                numericInput("smooth1", 
                                                             h5("Smooth over Window (Days)"), 
                                                             value = 1)
                                         )
                                       ),
                                       fluidRow(
                                         column(6,
                                                radioButtons("align1", 
                                                             h4("Align"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         ),
                                         column(6,
                                                radioButtons("exponentials1", 
                                                             h4("Guide"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                numericInput("num_align1", 
                                                             h4("Align on Number"), 
                                                             value = 0)
                                         )
                                       ),
                                       fluidRow(
                                         align = 'center',
                                         column(12,
                                                actionButton('shuffle_colors1',
                                                             'Shuffle Colors'),
                                         )
                                       )
                                   )
                      ),
                      mainPanel(width = 8, plotOutput("casesPlot", height = 1200 * 5 / 7) %>% withSpinner())
             ),
             tabPanel('Interactive Plot', 
                      sidebarPanel(width = 4,
                                   div(style = 'margin-top: -15px; margin-bottom: -5px',
                                       fluidRow(
                                         column(12,
                                                pickerInput("countyChoice2", 
                                                            h4("Included Counties"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.choices,
                                                            selected = all.choices[-1]
                                                )
                                         ),
                                         column(12,
                                                pickerInput("highlightSet2", 
                                                            h4("Highlighted Counties"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.choices,
                                                            selected = NULL)
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("seriesChoice2", 
                                                            h4("Data"), 
                                                            choices = all.series,
                                                            selected = all.series[4]))),
                                       fluidRow(
                                         column(12,
                                                pickerInput("transformation2", 
                                                            h4("y-axis"), 
                                                            choices = all.transformations,
                                                            selected = all.transformations[1])
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                radioButtons("normalize2", 
                                                             h4("Normalize by Population"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("ageRange2", 
                                                            h4("Ages"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.ages,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("sex2", 
                                                            h4("Sex"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.sexes,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                numericInput("smooth2", 
                                                             h5("Smooth over Window (Days)"), 
                                                             value = 1)
                                         )
                                       ),
                                       fluidRow(
                                         column(6,
                                                radioButtons("align2", 
                                                             h4("Align"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         ),
                                         column(6,
                                                radioButtons("exponentials2", 
                                                             h4("Guide"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                numericInput("num_align2", 
                                                             h4("Align on Number"), 
                                                             value = 0)
                                         )
                                       ),
                                       fluidRow(
                                         align = 'center',
                                         column(12,
                                                actionButton('shuffle_colors2',
                                                             'Shuffle Colors'),
                                         )
                                       )
                                   )
                      ),
                      mainPanel(width = 8, plotlyOutput('casesPlotly') %>% withSpinner())
             ),
             tabPanel("Map", 
                      sidebarPanel(width = 4,
                                   div(style = 'margin-top: -15px; margin-bottom: -5px',
                                       fluidRow(
                                         column(12,
                                                pickerInput("seriesChoice3", 
                                                            h4("Data"), 
                                                            choices = map.series,
                                                            selected = map.series[1]))),
                                       fluidRow(
                                         column(12,
                                                numericInput("smooth3", 
                                                             h5("Sum over Last # Days (default is all)"), 
                                                             value = length(unique(ohio.df$date)))
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("transformation3", 
                                                            h4("Transformation"), 
                                                            choices = all.transformations,
                                                            selected = all.transformations[1])
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                radioButtons("normalize3", 
                                                             h4("Normalize by Population"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("ageRange3", 
                                                            h4("Ages"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.ages,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                pickerInput("sex3", 
                                                            h4("Sex"), 
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            choices = all.sexes,
                                                            selected = 'Total')
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                radioButtons("prisoners3", 
                                                             h5("Remove Prisoners (Map only)"), 
                                                             choices = list('Yes' = T, 'No' = F),
                                                             selected = list('No' = F)
                                                )
                                         )
                                       )
                                   )
                      ),
                      mainPanel(width = 8, girafeOutput("mapPlot") %>% withSpinner())
             ),
             tabPanel("About",
                      
                      h4("Explanation of site functionality"),
                      
                      tags$ul(
                        tags$li("Charts will build automatically 1.5 seconds after changing any parameter."),
                        tags$li("The Counties menu provides selection of counties of interest."),
                        tags$li("The Highlights menu allows hightlighting particular counties on top of the selected counties."),
                        tags$li("The Data menu provides a selection of that available from the Ohio Department of Health; the data will also display on the map."),
                        tags$li("The y-axis menu allows selection of a transformation; the transformation will also affect the map."),
                        tags$li("The Ages menu provides selection of particular ages ranges, but to avoid confusion can only be used if only one county and sex is selected."),
                        tags$li("The Sex menu provides selection of particular sexes, but to avoid confusion can only be used if only one county and age is selected."),
                        tags$li("The Align option will align all of the counties selected to the first day that had at least the 'Align on Number' number of the selected Data."),
                        tags$li("The Guide option will overlay place a doubling time guide; it can only be selected if y-axis is Log10 and the data is Aligned."),
                        tags$li("For the Map, if 'Total' is included for ages or sex, the map will only use Total. If any other combination of ages or sexes is picked, it will sum the categories selected."),
                        tags$li("Due to lack of time series data, removing the prison population is only available on the map. Removing prisoners requires sex and age be set to 'Total' only."),
                        tags$li("At this time, only prisoner Total Cases and Total Deaths can be removed. Removing prisoners assumes the counts have been applied to the county where the prison is located."),
                      ),
                      
                      br(),
                      
                      h4("Complaints/Suggestions"),
                      a("@austingmeyer", href="https://twitter.com/austingmeyer"),
                      
                      br(),
                      
                      h4("Data"), 
                      a("https://coronavirus.ohio.gov/wps/portal/gov/covid-19/home/dashboard", href="https://coronavirus.ohio.gov/wps/portal/gov/covid-19/home/dashboard"),
                      
                      br(),
                      
                      h4("Very ugly code"), 
                      a("https://github.com/ausmeyer/covid_ohio_dash", href="https://github.com/ausmeyer/covid_ohio_dash")
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
  
  renderMap <- function(these.data) {
    cty_sf <- counties_sf("longlat")
    ohio_sf <- cty_sf[cty_sf$state == 'Ohio', ]
    
    s <- these.data$map.series3
    
    if(as.logical(these.data$normalize3)) {
      local.df <- normalized.df[normalized.df$sex %in% these.data$sexes3 & normalized.df$age_range %in% these.data$ages3, ]
      this.prison_summary <- normalized_prison_summary
    }
    else {
      local.df <- ohio.df[ohio.df$sex %in% these.data$sexes3 & ohio.df$age_range %in% these.data$ages3, ]
      this.prison_summary <- prison_summary
    }
    
    if('Total' %in% these.data$ages3)
      local.df <- local.df[local.df$age_range == 'Total', ]
    if('Total' %in% these.data$sexes3)
      local.df <- local.df[local.df$sex == 'Total', ]
    
    ohio.summary.df <- local.df %>% 
      group_by(county) %>% 
      filter(date > max(date) - these.data$map.smooth3) %>%
      summarise(caseCount = sum(caseCount), 
                deathCount = sum(deathCount), 
                hospitalizedCount = sum(hospitalizedCount))
    
    if(as.logical(these.data$prisoners3)) {
      tmp.df <- ohio.summary.df[na.omit(match(this.prison_summary$county, ohio.summary.df$county)), ]
      tmp.df$caseCount <- tmp.df$caseCount - this.prison_summary$caseCount
      tmp.df$deathCount <- tmp.df$deathCount - this.prison_summary$deathCount
      
      ohio.summary.df$caseCount[ohio.summary.df$county %in% tmp.df$county] <- tmp.df$caseCount
      ohio.summary.df$deathCount[ohio.summary.df$county %in% tmp.df$county] <- tmp.df$deathCount
    }
    
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
  
  inputData <- isolate({reactive({
    input.settings$map.series3 <<- input$seriesChoice3
    input.settings$transformation3 <<- input$transformation3
    input.settings$ages3 <<- input$ageRange3
    input.settings$sexes3 <<- input$sex3
    input.settings$normalize3 <<- input$normalize3
    input.settings$prisoners3 <<- input$prisoners3
    input.settings$map.smooth3 <<- input$smooth3
    
    input.settings$counties2 <<- input$countyChoice2
    input.settings$highlights2 <<- input$highlightSet2
    input.settings$series2 <<- input$seriesChoice2
    input.settings$transformation2 <<- input$transformation2
    input.settings$ages2 <<- input$ageRange2
    input.settings$sexes2 <<- input$sex2
    input.settings$align2 <<- input$align2
    input.settings$num_align2 <<- input$num_align2
    input.settings$exponentials2 <<- input$exponentials2
    input.settings$normalize2 <<- input$normalize2
    input.settings$smooth2 <<- input$smooth2
    
    input.settings$counties1 <<- input$countyChoice1
    input.settings$highlights1 <<- input$highlightSet1
    input.settings$series1 <<- input$seriesChoice1
    input.settings$transformation1 <<- input$transformation1
    input.settings$ages1 <<- input$ageRange1
    input.settings$sexes1 <<- input$sex1
    input.settings$align1 <<- input$align1
    input.settings$num_align1 <<- input$num_align1
    input.settings$exponentials1 <<- input$exponentials1
    input.settings$normalize1 <<- input$normalize1
    input.settings$smooth1 <<- input$smooth1
  }) %>% debounce(1500)})
  
  shuffleColors1 <- isolate({eventReactive(input$shuffle_colors1, {
    new.cols <<- iwanthue(length(unique(ohio.df$county)), random = T)
    sapply(1:length(unique(ohio.df$county)), function(x) colors.list[unique(ohio.df$county)[x]] <<- new.cols[x])
  })})
  
  shuffleColors2 <- isolate({eventReactive(input$shuffle_colors2, {
    new.cols <<- iwanthue(length(unique(ohio.df$county)), random = T)
    sapply(1:length(unique(ohio.df$county)), function(x) colors.list[unique(ohio.df$county)[x]] <<- new.cols[x])
  })})
  
  observe({
    inputData()
    
    if(input.settings$transformation1 == 'none' | !as.logical(input.settings$align1)) {
      updateRadioButtons(session, "exponentials1",
                         selected = list('No' = F))
    }
    
    shinyjs::toggleState("exponentials1", 
                         input.settings$transformation1 == 'log10' & 
                           input.settings$align1 == 'TRUE')
    
    if(input.settings$transformation2 == 'none' | !as.logical(input.settings$align2)) {
      updateRadioButtons(session, "exponentials2",
                         selected = list('No' = F))
    }
    
    shinyjs::toggleState("exponentials2", 
                         input.settings$transformation2 == 'log10' & 
                           input.settings$align2 == 'TRUE')
    
    if(!is.null(input.settings$ages3) &
       !is.null(input.settings$sexes3)) {
      if((input.settings$ages3 != 'Total' | 
          input.settings$sexes3 != 'Total') |
         length(input.settings$ages3) > 1 | 
         length(input.settings$sexes3) > 1) {
        
        updateRadioButtons(session, "prisoners3",
                           selected = list('No' = F))
      }
    }
    
    if(length(input.settings$ages3) <= 1 &
       length(input.settings$sexes3) <= 1 &
       'Total' %in% input.settings$ages3 & 
       'Total' %in% input.settings$sexes3) {
      shinyjs::enable('prisoners3') } else {
        shinyjs::disable('prisoners3')
      }
  })
  
  build.plots <- function() {
    this.validate1 <- function() {
      validate(
        need(length(input.settings$counties1) > 0, 
             "Please select a county set"),
        need(length(input.settings$ages1) > 0, 
             "Please select an age range"),
        need(length(input.settings$sexes1) > 0, 
             "Please select a sex"),
        need(all(input.settings$highlights1 %in% input.settings$counties1)  | 
               length(input.settings$highlights1) == 0, 
             "Please ensure highlights are in counties selected"),
        need(sum(c(length(input.settings$counties1) > 1, 
                   length(input.settings$ages1) > 1,
                   length(input.settings$sexes1) > 1)) <= 1, 
             "Please ensure only one of Counties, Ages, or Sex has more than one item selected"),
        need(length(input.settings$counties1) != length(input.settings$highlights1),
             "No need to highlight all of the selected counties. Unselect highlights.")
        
      )
    }
    
    this.validate2 <- function() {
      validate(
        need(length(input.settings$counties2) > 0, 
             "Please select a county set"),
        need(length(input.settings$ages2) > 0, 
             "Please select an age range"),
        need(length(input.settings$sexes2) > 0, 
             "Please select a sex"),
        need(all(input.settings$highlights2 %in% input.settings$counties2)  | 
               length(input.settings$highlights2) == 0, 
             "Please ensure highlights are in counties selected"),
        need(sum(c(length(input.settings$counties2) > 1, 
                   length(input.settings$ages2) > 1,
                   length(input.settings$sexes2) > 1)) <= 1, 
             "Please ensure only one of Counties, Ages, or Sex has more than one item selected"),
        need(length(input.settings$counties2) != length(input.settings$highlights2),
             "No need to highlight all of the selected counties. Unselect highlights.")
        
      )
    }
    
    this.validate3 <- function() {
      validate(
        need(length(input.settings$ages3) > 0, 
             "Please select an age range"),
        need(length(input.settings$sexes3) > 0, 
             "Please select a sex")
      )
    }
    
    output$casesPlot <- renderPlot({
      this.validate1()
      renderTimeSeries(input.settings, colors.list)
    })
    
    output$casesPlotly <- renderPlotly({
      this.validate2()
      gg.p <- ggplotly(renderTimeSeries(input.settings, 
                                        colors.list, 
                                        plotly.settings = T),
                       height = 1100 * 5 / 7,
                       tooltip = c('text')) %>%
        layout(font = list(family = 'Arial'),
               xaxis = list(title = list(standoff = 15, font = list(size = 20)), 
                            tickfont = list(size = 20),
                            automargin = T),
               yaxis = list(title = list(standoff = 15, font = list(size = 20)), 
                            tickfont = list(size = 20),
                            automargin = T))
      
      return(gg.p)
    })
    
    output$mapPlot <- renderGirafe({
      this.validate3()
      girafe(ggobj = renderMap(input.settings),
             width_svg = 20,
             height_svg = 20 * 5 / 7,
             options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
    
  }
  
  observe({
    inputData()
    build.plots()
  })
  
  observe({
    shuffleColors1()
    build.plots()
  })
  
  observe({
    shuffleColors2()
    build.plots()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
