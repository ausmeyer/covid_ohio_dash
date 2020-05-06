
set.seed(5)
source('load_libraries.R')
source('load_settings.R')
source('accessory_fxn.R')

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(includeHTML(("google-analytics.html"))),
  useShinyjs(),
  
  navbarPage(theme = shinytheme("yeti"), 
             collapsible = TRUE,
             "Ohio COVID-19 tracker", id="nav",
             # Sidebar with a slider input for number of bins 
             tabPanel("Basic Timeseries", 
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
             tabPanel('Interactive Timeseries', 
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
             tabPanel("Data",
                      dataTableOutput("dataTable") %>% withSpinner()),
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
  
  source('render_timeseries.R')
  source('render_map.R')
  source('build_shiny_plots.R')
  
  inputData <- isolate({reactive({
    list(map.series3 = input$seriesChoice3,
         transformation3 = input$transformation3,
         ages3 = input$ageRange3,
         sexes3 = input$sex3,
         normalize3 = input$normalize3,
         prisoners3 = input$prisoners3,
         map.smooth3 = input$smooth3,
         counties2 = input$countyChoice2,
         highlights2 = input$highlightSet2,
         series2 = input$seriesChoice2,
         transformation2 = input$transformation2,
         ages2 = input$ageRange2,
         sexes2 = input$sex2,
         align2 = input$align2,
         num_align2 = input$num_align2,
         exponentials2 = input$exponentials2,
         normalize2 = input$normalize2,
         smooth2 = input$smooth2,
         counties1 = input$countyChoice1,
         highlights1 = input$highlightSet1,
         series1 = input$seriesChoice1,
         transformation1 = input$transformation1,
         ages1 = input$ageRange1,
         sexes1 = input$sex1,
         align1 = input$align1,
         num_align1 = input$num_align1,
         exponentials1 = input$exponentials1,
         normalize1 = input$normalize1,
         smooth1 = input$smooth1)
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
    input.settings <- inputData()
    
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
  
  observe({
    input.settings <- inputData()
    build.plots(input.settings, output)
  })
  
  observe({
    shuffleColors1()
    build.plots(input.settings = inputData(), output)
  })
  
  observe({
    shuffleColors2()
    build.plots(input.settings = inputData(), output)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
