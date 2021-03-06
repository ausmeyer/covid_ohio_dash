build.plots <- function(input.settings, input, output) {
  
  this.validate1 <- function(input.settings) {
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
           "No need to highlight all of the selected counties. Unselect highlights."),
      need((as.logical(input.settings$exponentials1) &  
              input.settings$series1 %in% c('aggregateCaseCount', 'aggregateHospitalizedCount', 'aggregateDeathCount')) |
             !(as.logical(input.settings$exponentials1)),
           "To show guides, the data must be aggegrate rather than daily"),
      need((input.settings$pushtime1[2] == length(unique(ohio.df$date)) & input.settings$pushtime1[1] == 1) |
             as.logical(input.settings$drop1) | 
             input.settings$num_align1 == 0,
           "You must either drop the outside of the Time Frame selection or not try to align. Doing both is confusing.")
    )
  }
  
  this.validate2 <- function(input.settings) {
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
           "No need to highlight all of the selected counties. Unselect highlights."),
      need((as.logical(input.settings$exponentials2) &  
              input.settings$series2 %in% c('aggregateCaseCount', 'aggregateHospitalizedCount', 'aggregateDeathCount')) |
             !(as.logical(input.settings$exponentials2)),
           "To show guides, the data must be aggegrate rather than daily"),
      need((input.settings$pushtime2[2] == length(unique(ohio.df$date)) & input.settings$pushtime2[1] == 1) |
             as.logical(input.settings$drop2) | 
             input.settings$num_align2 == 0,
           "You must either drop the outside of the Time Frame selection or not try to align. Doing both is confusing.")
      )
  }
  
  this.validate3 <- function(input.settings) {
    validate(
      need(length(input.settings$ages3) > 0, 
           "Please select an age range"),
      need(length(input.settings$sexes3) > 0, 
           "Please select a sex")
    )
  }
  
  output$casesPlot <- renderPlot({
    this.validate1(input.settings)
    renderTimeSeries(input.settings, colors.list)
  })
  
  output$casesPlotly <- renderPlotly({
    this.validate2(input.settings)
    gg.p <- ggplotly(renderTimeSeries(input.settings, 
                                      colors.list, 
                                      plotly.settings = T),
                     height = 1150 * 5 / 7,
                     tooltip = c('text')) %>%
      layout(font = list(family = 'Arial'),
             xaxis = list(title = list(standoff = 15, font = list(size = 18)), 
                          tickfont = list(size = 16),
                          automargin = T),
             yaxis = list(title = list(standoff = 15, font = list(size = 18)), 
                          tickfont = list(size = 16),
                          automargin = T))
    
    return(gg.p)
  })
  
  output$mapPlot <- renderGirafe({
    this.validate3(input.settings)
    girafe(ggobj = renderMap(input.settings),
           width_svg = 20,
           height_svg = 20 * 5 / 7,
           options = list(opts_selection(type = "single", only_shiny = FALSE)))
  })
  
  output$casesBox1 <- renderValueBox({ renderValueBoxCases(input.settings, plotly.settings = F) })
  output$hospBox1 <- renderValueBox({ renderValueBoxHosp(input.settings, plotly.settings = F) })
  output$deathBox1 <- renderValueBox({ renderValueBoxDeaths(input.settings, plotly.settings = F) })
  
  output$casesBox2 <- renderValueBox({ renderValueBoxCases(input.settings, plotly.settings = T) })
  output$hospBox2 <- renderValueBox({ renderValueBoxHosp(input.settings, plotly.settings = T) })
  output$deathBox2 <- renderValueBox({ renderValueBoxDeaths(input.settings, plotly.settings = T) })
  
  output$dataTable <- renderDataTable({ 
    if(input.settings$normalize4 == 'raw')
      return(datatable(ohio.df, filter = "top", 
                       selection = "multiple", 
                       escape = FALSE, 
                       options = list(search = list(caseInsensitive = FALSE), dom = 'lrtip')))
    else if(input.settings$normalize4 == 'normalized')
      return(datatable(normalized.df, filter = "top", 
                       selection = "multiple", 
                       escape = FALSE, 
                       options = list(search = list(caseInsensitive = FALSE), dom = 'lrtip')))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input.settings$normalize4 == 'raw')
        paste("data-raw-", Sys.Date(), ".csv", sep="")
      else if(input.settings$normalize4 == 'normalized')
        paste("data-normalized-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      if(input.settings$normalize4 == 'raw')
        write.csv(ohio.df[input[["dataTable_rows_all"]], ], file, row.names = FALSE)
      else if(input.settings$normalize4 == 'normalized')
        write.csv(normalized.df[input[["dataTable_rows_all"]], ], file, row.names = FALSE)
    }
  )
}