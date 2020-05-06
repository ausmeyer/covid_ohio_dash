build.plots <- function(input.settings, output) {
  
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
           "No need to highlight all of the selected counties. Unselect highlights.")
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
           "No need to highlight all of the selected counties. Unselect highlights.")
      
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
    this.validate3(input.settings)
    girafe(ggobj = renderMap(input.settings),
           width_svg = 20,
           height_svg = 20 * 5 / 7,
           options = list(opts_selection(type = "single", only_shiny = FALSE)))
  })
  
}