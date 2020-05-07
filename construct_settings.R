constructSettings <- function(these.data, plotly.settings) {
  if(plotly.settings) {
    these.data <- list(
      counties = these.data$counties2,
      highlights = these.data$highlights2,
      series = these.data$series2,
      transformation = these.data$transformation2,
      ages = these.data$ages2,
      sexes = these.data$sexes2,
      num_align = these.data$num_align2,
      exponentials = these.data$exponentials2,
      normalize = these.data$normalize2,
      smooth = these.data$smooth2,
      pushtime = these.data$pushtime2
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
      num_align = these.data$num_align1,
      exponentials = these.data$exponentials1,
      normalize = these.data$normalize1,
      smooth = these.data$smooth1,
      pushtime = these.data$pushtime1
    )
  }
  return(these.data)
}