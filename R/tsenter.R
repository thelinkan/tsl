#' Inserts a number to a series
#'
#' @param series A ts object
#' @param year The year for the observation
#' @param month The month of the observation
#' @param value The value to insert on year month in the ts object
#' @return The new ts object.
#' @examples
#' temp <- c(1:14)
#' Series1 <- ts(temp,start=c(2016,1),frequency = 12)
#' Series1 <- tsenter(series=Series1,year=2017,period=3,value=47)
#' @export

tsenter <- function(series,year,period,value)
{
  if(is.ts(series)==FALSE)
  {
    stop("series has to be ts objects.")
  }
  if(frequency(series)!=12 && frequency(series)!=4 && frequency(series)!=2 && frequency(series)!=1)
  {
    stop("tsenter only supports monthly and quarterly data right now")
  }
  if(period>frequency(series) || period<1)
  {
    stop("period has to be between 1 and 12 for monthly data, and between 1 and 4 for quarterly data")
  }

  startper <- start(series)
  endper <- end(series)

  if(frequency(series)==12)
  {
    time <- c(year,period)
    periods <- tsperdiff(startper,time,"M")+1
    if(periods>0)
    {
      if(endper[1]<year || (endper[1]==year && endper[2]<period))
      {
        series <- window(series, start(series), c(year, period), extend=TRUE)
      }
    series <- replace (series,periods,value)
    }else{
      print("period före startperiod")
    }
  }
  else if(frequency(series)==4)
  {
    time <- c(year,period)
    periods <- tsperdiff(startper,time,"Q")+1
    if(periods>0)
    {
      if(endper[1]<year || (endper[1]==year && endper[2]<period))
      {
        series <- window(series, start(series), c(year, period), extend=TRUE)
      }
      series <- replace (series,periods,value)
    }else{
      print("period före startperiod")
    }
  }
  else if(frequency(series)==2)
  {
    time <- c(year,period)
    periods <- tsperdiff(startper,time,"S")+1
    if(periods>0)
    {
      if(endper[1]<year || (endper[1]==year && endper[2]<period))
      {
        series <- window(series, start(series), c(year, period), extend=TRUE)
      }
      series <- replace (series,periods,value)
    }else{
      print("period före startperiod")
    }
  }
  else if(frequency(series)==1)
  {
    time <- c(year,period)
    periods <- tsperdiff(startper,time,"A")+1
    if(periods>0)
    {
      if(endper[1]<year || (endper[1]==year && endper[2]<period))
      {
        series <- window(series, start(series), c(year, period), extend=TRUE)
      }
      series <- replace (series,periods,value)
    }else{
      print("period före startperiod")
    }
  }
  series
}
