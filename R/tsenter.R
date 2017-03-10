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
#' Series1 <- tsenter(series=Series1,year=2017,month=3,value=47)
#' @export

tsenter <- function(series,year,month,value)
{
  if(is.ts(series)==FALSE)
  {
    stop("series has to be ts objects.")
  }
  if(frequency(series)!=12)
  {
    stop("tsenter only supports monthly data right now")
  }

  startper <- start(series)
  endper <- end(series)
  
  time <- c(year,month)
  periods <- tsperdiff(startper,time)+1

  if(periods>0)
  {
    if(endper[1]<year || (endper[1]==year && endper[2]<month))
    {
      series <- window(series, start(series), c(year, month), extend=TRUE)
    }
  series <- replace (series,periods,value)
  }else{
    print("period före startperiod")
  }  
}
