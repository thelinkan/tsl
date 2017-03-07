#' Adds two TS objects together
#'
#' @param series1 A ts object
#' @param series2 A ts object
#' @return The new ts object.
#' @examples
#' temp <- c(1:14)
#' Series1 <- ts(temp,start=c(2016,1),frequency = 12)
#' Series2 <- ts(temp,start=c(2016,1),frequency = 12)
#' SeriesA <- tsadd(series1=Series1,series2=Series2)
#' @export

tsadd <- function(series1,series2)
{
  startper1 <- start(series1)
  endper1 <- end(series1)
  length1 <- tslength(series1)
  
  startper2 <- start(series2)
  endper2 <- end(series2)
  length2 <- tslength(series2)
  
  minlength=min(length1,length2)
  

  if(startper1[1]==startper2[1] && startper1[2]==startper2[2])
  {
    temp <- c(Series1[1]+Series2[1])
    year <- startper1[1]
    month <- startper1[2]
    SeriesA <- ts(temp,start=c(startper1[1],startper1[2]),frequency=12)
    for(a in 2:minlength)
    {
      month <- month+1
      if(month>12)
      {
        month <- 1
        year <- year+1
      }
      temp <- c(Series1[a]+Series2[a])
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
  }
  SeriesA
}
