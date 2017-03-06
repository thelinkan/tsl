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

  startper2 <- start(series2)
  endper2 <- end(series2)

  if(startper1[1]==startper2[1] && startper1[1]==startper2[1])
  {
    
  }
}
