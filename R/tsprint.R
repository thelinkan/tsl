#' @export

tsprint <- function(series)
{
  startper <- start(series)
  endper <- end(series)
  length <- tslength(series)
  freqp <- frequency(series)  

  year <- startper[1]
  period <- startper[2]
  for (i in 1:a)
  {
    cat(year,":",period,"  ",series[i],"\n",sep="")
    period <- period+1
    if(period>freqp)
    {
      period <- 1
      year <- year+1
    }
  }
}  
