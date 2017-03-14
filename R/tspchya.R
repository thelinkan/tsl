# 'Yearly change in percent
#' @export

tspchya <- function(series)
{
  
  if(is.ts(series)==FALSE)
  {
    stop("series has to be ts objects.")
  }
  if(frequency(series)!=12 && frequency(series)!=4)
  {
    stop("tspchya only supports monthly and quarterly data")
  }
  startper <- start(series)
  endper <- end(series)
  length <- tslength(series)
  freq <- frequency(series)
  if(length<=freq)
  {
    stop("tspchya needs more than one year in the ts series")
  } 
  temp <- (series[freq+1]-series[1])/series[1]*100
  year <- startper[1]+1
  period <- startper[2]
  SeriesA <- ts(temp,start=c(year,period),frequency=freq)
  for(a in 2:(length-freq))
  {
    period <- period+1
    if(period>freq)
    {
      period <- 1
      year <- year+1
    }
    temp <- (series[freq+a]-series[a])/series[a]*100
    SeriesA <- tsenter(SeriesA,year,period,temp)
  }
  SeriesA
}
