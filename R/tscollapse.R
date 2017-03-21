# Collapses a series from monthly to quarterly or yearly, or from quarterly to yearly.
# The following methods should be in it:
#   * Total
#   * Average
#   * Minimum
#   * Maximum
#   * First
#   * Last

#' @export

tscollapse <-function (series,ptyp,method)
{
  if(ptyp!="q" || frequency(series)!=12)
  {
    stop("tscollapse only supports collapse from monthly to quarterly data right now")
  }
  
  startper <- start(series)
  endper <- end(series)
  
  if(method=="Total" || method=="Average" || method=="Minimum" || method=="Maximum")
  {
    if(startper[2]%%3==2)
    {
      startper[2]=startper[2]+2
    } 
    if(startper[2]%%3==0)
    {
      startper[2]=startper[2]+1
    } 
    if(startper[2]>12)
    {
      startper[2]=startper[2]-12
      startper[1]=startper[1]+1
    }
    if(endper[2]%%3==1)
    {
      endper[2]=endper[2]-1
    } 
    if(endper[2]%%3==2)
    {
      endper[2]=endper[2]-2
    } 
    if(endper[2]<1)
    {
      endper[2]=endper[2]+12
      endper[1]=endper[1]-1
    }
    series <- window(series, startper, endper, extend=TRUE)
    if(method=="Total")
    {
      newseries <-  aggregate(series, nfrequency=4)
    }
    if(method=="Average")
    {
      newseries <-  aggregate(series, nfrequency=4,mean)
    }
    if(method=="Minimum" || method=="Maximum")
    {
      if(method=="Minimum")
        temp <- min(series[1],series[2],series[3])
      else 
        temp <- max(series[1],series[2],series[3])
      year <- startper[1]
      period <- (startper[2]+2)/3
      pd <- (tsperdiff(startper,endper,"m")-2)/3
      newseries <- ts(temp,start = c(year,period),frequency = 4)
      for(a in 1:pd)
      {
        period <- period+1
        if(period>4)
        {
          period <- 1
          year <- year+1
        }
        if(method=="Minimum")
          temp <- min(series[a*3+1],series[a*3+2],series[a*3+3])
        else
          temp <- max(series[1],series[2],series[3])
        newseries <- tsenter(newseries,year,period,temp)
        
      }
    }
  }
  else
  {
    stop("Method is not correct")
  }
  newseries
}
