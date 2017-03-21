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
  
  if(method=="Total" || method=="Average")
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
    print(series)
    series <- window(series, startper, endper, extend=TRUE)
    print(series)
    if(method=="Total")
    {
      newseries <-  aggregate(series, nfrequency=4)
    }
    if(method=="Average")
    {
      newseries <-  aggregate(series, nfrequency=4,mean)
    }
  }
  else
  {
    stop("Method is not correct")
  }
  newseries
}
