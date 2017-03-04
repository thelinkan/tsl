tsenter <- function(series,year,month,value)
{
  startper <- start(series)
  endper <- end(series)
  
  if(startper[1]==year)
  {
    periods <-startper[2]-month+1
  }
  else if(startper[1]+1==year)
  {
    periods <- 12-startper[2]+1+month
  }
  else
  {
    periods <- 12-startper[2]+1+month+(year-startper[1]-1)*12
  }
  
  if(endper[1]<year || (endper[1]==year && endper[2]<month))
  {
    series <- window(series, start(series), c(year, month), extend=TRUE)
  }
    
  series <- replace (series,periods,value)
  
}
