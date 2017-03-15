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
  if(is.ts(series1)==FALSE || is.ts(series2)==FALSE)
  {
    stop("series1 and series2 has to be ts objects.")
  }
  if(!((frequency(series1)==12 && frequency(series2)==12) || (frequency(series1)==4 && frequency(series2)==4)))
  {
    stop("tsadd only supports monthly and quarterly data right now")
  }
  if(frequency(series1)!=frequency(series2))
  {
    stop("both series1 and series2 has to be the same frequency")
  }
  startper1 <- start(series1)
  endper1 <- end(series1)
  length1 <- tslength(series1)
  freqp <- frequency(series1)  
  if(freqp==4)
    freq="q"
  else
    freq="m"

  startper2 <- start(series2)
  endper2 <- end(series2)
  length2 <- tslength(series2)
  
  minlength=min(length1,length2)
  maxlength=max(length1,length2)

  if(startper1[1]==startper2[1] && startper1[2]==startper2[2])
  {
    temp <- c(Series1[1]+Series2[1])
    year <- startper1[1]
    month <- startper1[2]
    SeriesA <- ts(temp,start=c(startper1[1],startper1[2]),frequency=freq)
    for(a in 2:minlength)
    {
      month <- month+1
      if(month>freq)
      {
        month <- 1
        year <- year+1
      }
      temp <- c(Series1[a]+Series2[a])
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
    if(maxlength>minlength)
    {
      for(a in minlength+1:maxlength)
      {
        month <- month+1
        if(month>freq)
        {
          month <- 1
          year <- year+1
        }
        SeriesA <- tsenter(SeriesA,year,month,"na")  
      }
    }
  }
  else if (startper1[1]==startper2[1] && startper1[2]>startper2[2] || (startper1[1]>startper2[1]))
  {
    pd <- tsperdiff(startper2,startper1,freq)
    temp <- c("na")
    year <- startper2[1]
    month <- startper2[2]
    SeriesA <- ts(temp,start=c(startper2[1],startper2[2]),frequency=12)
    if(pd>1)
    {
      for(a in 2:pd)
      {
        month <- month+1
        if(month>12)
        {
          month <- 1
          year <- year+1
        }
        temp <- c("na")
        SeriesA <- tsenter(SeriesA,year,month,temp)
      }
    }
    sp=min(tsperdiff(startper1,endper1,freq),tsperdiff(startper1,endper2,freq))+1
    ep=min(tsperdiff(startper1,endper1,freq),tsperdiff(startper1,endper2,freq))+1
    for(a in 1:sp)
    {
      month <- month+1
      if(month>12)
      {
        month <- 1
        year <- year+1
      }
      temp <- c(series1[a]+series2[a+pd])
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
    for(a in sp+1:ep)
    {
      month <- month+1
      if(month>12)
      {
        month <- 1
        year <- year+1
      }
      temp <- c("na")
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
  }
  else
  {
    pd <- tsperdiff(startper1,startper2,freq)
    temp <- c("na")
    year <- startper1[1]
    month <- startper1[2]
    SeriesA <- ts(temp,start=c(startper1[1],startper1[2]),frequency=12)
    if(pd>1)
    {
      for(a in 2:pd)
      {
        month <- month+1
        if(month>12)
        {
          month <- 1
          year <- year+1
        }
        temp <- c("na")
        SeriesA <- tsenter(SeriesA,year,month,temp)
      }
    }
    sp=min(tsperdiff(startper2,endper2,freq),tsperdiff(startper2,endper1,freq))+1
    ep=min(tsperdiff(startper2,endper2,freq),tsperdiff(startper2,endper1,freq))+1
    for(a in 1:sp)
    {
      month <- month+1
      if(month>12)
      {
        month <- 1
        year <- year+1
      }
      temp <- c(series2[a]+series1[a+pd])
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
    for(a in sp+1:ep)
    {
      month <- month+1
      if(month>12)
      {
        month <- 1
        year <- year+1
      }
      temp <- c("na")
      SeriesA <- tsenter(SeriesA,year,month,temp)
    }
  }
  SeriesA
}
