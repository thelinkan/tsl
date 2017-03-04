\name{tsenter}
\title{Time Series Enter}
\description{Enters value to correct period in time series}
\usage{
tsenter(series,year,month,value)
}
\arguments{
\item{series}{ a ts object. }
\item{year}{ year to edit. }
\item{month}{ month to edit. }
\item{value}{ value to insert for month and year. If month and year does not exist, extend time series. }
}
\author{Torbjörn Lindquist}
\examples{
Series1 <- tsenter(series=Series1,year=2017,month=3,value=47)
}
\keyword{time-series}
