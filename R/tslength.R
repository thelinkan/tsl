#' Returns the length of a ts object
#'
#' @param series A ts object
#' @return The number of periods in the ts object.
#' @export

tslength <- function(series)
{
  if(is.ts(series)==FALSE)
  {
    stop("series has to be ts objects.")
  }
  if(frequency(series)!=12 && frequency(series)!=4)
  {
    stop("tslength only supports monthly and quarterly data right now")
  }
  startper <- start(series)
  endper <- end(series)
  freq <- frequency(series)
  periods=(freq-startper[2]+1)+(endper[1]-startper[1]-1)*freq+endper[2]
}
