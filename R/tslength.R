#' Returns the length of a ts object
#'
#' @param series A ts object
#' @return The number of periods in the ts object.
#' @export

tslength <- function(series)
{
  startper <- start(series)
  endper <- end(series)
  periods=(12-startper[2]+1)+(endper[1]-startper[1]-1)*12+endper[2]
}
