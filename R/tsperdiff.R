#' @export

tsperdiff <-function (per1,per2,ptyp)
{
  if(ptyp!="m" && ptyp!="q")
  {
    stop("tsperdiff only supports monthly and quarterly data right now")
  }
  if(ptyp=="m")
  {
    perdiff <- (per2[1]-per1[1])*12+per2[2]-per1[2]
  }
  else if(ptyp=="q")
  {
    perdiff <- (per2[1]-per1[1])*4+per2[2]-per1[2]
  }
  perdiff
}
