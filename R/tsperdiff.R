#' @export

tsperdiff <-function (per1,per2,ptyp)
{
  if(ptyp!="M" && ptyp!="Q" && ptyp!="S" && ptyp!="A")
  {
    stop("tsperdiff only supports monthly, quarterly, semi annual and annual data right now")
  }
  if(ptyp=="M")
  {
    perdiff <- (per2[1]-per1[1])*12+per2[2]-per1[2]
  }
  if(ptyp=="Q")
  {
    perdiff <- (per2[1]-per1[1])*4+per2[2]-per1[2]
  }
  if(ptyp=="S")
  {
    perdiff <- (per2[1]-per1[1])*2+per2[2]-per1[2]
  }
  if(ptyp=="A")
  {
    perdiff <- (per2[1]-per1[1])
  }
  perdiff
}
