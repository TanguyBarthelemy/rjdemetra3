#' @include utils.R jd3_rslts.R
NULL


#' Title
#'
#' @param jrslts
#'
#' @return
#' @export
#'
#' @examples
x11_rslts<-function(jrslts){
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(x13.X11Results, q)
  return (rq)

}
