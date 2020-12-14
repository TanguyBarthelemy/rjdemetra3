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

  d7<-proc_ts(jrslts, "d7")
  d10<-proc_ts(jrslts, "d10")
  d11<-proc_ts(jrslts, "d11")
  d12<-proc_ts(jrslts, "d12")
  d13<-proc_ts(jrslts, "d13")

  return (list(d7=d7, d10=d10, d11=d11, d12=d12, d13=d13))
}
