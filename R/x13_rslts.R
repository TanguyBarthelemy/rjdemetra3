#' @include utils.R jd3_rslts.R
NULL

X11_RSLTS<-c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b13", "b17", "b20",
                 "c1", "c2", "c4", "c5", "c6", "c7", "c9", "c10", "c11", "c13", "c17", "c20",
                 "d1", "d2", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13")

#' Title
#'
#' @param jrslts
#'
#' @return
#' @export
#'
#' @examples
x11_rslts<-function(jrslts){

  all<-proc_matrix(jrslts, "all")
  colnames(all)<-X11_RSLTS
  return (list(all=all))
}
