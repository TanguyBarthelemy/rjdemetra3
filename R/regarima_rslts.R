#' @include utils.R jd3_rslts.R
NULL

regarima_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(regarima.RegArimaEstimation, q)
  return (rq)
}
