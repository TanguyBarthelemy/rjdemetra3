#' @include utils.R
NULL


#' @export
#' @rdname jd3_utilities
jd2p_context<-function(jcntx){
  q<-.jcall("demetra/util/r/Modelling", "[B", "toBuffer", jcntx)
  rq<-RProtoBuf::read(jd3.ModellingContext, q)
  return (rq)
}

