#' @include utils.R
NULL


#' Java Utility Functions
#'
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#'
#' @param jcntx parameters.
#'
#' @name jd3_utilities
NULL
#> NULL
#>
#' @export
#' @rdname jd3_utilities
jd2p_context<-function(jcntx){
  q<-.jcall("demetra/util/r/Modelling", "[B", "toBuffer", jcntx)
  rq<-RProtoBuf::read(jd3.ModellingContext, q)
  return (rq)
}

