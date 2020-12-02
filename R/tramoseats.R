#' @include utils.R jd3_rslts.R
NULL


#' Title
#'
#' @param ts
#' @param spec
#' @param context
#'
#' @return
#' @export
#'
#' @examples
tramo<-function(ts, spec, context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramoseats/r/Tramo$Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramoseats/r/Tramo$Results;", "process", jts, jspec, jcontext )
  }
  return (jrslt)
}

#' Title
#'
#' @param ts Time series
#' @param spec Specification list
#' @param context Optional. context for the processing (user-defined variables and calendars)
#'
#' @return
#' @export
#'
#' @examples
tramoseats<-function(ts, spec, context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/r/TramoSeats$Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/r/TramoSeats$Results;", "process", jts, jspec, jcontext)
  }
}
