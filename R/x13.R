#' @include utils.R jd3_rslts.R regarima_rslts.R
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
regarima_raw<-function(ts, spec, context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ldemetra/x13/r/RegArima$Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ldemetra/x13/r/RegArima$Results;", "process", jts, jspec, jcontext )
  }
}

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
regarima<-function(ts, spec, context=NULL){
  jrtslt<-regarima_raw(ts, spec, context)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_rslts(jrslt))
  }
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
x13_raw<-function(ts, spec, context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/r/X13$Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/r/X13$Results;", "process", jts, jspec, jcontext)
  }
  return (jrslt)
}

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
x13<-function(ts, spec, context=NULL){
  jrtslt<-x13_raw(ts, spec, context)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x13_rslts(jrslt))
  }
}

#' Title
#'
#' @param ts
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
x11_raw<-function(ts, spec){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  jrslt<-.jcall("demetra/x13/r/X11", "Ldemetra/x13/r/X11$Results;", "process", jts, spec)
  return (jrslt)
}

#' Title
#'
#' @param ts
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
x11<-function(ts, spec){
  jrtslt<-x11_raw(ts, spec)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x11_rslts(jrslt))
  }
}

