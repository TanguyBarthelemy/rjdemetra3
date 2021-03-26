#' @include utils.R regarima_rslts.R
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
regarima<-function(ts, spec="rg4", context=NULL){
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaOutput;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_output(jrslt))
  }
}

regarima_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/x13/r/RegArima", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(x13.RegArimaOutput, q)
  return (structure(list(
    result=p2r_regarima_rslts(p$result),
    estimation_spec=p2r_spec_regarima(p$estimation_spec),
    result_spec=p2r_spec_regarima(p$result_spec)
  ),
  class="JD3REGARIMA_OUTPUT")
  )

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
fast.regarima<-function(ts, spec="rg4", context=NULL){
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
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_rslts(jrslt))
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
x13<-function(ts, spec="rsa4", context=NULL){
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
x11<-function(ts, spec){
  jts<-ts_r2jd(ts)
  jspec<-r2jd_spec_x11(spec)
  jrslt<-.jcall("demetra/x13/r/X11", "Ldemetra/x13/r/X11$Results;", "process", jts, jspec)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x11_rslts(jrslt))
  }
}


