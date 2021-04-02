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
    jspec<-r2jd_spec_regarima(spec)
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
    jspec<-r2jd_spec_tramo(spec)
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
x13<-function(ts, spec="rsa4", context=NULL){
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/io/protobuf/X13Output;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/io/protobuf/X13Output;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x13_output(jrslt))
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
fast.x13<-function(ts, spec="rsa4", context=NULL){
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

x13_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/x13/r/X13", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(x13.X13Output, q)
  return (structure(list(
    result=p2r_x13_rslts(p$result),
    estimation_spec=p2r_spec_x13(p$estimation_spec),
    result_spec=p2r_spec_x13(p$result_spec)
  ),
  class="JD3X13_OUTPUT")
  )

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

#' Title
#'
#' @param spec
#' @param refspec
#' @param policy
#' @param period
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
regarima.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  jspec<-r2jd_spec_regarima(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/regarima/RegArimaSpec", "Ldemetra/regarima/RegArimaSpec;", "fromString", "rg4")

  }else{
    jrefspec<-r2jd_spec_regarima(refspec)
  }
  jdom<-jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_regarima(jnspec))
}

#' Title
#'
#' @param spec
#' @param refspec
#' @param policy
#' @param period
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
x13.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  jspec<-r2jd_spec_x13(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/x13/X13Spec", "Ldemetra/x13/X13Spec;", "fromString", "rsa4")

  }else{
    jrefspec<-r2jd_spec_x13(refspec)
  }
  jdom<-jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_x13(jnspec))

}

