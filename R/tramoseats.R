#' @include utils.R regarima_rslts.R tramoseats_spec.R
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
tramo<-function(ts, spec="trfull", context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoOutput;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (tramo_output(jrslt))
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
fast.tramo<-function(ts, spec="trfull", context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramoseats/r/Tramo$Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramoseats/r/Tramo$Results;", "process", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_rslts(jrslt))
  }
}


tramo_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoOutput, q)
  return (structure(list(
    result=p2r_regarima_rslts(p$result),
    estimation_spec=p2r_spec_tramo(p$estimation_spec),
    result_spec=p2r_spec_tramo(p$result_spec)
  ),
  class="JD3TRAMO_OUTPUT")
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
tramoseats<-function(ts, spec="rsafull", context=NULL){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/io/protobuf/TramoSeatsOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/io/protobuf/TramoSeatsOutput;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (tramoseats_output(jrslt))
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
tramo.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  jspec<-r2jd_spec_tramo(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec;", "fromString", "trfull")

  }else{
    jrefspec<-r2jd_spec_tramo(refspec)
  }
  jdom<-jdomain(period, start, end)
  jnspec<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_tramo(jnspec))
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
tramoseats.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  jspec<-r2jd_spec_tramoseats(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec;", "fromString", "rsafull")

  }else{
    jrefspec<-r2jd_spec_tramoseats(refspec)
  }
  jdom<-jdomain(period, start, end)
  jnspec<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/TramoSeatsSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_tramoseats(jnspec))

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
fast.tramoseats<-function(ts, spec="rsafull", context=NULL){
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
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (tramoseats_rslts(jrslt))
  }
}

tramoseats_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoSeatsOutput, q)
  return (structure(list(
    result=p2r_tramoseats_rslts(p$result),
    estimation_spec=p2r_spec_tramoseats(p$estimation_spec),
    result_spec=p2r_spec_tramoseats(p$result_spec)
  ),
  class="JD3TRAMOSEATS_OUTPUT")
  )

}


terror_names<-c("actual", "forecast", "error", "rel. error", "raw", "fraw", "efraw")
forecast_names<-c("forecast", "error", "fraw", "efraw")

#' Title
#'
#' @param ts
#' @param spec
#' @param nback
#'
#' @return
#' @export
#'
#' @examples
terror<-function(ts, spec, nback=1){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/MatrixType;", "process", jts, spec, as.integer(nback))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/MatrixType;", "process", jts, jspec, jcontext, as.integer(nback))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-matrix_jd2r(jrslt)
    colnames(rslt)<-terror_names
    return (rslt)
  }
}

#' Title
#'
#' @param ts
#' @param spec
#' @param nf
#'
#' @return
#' @export
#'
#' @examples
forecast<-function(ts, spec="trfull", nf=-1){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (nf<0) nf<-frequency(ts)*(-nf)

  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/MatrixType;", "forecast", jts, spec, as.integer(nf))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/MatrixType;", "forecast", jts, jspec, jcontext, as.integer(nf))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-matrix_jd2r(jrslt)
    colnames(rslt)<-forecast_names
    return (rslt)
  }
}
