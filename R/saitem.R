#' Title
#'
#' @param jsa
#'
#' @return
#' @export
#'
#' @examples
jsa.read<-function(jsa){
  jdef<-.jcall(jsa, "Ldemetra/sa/SaDefinition;", "getDefinition")

  jestimation<-.jcall(jsa, "Ldemetra/sa/SaEstimation;", "getEstimation")
  jrslt<-.jnull()
  if (!is.jnull(jestimation)){
    jrslt<-.jcall(jestimation, "Ldemetra/information/Explorable;", "getResults")
  }
  # ts
  jts<-.jcall(jdef, "Ldemetra/timeseries/Ts;", "getTs")
  rts<-.JD3_ENV$ts_jd2r(.jcall(jts, "Ldemetra/timeseries/TsData;", "getData"))

  jspec<-.jcall(jdef, "Ldemetra/sa/SaSpecification;", "activeSpecification")
  if (.jinstanceof(jspec, "demetra/tramoseats/TramoSeatsSpec")){
    spec<-.JD3_ENV$jd2r_spec_tramoseats(.jcast(jspec, "demetra/tramoseats/TramoSeatsSpec"))
    if (! is.jnull(jrslt)){
      rslt<-.JD3_ENV$tramoseats_rslts(.jcast(jrslt, "jdplus/tramoseats/TramoSeatsResults"))
    }
  }else if (.jinstanceof(jspec, "demetra/x13/X13Spec")){
    spec<-.JD3_ENV$jd2r_spec_x13(.jcast(jspec, "demetra/x13/X13Spec"))
    if (! is.jnull(jrslt)){
      rslt<-.JD3_ENV$x13_rslts(.jcast(jrslt, "jdplus/x13/X13Results"))
    }
  }

  return (list(
    ts=rts,
    spec=jspec,
    results=rslt
  ))
}

#' Title
#'
#' @param jsa
#' @param items
#'
#' @return
#' @export
#'
#' @examples
jsa.results<-function(jsa, items){
  jestimation<-.jcall(jsa, "Ldemetra/sa/SaEstimation;", "getEstimation")
  if (is.jnull(jestimation)) return (NULL)
  jrslt<-.jcall(jestimation, "Ldemetra/information/Explorable;", "getResults")
  if (is.null(items)) items<-.JD3_ENV$proc_dictionary2(jrslt)
  r<-lapply(items, function(t){.JD3_ENV$proc_data(jrslt, t)})
  names(r)<-items
  return (r)
}

#' Title
#'
#' @param jsa
#'
#' @return
#' @export
#'
#' @examples
jsa.name<-function(jsa){
  return (.jcall(jsa, "S", "getName"))
}

