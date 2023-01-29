#' @include utils.R
NULL

#' Read SAItem
#'
#' `jsa.results()` extracts specific variables of the model of the SAItem while
#' `jsa.read()` extracts all the informations of a SAItem (see details).
#'
#' @param jsa Java SAItem object.
#' @param items vector of characters containing the variables to extract.
#' See [rjd3x13::userdefined_variables_x13()] or [rjd3tramoseats::userdefined_variables_tramoseats()]. By default, extracts all the possible variables.
#'
#' @details A SAItem contains more information than just the results of a model.
#' All those informations are extracted with the `jsa.read()` function that returns a list with 5 objects:
#'
#' - `ts`: the raw time series.
#' - `domainSpec`: initial specification. Reference for any relaxing of some elements of the specification.
#' - `estimationSpec`: specification used for the current estimation.
#' - `pointSpec`: specification corresponding to the results of the current estimation (fully identified model).
#' - `results`: the result of the model.
#' @export
.jsa.read<-function(jsa){
  jdef<-.jcall(jsa, "Ldemetra/sa/SaDefinition;", "getDefinition")

  jestimation<-.jcall(jsa, "Ldemetra/sa/SaEstimation;", "getEstimation")
  jrslt<-.jnull()
  if (!is.jnull(jestimation)){
    jrslt<-.jcall(jestimation, "Ldemetra/information/Explorable;", "getResults")
  }
  # ts
  jts<-.jcall(jdef, "Ldemetra/timeseries/Ts;", "getTs")
  rts<-rjd3toolkit::.jd2r_ts(.jcall(jts, "Ldemetra/timeseries/TsData;", "getData"))

  jdspec<-.jcall(jdef, "Ldemetra/sa/SaSpecification;", "getDomainSpec")
  jspec<-.jcall(jdef, "Ldemetra/sa/SaSpecification;", "activeSpecification")
  if (.jinstanceof(jspec, "demetra/tramoseats/TramoSeatsSpec")){
    spec<-rjd3tramoseats::jd2r_spec_tramoseats(.jcast(jspec, "demetra/tramoseats/TramoSeatsSpec"))
    dspec<-rjd3tramoseats::jd2r_spec_tramoseats(.jcast(jdspec, "demetra/tramoseats/TramoSeatsSpec"))
    if (! is.jnull(jrslt)){
      rslt<-rjd3tramoseats::.tramoseats_rslts(.jcast(jrslt, "jdplus/tramoseats/TramoSeatsResults"))
      jpspec<-.jcall(jestimation, "Ldemetra/sa/SaSpecification;", "getPointSpec")
      pspec<-rjd3tramoseats::jd2r_spec_tramoseats(.jcast(jpspec, "demetra/tramoseats/TramoSeatsSpec"))
    }
  }else if (.jinstanceof(jspec, "demetra/x13/X13Spec")){
    spec<-rjd3x13::.jd2r_spec_x13(.jcast(jspec, "demetra/x13/X13Spec"))
    dspec<-rjd3x13::.jd2r_spec_x13(.jcast(jdspec, "demetra/x13/X13Spec"))
    if (! is.jnull(jrslt)){
      rslt<-rjd3x13::.x13_rslts(.jcast(jrslt, "jdplus/x13/X13Results"))
      jpspec<-.jcall(jestimation, "Ldemetra/sa/SaSpecification;", "getPointSpec")
      pspec<-rjd3x13::.jd2r_spec_x13(.jcast(jpspec, "demetra/x13/X13Spec"))
    }
  }else{
    rslt<-NULL
    pspec<-NULL
  }

  return (list(
    ts=rts,
    domainSpec=dspec,
    estimationSpec=spec,
    pointSpec=pspec,
    results=rslt
  ))
}

#' @rdname jsa.read
#' @export
.jsa.results<-function(jsa, items = NULL){
  jestimation<-.jcall(jsa, "Ldemetra/sa/SaEstimation;", "getEstimation")
  if (is.jnull(jestimation))
    return (NULL)
  jrslt<-.jcall(jestimation, "Ldemetra/information/Explorable;", "getResults")
  if (is.null(items))
    items<-rjd3toolkit::.proc_dictionary2(jrslt)
  r<-lapply(items, function(t){rjd3toolkit::.proc_data(jrslt, t)})
  names(r)<-items
  return (r)
}


#' @rdname jmp.name
#' @export
.jsa.name<-function(jsa){
  return (.jcall(jsa, "S", "getName"))
}

