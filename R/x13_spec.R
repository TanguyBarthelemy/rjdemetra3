#' @include utils.R
NULL


#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_regarima_default<-function(name){
  return (.jcall("demetra/regarima/RegArimaSpec", "Ldemetra/regarima/RegArimaSpec;", "fromString", name))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_x13_default<-function(name){
  return (.jcall("demetra/x13/X13Spec", "Ldemetra/x13/X13Spec;", "fromString", name))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_x11_default<-function(){
  return (.jfield("demetra/x11/X11Spec", "Ldemetra/x11/X11Spec;", "DEFAULT"))
}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
jd2r_spec_x11<-function(spec){
  mode<-.jcall(.jcall(spec, "Ldemetra/sa/DecompositionMode;", "getMode"), "S", "name")
  seasonal<-.jcall(spec, "Z", "isSeasonal")
  filters<-jd2r_enumlist(.jcall(spec, "Ljava/util/List;", "getFilters"))
  sigma.lower<-.jcall(spec, "D", "getLowerSigma")
  sigma.upper<-.jcall(spec, "D", "getUpperSigma")
  henderson<-.jcall(spec, "I", "getHendersonFilterLength")
  nforecasts<-.jcall(spec, "I", "getForecastHorizon")
  nbackcasts<-.jcall(spec, "I", "getBackcastHorizon")
  calendarsigma<-.jcall(.jcall(spec, "Ldemetra/x11/CalendarSigmaOption;", "getCalendarSigma"), "S", "name")
  sigmavec<-jd2r_enumlist(.jcall(spec, "Ljava/util/List;", "getSigmaVec"))
  excludeforecast<-.jcall(spec, "Z", "isExcludeForecast")
  bias<-.jcall(.jcall(spec, "Ldemetra/x11/BiasCorrection;", "getBias"), "S", "name")
  return (list(mode=mode, seasonal=seasonal, filters=filters, sigma.lower=sigma.lower, sigma.upper=sigma.upper,
               henderson=henderson, calendarsigma=calendarsigma, sigmavec=sigmavec, excludeforecast=excludeforecast,
               nforecasts=nforecasts, nbackcasts=nbackcasts, bias=bias))
}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
r2jd_spec_x11<-function(spec){
  jbuilder<-.jcall("demetra/x11/X11Spec", "Ldemetra/x11/X11Spec$Builder;", "builder")
  jmode<-.jcall("demetra/sa/DecompositionMode", "Ldemetra/sa/DecompositionMode;", "valueOf", spec$mode)
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "mode", jmode)
  for (i in 1:length(spec$filters)){
    jfilter<-.jcall("demetra/x11/SeasonalFilterOption", "Ldemetra/x11/SeasonalFilterOption;", "valueOf", spec$filters[i])
    jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "filter", jfilter)
  }
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "seasonal", as.logical(spec$seasonal))
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "lowerSigma", spec$sigma.lower)
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "upperSigma", spec$sigma.upper)
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "hendersonFilterLength", as.integer(spec$henderson))
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "forecastHorizon", as.integer(spec$nforecasts))
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "backcastHorizon", as.integer(spec$nbackcasts))
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "excludeForecast", as.logical(spec$excludeforecast))
  jcal<-.jcall("demetra/x11/CalendarSigmaOption", "Ldemetra/x11/CalendarSigmaOption;", "valueOf", spec$calendarsigma)
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "calendarSigma", jcal)
  if (! is.null(spec$sigmavec)){
    for (i in 1:length(spec$sigmavec)){
      jfilter<-.jcall("demetra/x11/SigmaVecOption", "Ldemetra/x11/SigmaVecOption;", "valueOf", spec$sigmavec[i])
      jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "sigmaVec", jfilter)
    }
  }
  jcal<-.jcall("demetra/x11/BiasCorrection", "Ldemetra/x11/BiasCorrection;", "valueOf", spec$bias)
  jbuilder<-.jcall(jbuilder, "Ldemetra/x11/X11Spec$Builder;", "bias", jcal)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/x11/X11Spec"))

}

