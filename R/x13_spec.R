#' @include utils.R
NULL

jd2r_regarima_basic<-function(spec){
  jbasic<-.jcall(spec, "Ldemetra/regarima/BasicSpec;", "getBasic")
  jspan<-.jcall(jbasic, "Ldemetra/timeseries/TimeSelector;", "getSpan")
  span<-jd2r_span(jspan)
  preprocessing<-.jcall(jbasic, "Z", "isPreprocessing")
  preliminarycheck<-.jcall(jbasic, "Z", "isPreliminaryCheck")
  return ((list(preprocessing=preprocessing, preliminarycheck=preliminarycheck, span=span)))
}

jd2r_regarima_transform<-function(spec){
  jtransform<-.jcall(spec, "Ldemetra/regarima/TransformSpec;", "getTransform")
  aicdiff<-.jcall(jtransform, "D", "getAicDiff")
  fn<-.jcall(.jcall(jtransform, "Ldemetra/modelling/TransformationType;", "getFunction"), "S", "name")
  adjust<-.jcall(.jcall(jtransform, "Ldemetra/timeseries/calendars/LengthOfPeriodType;", "getAdjust"), "S", "name")
  return ((list(fn=fn, aicdiff=aicdiff, adjust=adjust)))
}


r2jd_regarima_basic<-function(spec){
  jbuilder<-.jcall("demetra/regarima/BasicSpec", "Ldemetra/regarima/BasicSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/regarima/BasicSpec$Builder;", "preprocessing", spec$preprocessing)
  jbuilder<-.jcall(jbuilder, "Ldemetra/regarima/BasicSpec$Builder;", "preliminaryCheck", spec$preliminarycheck)
  jbuilder<-.jcall(jbuilder, "Ldemetra/regarima/BasicSpec$Builder;", "span", r2jd_span(spec$span))
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/regarima/BasicSpec"))
}


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

stresstest<-function(){
for (i in 1:1000){
  spec<-spec_tramo_default("trfull")
  a<-jd2r_spec_tramo(spec)
#  nspec<-r2jd_spec_tramo(a)
}
}
