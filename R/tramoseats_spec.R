#' @include utils.R
NULL

jd2r_spec_tramo_outlier<-function(spec){
  joutlier<-spec$getOutliers()
  jspan <-joutlier$getSpan()

  enabled<-joutlier$isUsed()
  span<-jd2r_span(jspan)
  ao <-.jcall(joutlier, "Z", "isAo")
  ls <-.jcall(joutlier, "Z", "isLs")
  tc <-.jcall(joutlier, "Z", "isTc")
  so <-.jcall(joutlier, "Z", "isSo")
  cv <-.jcall(joutlier, "D", "getCriticalValue")
  eml <-.jcall(joutlier, "Z", "isMaximumLikelihood")
  tcrate <-.jcall(joutlier, "D", "getDeltaTC")
  return (list(enabled=enabled,span=span,ao=ao,ls=ls,tc=tc,so=so,cv=cv,eml=eml,tcrate=tcrate))
}

jd2r_spec_tramo_estimate<-function(spec){
  jestimate<-.jcall(spec, "Ldemetra/tramo/EstimateSpec;", "getEstimate")
  jspan <- .jcall(jestimate, "Ldemetra/timeseries/TimeSelector;", "getSpan")
  span<-jd2r_span(jspan)
  tol <-.jcall(jestimate, "D", "getTol")
  eml <-.jcall(jestimate, "Z", "isMaximumLikelihood")
  urfinal <-.jcall(jestimate, "D", "getUbp")
  return (list(span=span,tol=tol,eml=eml,urfinal=urfinal))
}

jd2r_spec_tramo_automdl<-function(spec){
  jami<-.jcall(spec, "Ldemetra/tramo/AutoModelSpec;", "getAutoModel")
  enabled <-.jcall(jami, "Z", "isEnabled")
  acceptdefault <-.jcall(jami, "Z", "isAcceptDefault")
  cancel <-.jcall(jami, "D", "getCancel")
  ub1 <-.jcall(jami, "D", "getUb1")
  ub2 <-.jcall(jami, "D", "getUb2")
  armalimit <-.jcall(jami, "D", "getTsig")
  reducecv <-.jcall(jami, "D", "getPc")
  ljungboxlimit <-.jcall(jami, "D", "getPcr")
  compare <-.jcall(jami, "Z", "isAmiCompare")
  return (list(enabled=enabled,acceptdefault=acceptdefault,cancel=cancel,ub1=ub1,ub2=ub2,
             armalimit=armalimit,reducecv=reducecv,ljungboxlimit=ljungboxlimit,compare=compare))
}

jd2r_spec_tramo_easter<-function(jeaster){
  type <- .jcall(.jcall(jeaster, "Ldemetra/tramo/EasterSpec$Type;","getType"), "S", "name")
  julian <-.jcall(jeaster, "Z", "isJulian")
  duration <-.jcall(jeaster, "I", "getDuration")
  test <-.jcall(jeaster, "Z", "isTest")
  return (list(type=type,julian=julian,duration=duration,test=test))
}

jd2r_spec_tramo_td<-function(jtd){
  option<-"None"
  stocktd<-0
  holidays<-NULL
  userdefined=NULL
  leapyear<-"None"
  type<-"None"
  test <-.jcall(.jcall(jtd, "Ldemetra/tramo/RegressionTestType;", "getRegressionTestType"), "S", "name")
  mauto <-.jcall(.jcall(jtd, "Ldemetra/tramo/TradingDaysSpec$AutoMethod;", "getAutomaticMethod"), "S", "name")
  pftd <-.jcall(jtd, "D", "getProbabilityForFTest")
  if (.jcall(jtd, "Z", "isUsed")){
    if (.jcall(jtd, "Z", "isUserDefined")){
      option <- "UserDefined"
      userdefined<-.jcall(jtd, "[S", "getUserVariables")
    }else if (.jcall(jtd, "Z", "isStockTradingDays")){
      option <- "Stock"
      stocktd<-.jcall(jtd, "I", "getStockTradingDays")
    }else{
      if (.jcall(jtd, "Z", "isHolidays")){
        option <- "Holidays"
        .jcall(jtd, "S", "getHolidays")
      }else{
        option <- "Default"
      }
      leapyear<-.jcall(.jcall(jtd, "Ldemetra/timeseries/calendars/LengthOfPeriodType;", "getLengthOfPeriodType"), "S", "name")
      type<-.jcall(.jcall(jtd, "Ldemetra/timeseries/calendars/TradingDaysType;", "getTradingDaysType"), "S", "name")
    }
  }
  return (list(option=option,stocktd=stocktd,holydays=holidays,userdefined=userdefined,mauto=mauto,pftd =pftd,leapyear=leapyear,type=type,test=test))
}


jd2r_spec_tramo_regression<-function(spec){
  jregression<-.jcall(spec, "Ldemetra/tramo/RegressionSpec;", "getRegression")
  mu <-.jcall(jregression, "Z", "isMean")

  #Calendar
  jcal<-.jcall(jregression, "Ldemetra/tramo/CalendarSpec;", "getCalendar")
  jtd<-.jcall(jcal, "Ldemetra/tramo/TradingDaysSpec;", "getTradingDays")
  jeaster<-.jcall(jcal, "Ldemetra/tramo/EasterSpec;", "getEaster")

  return (list(
      mean=mu,
      tradingdays=jd2r_spec_tramo_td(jtd),
      easter=jd2r_spec_tramo_easter(jeaster)))
}

#' Title
#'
#' @param spec
#' @param context_dictionary
#' @param extra_info
#' @param freq
#'
#' @return
#' @export
#'
#' @examples
jd2r_spec_tramo<- function(spec, context_dictionary = NULL,
                            extra_info = FALSE, freq = NA){

  # Transform
  jtransform<-.jcall(spec, "Ldemetra/tramo/TransformSpec;", "getTransform")
  jspan<-.jcall(jtransform, "Ldemetra/timeseries/TimeSelector;", "getSpan")
  span<-jd2r_span(jspan)
  preliminary.check<-.jcall(jtransform, "Z", "isPreliminaryCheck")
  transform.fct<-.jcall(jtransform, "D", "getFct")
  transform.function<-.jcall(.jcall(jtransform, "Ldemetra/modelling/TransformationType;", "getFunction"), "S", "name")

  userdef_spec<-NULL

  return(structure(list(
    basic=list(preliminarycheck = preliminary.check, span=span),
    transform=list(fn = transform.function,fct = transform.fct),
    estimate=jd2r_spec_tramo_estimate(spec),
    regression=jd2r_spec_tramo_regression(spec),
    outlier=jd2r_spec_tramo_outlier(spec),
    automdl=jd2r_spec_tramo_automdl(spec),
    arima=jd2r_spec_arima(spec)), class="JD3TRAMOSPEC"))
}

r2jd_tramo_transform<-function(spec){
  jfn<-.jcall("demetra/modelling/TransformationType", "Ldemetra/modelling/TransformationType;", "valueOf", spec$transform$fn)
  jbuilder<-.jcall("demetra/tramo/TransformSpec", "Ldemetra/tramo/TransformSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TransformSpec$Builder;", "preliminaryCheck", spec$basic$preliminarycheck)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TransformSpec$Builder;", "span", r2jd_span(spec$basic$span))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TransformSpec$Builder;", "function", jfn)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TransformSpec$Builder;", "fct", spec$transform$fct)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/TransformSpec"))
}

r2jd_tramo_estimate<-function(estimate){
  jbuilder<-.jcall("demetra/tramo/EstimateSpec", "Ldemetra/tramo/EstimateSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EstimateSpec$Builder;", "span", r2jd_span(estimate$span))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EstimateSpec$Builder;", "tol", estimate$tol)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EstimateSpec$Builder;", "maximumLikelihood", estimate$eml)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EstimateSpec$Builder;", "ubp", estimate$urfinal)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/EstimateSpec"))
}

r2jd_tramo_automdl<-function(ami){

  jbuilder<-.jcall("demetra/tramo/AutoModelSpec", "Ldemetra/tramo/AutoModelSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "enabled", ami$enabled)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "acceptDefault", ami$acceptdefault)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "cancel", ami$cancel)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "ub1", ami$ub1)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "ub2", ami$ub2)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "tsig", ami$armalimit)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "pc", ami$reducecv)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "pcr", ami$ljungboxlimit)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/AutoModelSpec$Builder;", "amiCompare", ami$compare)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/AutoModelSpec"))
}

r2jd_tramo_outlier<-function(outlier){
  jbuilder<-.jcall("demetra/tramo/OutlierSpec", "Ldemetra/tramo/OutlierSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "span", r2jd_span(outlier$span))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "ao", outlier$ao)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "ls", outlier$ls)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "tc", outlier$tc)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "so", outlier$so)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "criticalValue", outlier$cv)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/OutlierSpec$Builder;", "maximumLikelihood", outlier$eml)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/OutlierSpec"))
}

r2jd_tramo_easter<-function(easter){
  jbuilder<-.jcall("demetra/tramo/EasterSpec", "Ldemetra/tramo/EasterSpec$Builder;", "builder")
  jtype<-.jcall("demetra/tramo/EasterSpec$Type", "Ldemetra/tramo/EasterSpec$Type;", "valueOf", easter$type)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EasterSpec$Builder;", "type", jtype)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EasterSpec$Builder;", "julian", easter$julian)
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EasterSpec$Builder;", "duration", as.integer(easter$duration))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/EasterSpec$Builder;", "test", easter$test)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/EasterSpec"))
}

r2jd_tramo_td<-function(td){
  if (td$option == "Default"){
    if (td$mauto == "Unused"){
      jtdtype<-.jcall("demetra/timeseries/calendars/TradingDaysType", "Ldemetra/timeseries/calendars/TradingDaysType;", "valueOf", td$type)
      jlptype<-.jcall("demetra/timeseries/calendars/LengthOfPeriodType", "Ldemetra/timeseries/calendars/LengthOfPeriodType;", "valueOf", td$leapyear)
      jtesttype<-.jcall("demetra/tramo/RegressionTestType", "Ldemetra/tramo/RegressionTestType;", "valueOf", td$test)
      return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "td", jtdtype, jlptype, jtesttype))
    }else{
      jmethod<-.jcall("demetra/tramo/TradingDaysSpec$AutoMethod", "Ldemetra/tramo/TradingDaysSpec$AutoMethod;", "valueOf", td$mauto)
      return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "automatic", jmethod, td$pftd))
    }
  }else if (td$option == "Holiday"){
    if (td$mauto == "Unused"){
      jtdtype<-.jcall("demetra/timeseries/calendars/TradingDaysType", "Ldemetra/timeseries/calendars/TradingDaysType;", "valueOf", td$type)
      jlptype<-.jcall("demetra/timeseries/calendars/LengthOfPeriodType", "Ldemetra/timeseries/calendars/LengthOfPeriodType;", "valueOf", td$leapyear)
      jtesttype<-.jcall("demetra/tramo/RegressionTestType", "Ldemetra/tramo/RegressionTestType;", "valueOf", td$test)
      return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "holidays", td$holidays, jtdtype, jlptype, jtesttype))
    }else{
      jmethod<-.jcall("demetra/tramo/TradingDaysSpec$AutoMethod", "Ldemetra/tramo/TradingDaysSpec$AutoMethod;", "valueOf", td$mauto)
      return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "automaticHolidays", td$holidays, jmethod, pftd))
    }
  }else if (td$option == "Stock"){
    jtesttype<-.jcall("demetra/tramo/RegressionTestType", "Ldemetra/tramo/RegressionTestType;", "valueOf", td$test)
    return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "stockTradingDays", as.integer(td$w), jtesttype))
  }else if (td$option == "UserDefined"){
    jtesttype<-.jcall("demetra/tramo/RegressionTestType", "Ldemetra/tramo/RegressionTestType;", "valueOf", td$test)
    return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "userDefined", .jarray(td$userdefined), jtesttype))
  }else{
    return (.jcall("demetra/tramo/TradingDaysSpec", "Ldemetra/tramo/TradingDaysSpec;", "none"))
  }
}

r2jd_tramo_regression<-function(regression){

  jbuilder<-.jcall("demetra/tramo/CalendarSpec", "Ldemetra/tramo/CalendarSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/CalendarSpec$Builder;", "tradingDays", r2jd_tramo_td(regression$tradingdays))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/CalendarSpec$Builder;", "easter", r2jd_tramo_easter(regression$easter))
  jcal<-.jcall(jbuilder,"Ldemetra/tramo/CalendarSpec;", "buildWithoutValidation")

  jbuilder<-.jcall("demetra/tramo/RegressionSpec", "Ldemetra/tramo/RegressionSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/RegressionSpec$Builder;", "calendar", jcal)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/RegressionSpec"))

}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
r2jd_spec_tramo<-function(spec){
  jbuilder<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec$Builder;", "builder")

  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "transform", r2jd_tramo_transform(spec))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "estimate", r2jd_tramo_estimate(spec$estimate))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "arima", r2jd_arima(spec$arima))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "autoModel", r2jd_tramo_automdl(spec$automdl))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "outliers", r2jd_tramo_outlier(spec$outlier))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "regression", r2jd_tramo_regression(spec$regression))

  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramo/TramoSpec"))
}

jd2r_spec_seats<-function(spec){

  approximation<-.jcall(.jcall(spec, "Ldemetra/seats/DecompositionSpec$ModelApproximationMode;", "getApproximationMode"), "S", "name")
  nforecasts<-.jcall(spec, "I", "getForecastCount")
  nbackcasts<-.jcall(spec, "I", "getBackcastCount")
  trendboundary = .jcall(spec, "D", "getTrendBoundary")
  seasboundary = .jcall(spec, "D", "getSeasBoundary")
  seasboundaryatpi = .jcall(spec, "D", "getSeasBoundaryAtPi")
  seastolerance = .jcall(spec, "D", "getSeasTolerance")
  method<-.jcall(.jcall(spec, "Ldemetra/seats/DecompositionSpec$ComponentsEstimationMethod;", "getMethod"), "S", "name")
  return (list(approximation=approximation, nforecasts=nforecasts, nbackcasts=nbackcasts,
               trendboundary=trendboundary, seasboundary=seasboundary, seasboundaryatpi=seasboundaryatpi, seastolerance=seastolerance, method=method))
}

r2jd_spec_seats<-function(spec){
  jbuilder<-.jcall("demetra/seats/DecompositionSpec", "Ldemetra/seats/DecompositionSpec$Builder;", "builder")

  japp<-.jcall("demetra/seats/DecompositionSpec$ModelApproximationMode", "Ldemetra/seats/DecompositionSpec$ModelApproximationMode;", "valueOf", spec$approximation)
  jmethod<-.jcall("demetra/seats/DecompositionSpec$ComponentsEstimationMethod", "Ldemetra/seats/DecompositionSpec$ComponentsEstimationMethod;", "valueOf", spec$method)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "approximationMode", japp)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "forecastCount", as.integer(spec$nforecasts))
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "backcastCount", as.integer(spec$nbackcasts))
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "trendBoundary", spec$trendboundary)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "seasBoundary", spec$seasboundary)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "seasBoundaryAtPi", spec$seasboundaryatpi)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "seasTolerance", spec$seastolerance)
  jbuilder<-.jcall(jbuilder, "Ldemetra/seats/DecompositionSpec$Builder;", "method", jmethod)

  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/seats/DecompositionSpec"))

}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
jd2r_spec_tramoseats<-function(spec){
  tspec<-.jcall(spec, "Ldemetra/tramo/TramoSpec;", "getTramo")
  dspec<-.jcall(spec, "Ldemetra/seats/DecompositionSpec;", "getSeats")
  bspec<-.jcall(spec, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec;", "getBenchmarking")
  return(structure(list(
    tramo=jd2r_spec_tramo(tspec),
    seats=jd2r_spec_seats(dspec),
    benchmarking=jd2r_spec_benchmarking(bspec)), class="JD3TRAMOSEATSSPEC"))
}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
r2jd_spec_tramoseats<-function(spec){
  jbuilder<-.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramoseats/TramoSeatsSpec$Builder;", "tramo", r2jd_spec_tramo(spec$tramo))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramoseats/TramoSeatsSpec$Builder;", "seats", r2jd_spec_seats(spec$seats))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramoseats/TramoSeatsSpec$Builder;", "benchmarking", r2jd_spec_benchmarking(spec$benchmarking))

  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/tramoseats/TramoSeatsSpec"))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_tramo_default<-function(name){
  return (.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec;", "fromString", name))
}


#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_tramoseats_default<-function(name){
  return (.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec;", "fromString", name))
}



