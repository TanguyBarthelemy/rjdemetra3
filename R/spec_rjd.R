#' @include utils.R


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

jd2r_spec_tramo_arima<-function(spec){
  jarima<-.jcall(spec, "Ldemetra/arima/SarimaSpec;", "getArima")
  p <-.jcall(jarima, "I", "getP")
  d <-.jcall(jarima, "I", "getD")
  q <-.jcall(jarima, "I", "getQ")
  bp <-.jcall(jarima, "I", "getBp")
  bd <-.jcall(jarima, "I", "getBd")
  bq <-.jcall(jarima, "I", "getBq")
  coef.spec <- NA
  if (! .jcall(jarima, "Z", "isUndefined")){
    coef <- TRUE
    phi <-.jcall(jarima, "[Ldemetra/data/Parameter;", "getPhi")
    bphi <-.jcall(jarima, "[Ldemetra/data/Parameter;", "getBphi")
    theta <-.jcall(jarima, "[Ldemetra/data/Parameter;", "getTheta")
    btheta <-.jcall(jarima, "[Ldemetra/data/Parameter;", "getBtheta")
    coef.spec <-
      rbind(arimaCoef_jd2r(phi),
            arimaCoef_jd2r(bphi),
            arimaCoef_jd2r(theta),
            arimaCoef_jd2r(btheta))

  }
  return (list(p=p,d=d,q=q,bp=bp,bd=bd,bq=bq,coef.spec=coef.spec))
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
  test <-.jcall(jtd, "Z", "isTest")
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
      type<-.jcall(.jcall(jtd, "Ldemetra/timeseries/regression/TradingDaysType;", "getTradingDaysType"), "S", "name")
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
spec_tramo_jd2r <- function(spec, context_dictionary = NULL,
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
    basic=list(preliminary.check = preliminary.check, span=span),
    transform=list(fn = transform.function,fct = transform.fct),
    estimate=jd2r_spec_tramo_estimate(spec),
    regression=jd2r_spec_tramo_regression(spec),
    outlier=jd2r_spec_tramo_outlier(spec),
    automdl=jd2r_spec_tramo_automdl(spec),
    arima=jd2r_spec_tramo_arima(spec)), class="JD3TRAMOSPEC"))
}

r2jd_tramo_arima<-function(arima){

  jbuilder<-.jcall("demetra/arima/SarimaSpec", "Ldemetra/arima/SarimaSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "d", as.integer(arima$d))
  jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "bd", as.integer(arima$bd))
  if (is.na(arima$coef.spec)){
    jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "p", as.integer(arima$p))
    jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "q", as.integer(arima$q))
    jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "bp", as.integer(arima$bp))
    jbuilder<-.jcall(jbuilder, "Ldemetra/arima/SarimaSpec$Builder;", "bq", as.integer(arima$bq))
  }else{
    # TODO
    stop("Not implemented yet")
  }
  return (jbuilder$build())
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

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
spec_tramo_r2jd<-function(spec){

  jbuilder<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec$Builder;", "builder")
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "arima", r2jd_tramo_arima(spec$arima))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "autoModel", r2jd_tramo_automdl(spec$automdl))
  jbuilder<-.jcall(jbuilder, "Ldemetra/tramo/TramoSpec$Builder;", "outliers", r2jd_tramo_outlier(spec$outlier))

  return (jbuilder$build())
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




