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
spec_regarima_default<-function(name="rg4"){
  return (jd2r_spec_regarima(.jcall("demetra/regarima/RegArimaSpec", "Ldemetra/regarima/RegArimaSpec;", "fromString", name)))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_x13_default<-function(name="rsa4"){
  return (jd2r_spec_x13(.jcall("demetra/x13/X13Spec", "Ldemetra/x13/X13Spec;", "fromString", name)))
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
  return (jd2r_spec_x11(.jfield("demetra/x11/X11Spec", "Ldemetra/x11/X11Spec;", "DEFAULT")))
}

jd2r_spec_x11<-function(spec){
  b<-.jcall("demetra/x13/r/X11", "[B", "toBuffer", spec)
  p<-RProtoBuf::read(x13.X11Spec, b)
  return (p2r_spec_x11(p))
}

r2jd_spec_x11<-function(spec){
  p<-r2p_spec_x11(spec)
  b<-RProtoBuf::serialize(p, NULL)
  nspec<-.jcall("demetra/x13/r/X11", "Ldemetra/x11/X11Spec;", "of", b)
  return (nspec)
}

r2jd_spec_regarima<-function(spec){
  p<-r2p_spec_regarima(spec)
  b<-RProtoBuf::serialize(p, NULL)
  nspec<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaSpec;", "specOf", b)
  return (nspec)
}

jd2r_spec_regarima<-function(spec){
  b<-.jcall("demetra/x13/r/RegArima", "[B", "toBuffer", spec)
  p<-RProtoBuf::read(x13.RegArimaSpec, b)
  return (p2r_spec_regarima(p))
}


r2jd_spec_x13<-function(spec){
  p<-r2p_spec_x13(spec)
  b<-RProtoBuf::serialize(p, NULL)
  nspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "specOf", b)
  return (nspec)
}

jd2r_spec_x13<-function(spec){
  b<-.jcall("demetra/x13/r/X13", "[B", "toBuffer", spec)
  p<-RProtoBuf::read(x13.Spec, b)
  return (p2r_spec_x13(p))
}

## P <-> R

p2r_spec_regarima<-function(pspec){
  basic<-list(
    span=p2r_span(pspec$basic$span),
    preprocessing = pspec$basic$preprocessing,
    preliminaryCheck = pspec$basic$preliminary_check)
  transform=list(
    fn=enum_extract(regarima.Transformation, pspec$transform$transformation),
    adjust=enum_extract(regarima.LengthOfPeriod, pspec$transform$adjust),
    aicdiff=pspec$transform$aicdiff)

  automodel=list(
    enabled=pspec$automodel$enabled,
    ljungbox=pspec$automodel$ljungbox,
    tsig=pspec$automodel$tsig,
    predcv=pspec$automodel$predcv,
    ubfinal=pspec$automodel$ubfinal,
    ub1=pspec$automodel$ub1,
    ub2=pspec$automodel$ub2,
    cancel=pspec$automodel$cancel,
    fct=pspec$automodel$fct,
    acceptdef=pspec$automodel$acceptdef,
    checkmu=pspec$automodel$checkmu,
    mixed=pspec$automodel$mixed,
    balanced=pspec$automodel$balanced
  )

  arima=p2r_spec_sarima(pspec$arima)

  outlier<-list(
    outliers=lapply(pspec$outlier$outliers, function(z){list(type=z$code, va=z$va)} ),
    span=p2r_span(pspec$outlier$span),
    defva=pspec$outlier$defva,
    method=enum_extract(x13.OutlierMethod, pspec$outlier$method),
    monthlytcrate=pspec$outlier$monthly_tc_rate,
    maxiter=pspec$outlier$maxiter,
    lsrun=pspec$outlier$lsrun
  )

  td<-list(
    td=enum_extract(regarima.TradingDays, pspec$regression$td$td),
    lp=enum_extract(regarima.LengthOfPeriod, pspec$regression$td$lp),
    holidays=pspec$regression$td$holidays,
    users=unlist(pspec$regression$td$users),
    w=pspec$regression$td$w,
    test=enum_extract(x13.RegressionTest, pspec$regression$td$test),
    autoadjust=pspec$regression$td$auto_adjust,
    tdcoefficients=p2r_parameters(pspec$regression$td$tdcoefficients),
    lpcoefficient=p2r_parameter(pspec$regression$td$lpcoefficient)
  )

  easter<-list(
    type=enum_extract(x13.EasterType, pspec$regression$easter$type),
    duration=pspec$regression$easter$duration,
    test=enum_extract(x13.RegressionTest, pspec$regression$easter$test),
    coefficient=p2r_parameter(pspec$regression$easter$coefficient)
  )

  # TODO: complete regression
  regression<-list(
    mean=p2r_parameter(pspec$regression$mean),
    td=td,
    easter=easter,
    outliers=p2r_outliers(pspec$regression$outliers),
    ramps=p2r_ramps(pspec$regression$ramps)
  )

  estimate<-list(
    span=p2r_span(pspec$estimate$span),
    tol=pspec$estimate$tol
  )
  return (structure(
    list(
      basic=basic,
      transform=transform,
      outlier=outlier,
      arima=arima,
      automodel=automodel,
      regression=regression,
      estimate=estimate
    ),
    class="JD3REGARIMASPEC"))
}


r2p_spec_regarima<-function(r){
  p<-x13.RegArimaSpec$new()
  # BIAS
  p$basic$preliminary_check<-r$basic$preliminaryCheck
  p$basic$preprocessing<-r$basic$preprocessing
  p$basic$span<-r2p_span(r$basic$span)

  # TRANSFORM
  p$transform$transformation<-enum_of(regarima.Transformation, r$transform$fn, "FN")
  p$transform$adjust<-enum_of(regarima.LengthOfPeriod, r$transform$adjust, "LP")
  p$transform$aicdiff<-r$transform$aicdiff

  # OUTLIER
  p$outlier$outliers<-lapply(r$outlier$outliers, function(z)
    {t<-x13.RegArimaSpec$OutlierSpec$Type$new();t$code=z$type; t$va=z$va; return (t)})
  p$outlier$span<-r2p_span(r$outlier$span)
  p$outlier$defva<-r$outlier$defva
  p$outlier$method<-enum_of(x13.OutlierMethod, r$outlier$method, "OUTLIER")
  p$outlier$monthly_tc_rate<-r$outlier$monthlytcrate
  p$outlier$maxiter<-r$outlier$maxiter
  p$outlier$lsrun<-r$outlier$lsrun

  #AMI

  p$automodel$enabled<-r$automodel$enabled
  p$automodel$ljungbox<-r$automodel$ljungbox
  p$automodel$tsig<-r$automodel$tsig
  p$automodel$predcv<-r$automodel$predcv
  p$automodel$ubfinal<-r$automodel$ubfinal
  p$automodel$ub1<-r$automodel$ub1
  p$automodel$ub2<-r$automodel$ub2
  p$automodel$cancel<-r$automodel$cancel
  p$automodel$fct<-r$automodel$fct
  p$automodel$acceptdef<-r$automodel$acceptdef
  p$automodel$checkmu<-r$automodel$checkmu
  p$automodel$mixed<-r$automodel$mixed
  p$automodel$balanced<-r$automodel$balanced

  #ARIMA
  p$arima<-r2p_spec_sarima(r$arima)

  #REGRESSION

  p$regression$mean=r2p_parameter(r$regression$mean)
  p$regression$outliers=r2p_outliers(r$regression$outliers)
  p$regression$ramps=r2p_ramps(r$regression$ramps)

  #TD
  p$regression$td$td<-enum_of(regarima.TradingDays, r$regression$td$td, "TD")
  p$regression$td$lp<-enum_of(regarima.LengthOfPeriod, r$regression$td$lp, "LP")
  p$regression$td$holidays<-r$regression$td$holidays
  p$regression$td$users<-r$regression$td$users
  p$regression$td$w<-r$regression$td$w
  p$regression$td$test <-enum_of(x13.RegressionTest, r$regression$td$test, "TEST")
  p$regression$td$auto_adjust <-r$regression$td$autoadjust
  p$regression$td$tdcoefficients<-r2p_parameters(r$regression$td$tdcoefficients)
  p$regression$td$lpcoefficient<-r2p_parameter(r$regression$td$lpcoefficient)

  #EASTER
  p$regression$easter$type<-enum_of(x13.EasterType, r$regression$easter$type, "EASTER")
  p$regression$easter$duration<-r$regression$easter$duration
  p$regression$easter$test<-enum_of(x13.RegressionTest, r$regression$easter$test, "TEST")
  p$regression$easter$coefficient<-r2p_parameter(r$regression$easter$coefficient)

  #ESTIMATE
  p$estimate$span<-r2p_span(r$estimate$span)
  p$estimate$tol<-r$estimate$tol

  return (p)
}


p2r_spec_x11<-function(p){

  return (list(
    mode=enum_extract(sa.DecompositionMode, p$mode),
    seasonal=p$seasonal,
    henderson=p$henderson,
    sfilters=sapply(p$sfilters, function(z){enum_extract(x13.SeasonalFilter, z)}),
    lsig=p$lsig,
    usig=p$usig,
    nfcasts=p$nfcasts,
    nbcasts=p$nbcasts,
    sigma=enum_extract(x13.CalendarSigma, p$sigma),
    vsigmas=p$vsigmas,
    excludefcasts=p$exclude_fcasts,
    bias=enum_extract(x13.BiasCorrection, p$bias)
  ))
}


r2p_spec_x11<-function(r){
  p<-x13.X11Spec$new()
  p$mode<- enum_of(x13.DecompositionMode, r$mode, "MODE")
  p$seasonal<-r$seasonal
  p$henderson<-r$henderson
  p$sfilters<-sapply(r$sfilters, function(z){enum_of(x13.SeasonalFilter, z, "SEASONAL")} )
  p$lsig<-r$lsig
  p$usig<-r$usig
  p$nfcasts<-r$nfcasts
  p$nbcasts<-r$nbcasts
  p$sigma<-enum_of(x13.CalendarSigma, r$sigma, "SIGMA")
  p$vsigmas<-r$vsigmas
  p$exclude_fcasts<-r$excludefcasts
  p$bias<-enum_of(x13.BiasCorrection, r$bias, "BIAS")
  return (p)
}

p2r_spec_x13<-function(pspec){
  return (structure(list(
    regarima=p2r_spec_regarima(pspec$regarima),
    x11=p2r_spec_x11(pspec$x11),
    benchmarking=p2r_spec_benchmarking(pspec$benchmarking)
  ), class="JD3X13SPEC"))
}

r2p_spec_x13<-function(r){
  p<-x13.Spec$new()
  p$regarima<-r2p_spec_regarima(r$regarima)
  p$x11<-r2p_spec_x11(r$x11)
  p$benchmarking<-r2p_spec_benchmarking(r$benchmarking)
  return (p)
}


