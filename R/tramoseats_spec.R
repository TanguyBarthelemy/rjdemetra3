#' @include common_spec.R
NULL


#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_tramo_default<-function(name="trfull"){
  jspec<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec;", "fromString", name)
  return (jd2r_spec_tramo(jspec))
}


#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
spec_tramoseats_default<-function(name="rsafull"){
  jspec<-.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec;", "fromString", name)
  return (jd2r_spec_tramoseats(jspec))
}

## JD <-> R

jd2r_spec_tramo<-function(spec, context_dictionary = NULL){
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.TramoSpec, q)
  return (p2r_spec_tramo(rq))
}

r2jd_spec_tramo<-function(spec){
  pspec<-r2p_spec_tramo(spec)
  nq<-RProtoBuf::serialize(pspec, NULL)
  nspec<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoSpec;", "of", nq)
  return (nspec)
}

jd2r_spec_tramoseats<-function(spec){
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.Spec, q)
  return (p2r_spec_tramoseats(rq))
}

r2jd_spec_tramoseats<-function(spec){
  pspec<-r2p_spec_tramoseats(spec)
  nq<-RProtoBuf::serialize(pspec, NULL)
  nspec<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/TramoSeatsSpec;", "of", nq)
  return (nspec)
}


## P <-> R


p2r_spec_tramo<-function(pspec){
  b<-pspec$basic
  basic<-list(span=p2r_span(b$span), preliminaryCheck = b$preliminary_check)
  t<-pspec$transform
  transform=list(fn=enum_extract(regarima.Transformation, t$transformation), fct=t$fct)
  a<-pspec$automodel
  automodel=list(enabled=a$enabled, acceptdef=a$accept_def, cancel=a$cancel, ub1=a$ub1, ub2=a$ub2, pcr=a$pcr, pc=a$pc, tsig=a$tsig, amicompare=a$ami_compare)
  arima=p2r_spec_sarima(pspec$arima)
  o<-pspec$outlier
  outlier<-list(enabled=o$enabled, span=p2r_span(o$span), ao=o$ao, ls=o$ls, tc=o$tc, so=o$so, va=o$va, tcrate=o$tcrate, ml=o$ml)
  r<-pspec$regression
  ptd<-pspec$regression$td
  pee<-pspec$regression$easter
  td<-list(td=enum_extract(regarima.TradingDays, ptd$td), lp=enum_extract(regarima.LengthOfPeriod, ptd$lp),
           holidays=ptd$holidays, users=unlist(ptd$users), w=ptd$w
           , test=enum_extract(tramoseats.TradingDaysTest, ptd$test), auto=enum_extract(tramoseats.AutomaticTradingDays, ptd$auto),
           ptest=ptd$ptest)
  easter<-list(type=enum_extract(tramoseats.EasterType, pee$type), duration=pee$duration, julian=pee$julian, test=pee$test)
  # TODO: complete regression
  regression<-list(mean=r$mean, td=td, easter=easter)
  e<-pspec$estimate
  estimate<-list(span=p2r_span(e$span), ml=e$ml, tol=e$tol, ubp=e$ubp)
  return (structure(
    list(basic=basic, transform=transform, outlier=outlier,
         arima=arima, automodel=automodel, regression=regression, estimate=estimate),
    class="JD3TRAMOSPEC"))
}


r2p_spec_tramo<-function(rspec){
  pspec<-tramoseats.TramoSpec$new()
  # BIAS
  pspec$basic$span<-r2p_span(rspec$basic$span)
  pspec$basic$preliminary_check<-rspec$basic$preliminaryCheck

  # TRANSFORM
  pspec$transform$transformation<-enum_of(regarima.Transformation, rspec$transform$fn, "FN")
  pspec$transform$fct<-rspec$transform$fct

  #OUTLIER

  pspec$outlier$enabled<-rspec$outlier$enabled
  pspec$outlier$span<-r2p_span(rspec$outlier$span)
  pspec$outlier$ao<-rspec$outlier$ao
  pspec$outlier$ls<-rspec$outlier$ls
  pspec$outlier$tc<-rspec$outlier$tc
  pspec$outlier$so<-rspec$outlier$so
  pspec$outlier$va<-rspec$outlier$va
  pspec$outlier$tcrate=rspec$outlier$tcrate
  pspec$outlier$ml<-rspec$outlier$ml

  #AMI

  pspec$automodel$enabled<-rspec$automodel$enabled
  pspec$automodel$cancel<-rspec$automodel$cancel
  pspec$automodel$ub1<-rspec$automodel$ub1
  pspec$automodel$ub2<-rspec$automodel$ub2
  pspec$automodel$pcr<-rspec$automodel$pcr
  pspec$automodel$pc<-rspec$automodel$pc
  pspec$automodel$tsig<-rspec$automodel$tsig
  pspec$automodel$accept_def<-rspec$automodel$acceptdef
  pspec$automodel$ami_compare<-rspec$automodel$amicompare

  #ARIMA
  pspec$arima<-r2p_spec_sarima(rspec$arima)

  #REGRESSION

  pspec$regression$mean=rspec$regression$mean

  #TD
  pspec$regression$td$td<-enum_of(regarima.TradingDays, rspec$regression$td$td, "TD")
  pspec$regression$td$lp<-enum_of(regarima.LengthOfPeriod, rspec$regression$td$lp, "LP")
  pspec$regression$td$holidays<-rspec$regression$td$holidays
  pspec$regression$td$users<-rspec$regression$td$users
  pspec$regression$td$w<-rspec$regression$td$w
  pspec$regression$td$test <-enum_of(tramoseats.TradingDaysTest, rspec$regression$td$test, "TD")
  pspec$regression$td$auto <-enum_of(tramoseats.AutomaticTradingDays, rspec$regression$td$auto, "TD")
  pspec$regression$td$ptest<-rspec$regression$td$ptest

  #EASTER
  pspec$regression$easter$type<-enum_of(tramoseats.EasterType, rspec$regression$easter$type, "EASTER")
  pspec$regression$easter$duration<-rspec$regression$easter$duration
  pspec$regression$easter$julian<-rspec$regression$easter$julian
  pspec$regression$easter$test<-rspec$regression$easter$test

  #ESTIMATE
  pspec$estimate$span<-r2p_span(rspec$estimate$span)
  pspec$estimate$ml-rspec$estimate$ml
  pspec$estimate$tol<-rspec$estimate$tol
  pspec$estimate$ubp<-rspec$estimate$ubp

  return (pspec)
}

# SEATS

p2r_spec_seats<-function(spec){
  return (list(
    xl=spec$xl_boundary,
    approximation=enum_extract(tramoseats.SeatsApproximation, spec$approximation),
    epsphi=spec$seastolerance,
    rmod=spec$trend_boundary,
    sbound=spec$seas_boundary,
    sboundatpi=spec$seas_boundary_at_pi,
    bias=spec$bias_correction,
    nfcasts=spec$nfcasts,
    nbcasts=spec$nbcasts,
    algorithm=enum_extract(tramoseats.SeatsAlgorithm, spec$algorithm)
  ))
}

r2p_spec_seats<-function(spec){
  pspec<-tramoseats.DecompositionSpec$new()
  pspec$xl_boundary<-spec$xl
  pspec$approximation<-enum_of(tramoseats.SeatsApproximation, spec$approximation, "SEATS")
  pspec$seastolerance<-spec$epsphi
  pspec$trend_boundary<-spec$rmod
  pspec$seas_boundary<-spec$sbound
  pspec$seas_boundary_at_pi<-spec$sboundatpi
  pspec$bias_correction<-spec$bias
  pspec$nfcasts<-spec$nfcasts
  pspec$nbcasts<-spec$nbcasts
  pspec$algorithm<-enum_of(tramoseats.SeatsAlgorithm, spec$algorithm, "SEATS")
  return (pspec)
}

p2r_spec_tramoseats<-function(pspec){
  return (structure(list(
    tramo=p2r_spec_tramo(pspec$tramo),
    seats=p2r_spec_seats(pspec$seats),
    benchmarking=p2r_spec_benchmarking(pspec$benchmarking)
    ), class="JD3TRAMOSEATSSPEC"))
}

r2p_spec_tramoseats<-function(r){
  p<-tramoseats.Spec$new()
  p$tramo<-r2p_spec_tramo(r$tramo)
  p$seats<-r2p_spec_seats(r$seats)
  p$benchmarking<-r2p_spec_benchmarking(r$benchmarking)
  return (p)
}
