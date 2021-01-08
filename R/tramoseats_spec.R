#' @include utils.R
NULL

jd2r_spec_tramo<-function(spec, context_dictionary = NULL){
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.TramoSpec, q)
  return (rq)
}

r2jd_spec_tramo<-function(spec){
  nq<-RProtoBuf::serialize(spec, NULL)
  nspec<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoSpec;", "of", nq)
  return (nspec)
}

jd2r_spec_tramoseats<-function(spec){
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.Spec, q)
  p2r_spec_tramoseats(rq)
  return (rq)
}

r2jd_spec_tramoseats<-function(spec){
  nq<-RProtoBuf::serialize(spec, NULL)
  nspec<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/TramoSeatsSpec;", "of", nq)
  return (nspec)
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
  rq<-raw_spec_tramo_default(name)
  return (p2r_spec_tramo(rq))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
raw_spec_tramo_default<-function(name){
  spec<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec;", "fromString", name)
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.TramoSpec, q)
  return (rq)
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
  rq<-raw_spec_tramoseats_default(name)
  return (p2r_spec_tramoseats(rq))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
raw_spec_tramoseats_default<-function(name){
  spec<-.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec;", "fromString", name)
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(tramoseats.Spec, q)
  return (rq)
}

p2r_parameters<-function(p){
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){z$type})
  return (cbind(value, type))
}


p2r_spec_tramo<-function(pspec){
  b<-pspec$basic
  basic<-list(span=p2r_span(b$span), preliminaryCheck = b$preliminary_check)
  t<-pspec$transform
  transform=list(fn=enum_extract(regarima.Transformation, t$transformation), fct=t$fct)
  a<-pspec$automodel
  automodel=list(enabled=a$enabled, cancel=a$cancel, ub1=a$ub1, ub2=a$ub2, pcr=a$pcr, pc=a$pc, tsig=a$tsig, amicompare=a$ami_compare)
  m<-pspec$arima
  arima=list(d=m$d, bd=m$bd,
             phi=p2r_parameters(m$phi), theta=p2r_parameters(m$theta),
             bphi=p2r_parameters(m$bphi), btheta=p2r_parameters(m$btheta))
  o<-pspec$outlier
  outlier<-list(span=p2r_span(o$span), ao=o$ao, ls=o$ls, tc=o$tc, so=o$so, va=o$va, tcrate=o$tcrate, ml=o$ml)
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
  estimate<-list(span=p2r_span(e$span), tol=e$tol, ubp=e$ubp)
  return (structure(list(basic=basic, transform=transform, outlier=outlier, automodel=automodel, regression=regression, estimate=estimate), class="JD3TRAMOSPEC"))
}


r2p_spec_tramo<-function(rspec, pspec){
  # BIAS
  pspec$basic$preliminary_check<-rspec$basic$preliminaryCheck
  pspan<-jd3.TimeSelector$new()
  r2p_span(rspec$basic$span, pspan)
  pspec$basic$span<-pspan

  # TRANSFORM
  pt<-pspec$transform
  rt<-rspec$transform
  pt$transformation<-enum_of(regarima.Transformation, rt$fn, "FN")
  pt$fct<-rt$fct

  #OUTLIER
  po<-pspec$outlier
  ro<-rspec$outlier
  r2p_span(ro$span, po$span)
  po$enabled<-ro$enabled
  po$ao<-ro$ao
  po$ls<-ro$ls
  po$tc<-ro$tc
  po$so<-ro$s
  po$va<-ro$va
  po$tcrate=ro$tcrate
  po$ml<-ro$ml
}


p2r_spec_tramoseats<-function(pspec){
  return (structure(list(tramo=p2r_spec_tramo(pspec$tramo)), class="JD3TRAMOSEATSSPEC"))
}
