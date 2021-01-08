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
spec_x13_default<-function(name){
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
  q<-.jcall("demetra/x13/r/X11", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(x13.X11Spec, q)
  return (rq)
}

r2jd_spec_x11<-function(spec){
  nq<-RProtoBuf::serialize(spec, NULL)
  nspec<-.jcall("demetra/x13/r/X11", "Ldemetra/x11/X11Spec;", "of", nq)
  return (nspec)
}

r2jd_spec_regarima<-function(spec){
  nq<-RProtoBuf::serialize(spec, NULL)
  nspec<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaSpec;", "of", nq)
  return (nspec)
}

jd2r_spec_regarima<-function(spec){
  q<-.jcall("demetra/x13/r/RegArima", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(x13.RegArimaSpec, q)
  return (rq)
}


r2jd_spec_x13<-function(spec){
  nq<-RProtoBuf::serialize(spec, NULL)
  nspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "of", nq)
  return (nspec)
}

jd2r_spec_x13<-function(spec){
  q<-.jcall("demetra/x13/r/X13", "[B", "toBuffer", spec)
  rq<-RProtoBuf::read(x13.Spec, q)
  return (rq)
}
