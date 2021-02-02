#' @include utils.R jd3_rslts.R
NULL

regarima_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(regarima.RegArimaEstimation, q)
  return (p2r_regarima_rslts(rq))
}

p2r_var<-function(p){
  name<-sapply(p, function(z){z$name})
  type<-sapply(p, function(z){enum_extract(regarima.VariableType, z$var_type)})
  coef<-sapply(p, function(z){z$coefficient})
  stde<-sapply(p, function(z){z$stde})
  pvalue<-sapply(p, function(z){z$pvalue})
  a<-data.frame(coef=coef, stde=stde, pvalue=pvalue, type=type)
  row.names(a)<-name
  return (a)
}

p2r_regarima_rslts<-function(p){
  log<-p$transformation == regarima.Transformation$FN_LOG
  return (list(log=log, preadjustment = enum_extract(regarima.LengthOfPeriod, p$preadjustment),
               y=p$y,
               x=p2r_matrix(p$x),
               sarima=p2r_sarima(p$sarima),
               likelihood=p2r_likelihood(p$likelihood),
               res=p$residuals,
               variables=p2r_var(p$variables),
               coefficients=p$coefficients,
               covariance=p2r_matrix(p$covariance)))
}

