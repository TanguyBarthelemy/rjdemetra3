# Enums

enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
}

# Span

p2r_span<-function(span){

  type<-enum_extract(jd3.SelectionType, span$type)
  d<-span$d0
  if (nchar(d)>0)  dt0<-as.Date(d) else dt0=NULL
  d<-span$d1
  if (nchar(d)>0)  dt1<-as.Date(d) else dt1=NULL

  return (structure(list(type=type, d0=dt0, d1=dt1, n0=span$n0, n1=span$n1), class= "JD3SPAN"))
}

r2p_span<-function(rspan){
  pspan<-jd3.TimeSelector$new()
  pspan$type<-enum_of(jd3.SelectionType, rspan$type, "SPAN")
  pspan$n0<-rspan$n0
  pspan$n1<-rspan$n1
  if (! is.null(rspan$d0)) pspan$d0<-as.character(rspan$d0)
  if (! is.null(rspan$d1)) pspan$d1<-as.character(rspan$d1)

  return (pspan)
}

# Parameter

# Matrix in the following form:
# row(1): values
# row(2): Parameters type

p2r_parameters<-function(p){
  n<-length(p)
  if (n == 0) return (NULL)
  r<-sapply(p, function(z){c(z$value, z$type)})
  return (r)
}

r2p_parameter<-function(v, t){
  p<-jd3.Parameter$new()
  p$value<-v
  p$type<-t
  return (p)
}

r2p_parameters<-function(r){

  n<-length(r)
  if (n == 0) return (NULL)
  p<-apply(r, 2, function(z){r2p_parameter(z[1], z[2])})
    return (p)
}

# Sarima

p2r_spec_sarima<-function(spec){
  return (list(
    d=spec$d,
    bd=spec$bd,
    phi=p2r_parameters(spec$phi),
    theta=p2r_parameters(spec$theta),
    bphi=p2r_parameters(spec$bphi),
    btheta=p2r_parameters(spec$btheta)
  ))
}

r2p_spec_sarima<-function(r){
  p<-regarima.SarimaSpec$new()
  p$d<-r$d
  p$bd<-r$bd
  p$phi<-r2p_parameters(r$phi)
  p$theta<-r2p_parameters(r$theta)
  p$bphi<-r2p_parameters(r$bphi)
  p$btheta<-r2p_parameters(r$btheta)
  return (p)
}

# Benchmarking

p2r_spec_benchmarking<-function(p){
  return (list(
    enabled=p$enabled,
    target=enum_extract(sa.BenchmarkingTarget, p$target),
    lambda=p$lambda,
    rho=p$rho,
    bias=enum_extract(sa.BenchmarkingBias, p$bias),
    forecast=p$forecast
  ))
}

r2p_spec_benchmarking<-function(r){
  p<-sa.BenchmarkingSpec$new()
  p$enabled<-r$enabled
  p$target<-enum_of(sa.BenchmarkingTarget, r$target, "BENCH")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-enum_of(sa.BenchmarkingBias, r$bias, "BENCH")
  p$forecast<-r$forecast
  return (p)
}

