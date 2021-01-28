identical_na <- function(x){
  identical(x, NA) ||
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}


ramp <- function(start = 1990, end = 2020,
                 start_ramp, end_ramp, frequency = 12){
  start <- format_ts_date(start, frequency)
  end <- format_ts_date(end, frequency)
  start_ramp <- format_ts_date(start_ramp, frequency)
  end_ramp <- format_ts_date(end_ramp, frequency)

  if (start_ramp >= end_ramp)
    return(NULL)
  if (missing(start) || missing(end)) {
    # if start and end not specified by hand
    start <- min(start_ramp - 1, start)
    end <- min(end_ramp + 1, end)
  }

  x <- ts(-1, start = start, end = end,
          frequency = frequency)
  t <- ts(1:length(x), start = start, end = end,
          frequency = frequency)
  t0 <- as.numeric(window(t, start = start_ramp, end = start_ramp))
  t1 <- as.numeric(window(t, start = end_ramp, end = end_ramp))
  x <- -1 * (t <= t0) + ((t - t0) / (t1-t0) - 1) * (t > t0) * (t < t1)
  x
}

format_ts_date <- function(date, frequency){
  if (length(date) == 2)
    date <- date[1] + (date[2] - 1) / frequency
  date
}

j2r_ldt<-function(ldt){
  if (is.jnull(ldt))
    return (NULL)
  dt<-.jcall(ldt, "Ljava/time/LocalDate;", "toLocalDate")
  return (as.Date(.jcall(dt, "S", "toString")))
}

j2r_dt<-function(dt){
  if (is.jnull(dt))
    return (NULL)
  return (as.Date(.jcall(dt, "S", "toString")))
}

r2j_dt<-function(dt){
  jdt<-.jnew("java/lang/String", as.character(dt))
  return (.jcall("java/time/LocalDate", "Ljava/time/LocalDate;", "parse", .jcast(jdt, "java/lang/CharSequence")))
}

r2j_ldt<-function(dt){
  jdt<-r2j_dt(dt)
  return (.jcall(jdt, "Ljava/time/LocalDateTime;", "atStartOfDay"))
}

jd2r_span <- function(jspan){

  type<-.jcall(.jcall(jspan, "Ldemetra/timeseries/TimeSelector$SelectionType;", "getType"), "S","name")
  d0<-j2r_ldt(.jcall(jspan, "Ljava/time/LocalDateTime;", "getD0"))
  d1<-j2r_ldt(.jcall(jspan, "Ljava/time/LocalDateTime;", "getD1"))
  n0<-.jcall(jspan, "I", "getN0")
  n1<-.jcall(jspan, "I", "getN1")

  return (structure(list(type=type, d0=d0, d1=d1, n0=n0, n1=n1), class= "JD3SPAN"))
}

r2jd_span <- function(span){

  if (span$type == "All") return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "all"))
  if (span$type == "First")return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "first", as.integer(span$n0)))
  if (span$type == "Last")return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "last", as.integer(span$n1)))
  if (span$type == "Excluding")return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "excluding", as.integer(span$n0), as.integer(span$n1)))
  if (span$type == "From"){
    return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "from", as.integer(span$n0)))
  }
  return (.jcall("demetra/timeseries/TimeSelector", "Ldemetra/timeseries/TimeSelector;", "none"))
}



jd2r_parameters <- function(jparams){
  if (is.jnull(jparams))
    return(NULL)
  param<-.jcastToArray(jparams)
  len <- length(param)
  if (len==0)
    return (NULL)
  param_name <- deparse(substitute(jparams))
  Type <- sapply(param, function(x) .jcall(.jcall(x, "Ldemetra/data/ParameterType;", "getType"), "S", "name"))
  Value <- sapply(param, function(x) .jcall(x, "D", "getValue"))
  data_param <- data.frame(Type = Type, Value = Value)
  rownames(data_param) <- sprintf("%s(%i)",
                                  param_name,
                                  1:len)
  data_param
}

jd2r_spec_benchmarking<-function(spec){
  enabled<-.jcall(spec, "Z", "isEnabled")
  forecast<-.jcall(spec, "Z", "isForecast")
  target<-.jcall(.jcall(spec, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Target;", "getTarget"), "S", "name")
  rho<-.jcall(spec, "D", "getRho")
  lambda<-.jcall(spec, "D", "getLambda")
  bias<-.jcall(.jcall(spec, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$BiasCorrection;", "getBiasCorrection"), "S", "name")
  return (list(enabled=enabled, forecast=forecast, target=target,
               rho=rho, lambda=lambda, bias=bias))
}

r2jd_spec_benchmarking<-function(spec){
  jbuilder<-.jcall("demetra/sa/benchmarking/SaBenchmarkingSpec", "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "builder")
  jtarget<-.jcall("demetra/sa/benchmarking/SaBenchmarkingSpec$Target", "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Target;", "valueOf", spec$target)
  jbias<-.jcall("demetra/sa/benchmarking/SaBenchmarkingSpec$BiasCorrection", "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$BiasCorrection;", "valueOf", spec$bias)

  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "enabled", spec$enabled)
  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "forecast", spec$forecast)
  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "target", jtarget)
  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "rho", spec$rho)
  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "lambda", spec$lambda)
  jbuilder<-.jcall(jbuilder, "Ldemetra/sa/benchmarking/SaBenchmarkingSpec$Builder;", "biasCorrection", jbias)
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/sa/benchmarking/SaBenchmarkingSpec"))
}

jd2r_spec_arima<-function(spec){
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
      rbind(jd2r_parameters(phi),
            jd2r_parameters(bphi),
            jd2r_parameters(theta),
            spec<-spec_tramo_default("TRfull")
            (btheta))

  }
  return (list(p=p,d=d,q=q,bp=bp,bd=bd,bq=bq,coef.spec=coef.spec))
}

r2jd_arima<-function(arima){

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
  jval<-.jcall(jbuilder, "Ldemetra/util/Validatable;", "build")
  return (.jcast(jval, "demetra/arima/SarimaSpec"))
}

jd2r_enumlist<-function(jrslt){
  if (is.jnull(jrslt)) return (NULL)
  size<-.jcall(jrslt, "I", "size")
  if (size == 0)
    return (NULL)
  v<-array(dim=size)
  jiter<-.jcall(jrslt, "Ljava/util/Iterator;", "iterator")
  for (i in 1:size){
    v[i]=.jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
  }
  return (v)
}

#### PROTOBUF FUNCTIONS


enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
}

p2r_likelihood<-function(p){
  return (list(nobs=p$nobs, neffectiveobs=p$neffectiveobs, nparams=p$nparams,
               ll=p$log_likelihood, adjll=p$adjusted_log_likelihood,
               aic=p$aic, aicc=p$aicc, bic=p$bic, bicc=p$bicc, ssq=p$ssq))
}

p2r_matrix<-function(p){
  m<-matrix(data=p$values, nrow = p$nrows, ncol = p$ncols)
  `attr<-`(m, "name", p$name)
  return (m)
}

p2r_ts<-function(p){
  if (length(p$values) == 0)
    return (NULL)
  s<-ts(data=p$values, frequency = p$period, start = c(p$start_year, p$start_period))
  `attr<-`(s, "name", p$name)
  return (s)
}


p2r_parameters<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  return (data.frame(value=value, type=type))
}

p2r_sarima<-function(p){
  return (structure(list(period=p$period, p = p$p, d=p$d, q=p$q, bp = p$bp, bd = p$bd, bq = p$bq,
               parameters=p$parameters, covariance=p2r_matrix(p$covariance)), class= "JD3SARIMA"))
}

p2r_arima<-function(p){
  return (structure(list(name=p$name, innovationvariance=p$innovation_variance, ar=p$ar, delta=p$delta, ma=p$ma), class= "JD3ARIMA"))
}


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
  pspan$d0<-as.character(rspan$d0)
  pspan$d1<-as.character(rspan$d1)

  return (pspan)
}

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
  p$target<-enum_of(sa.BenchmarkingTarget, r$target, "BENCH_TARGET")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-enum_of(sa.BenchmarkingBias, r$bias, "BENCH_BIAS")
  p$forecast<-r$forecast
  return (p)
}

p2r_sacomponent<-function(p){
  return (list(type=enum_extract(sa.ComponentType, p$type), data=p2r_ts(p$data), stde=p2r_ts(p$stde), nbcasts=p$nbcasts, nfcasts=p$nfcasts))
}

p2r_sa_decomposition<-function(p){
  return (list(mode = enum_extract(sa.DecompositionMode, p$mode),
               components=lapply(p$components, function(z){p2r_sacomponent(z)})))
}

p2r_ucarima<-function(p){

  return (structure
          (list(
    model=p2r_arima(p$model),
    components=lapply(p$components, function(z){p2r_arima(z)})),
    class= "JD3UCARIMA"))
}

#' Title
#'
#' @param m
#'
#' @return
#' @export
#'
#' @examples
print.JD3ARIMA<-function(m){
  cat(m$name, "\n\n")
  cat("AR: ", m$ar, "\n")
  cat("DIF: ", m$delta, "\n")
  cat("MA: ", m$ma, "\n")
  cat("var: ", m$innovationvariance, "\n\n")
}


#' Title
#'
#' @param ucm
#'
#' @return
#' @export
#'
#' @examples
print.JD3UCARIMA<-function(ucm){
print(ucm$model)
lapply(ucm$components, function(z){print(z)})
}




#' Title
#'
#' @param span
#'
#' @return
#' @export
#'
#' @examples
print.JD3SPAN<-function(span){
  type<-span$type
  d0<-span$d0
  d1<-span$d1
  n0<-span$n0
  n1<-span$n1

  if (type=="ALL") {x<-"All"}
  else if (type=="FROM") {x<-paste("From",d0, sep=" ")}
  else if (type=="To") {x<-paste("Until",d1, sep=" ")}
  else if (type=="BETWEEN") {x<-paste(d0,d1,sep=" - ")}
  else if (type=="FIRST") {x<-paste("All but first",n0,"periods", sep=" ")}
  else if (type=="LAST") {x<-paste("All but last",n1,"periods", sep=" ")}
  else if (type=="EXCLUDING") {x<-paste("All but first",n0,"periods and last",n1,"periods", sep=" ")}
  else {x<- "Undefined"}

  print(x)
}

