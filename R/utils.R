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

  x <- if (type=="All") {"All"} else if (type=="From") {paste("From",d0, sep=" ")}
  else if (type=="To") {paste("Until",d1, sep=" ")}
  else if (type=="Between") {paste(d0,d1,sep=" - ")}
  else if (type=="First") {paste("All but first",n0,"periods", sep=" ")}
  else if (type=="Last") {paste("All but last",n1,"periods", sep=" ")}
  else if (type=="Excluding") {paste("All but first",n0,"periods and last",n1,"periods", sep=" ")}

  print(x)
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
