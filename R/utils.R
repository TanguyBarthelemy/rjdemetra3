#' @include common_spec.R
NULL

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

p2r_likelihood<-function(p){
  return (structure(list(nobs=p$nobs, neffectiveobs=p$neffectiveobs, nparams=p$nparams,
               ll=p$log_likelihood, adjll=p$adjusted_log_likelihood,
               aic=p$aic, aicc=p$aicc, bic=p$bic, bicc=p$bicc, ssq=p$ssq),
               class = "JD3LIKELIHOOD"))
}

p2r_matrix<-function(p){
  m<-matrix(data=p$values, nrow = p$nrows, ncol = p$ncols)
  `attr<-`(m, "name", p$name)
  return (m)
}

p2r_ts<-function(p){
  if (length(p$values) == 0)
    return (NULL)
  s<-ts(data=p$values, frequency = p$annual_frequency, start = c(p$start_year, p$start_period))
  `attr<-`(s, "name", p$name)
  return (s)
}

p2r_test<-function(p){
  return (list(value=p$value, pvalue=p$pvalue, description=p$description))
}

p2r_parameters_rslt<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  return (data.frame(value=value, type=type))
}

p2r_parameters_rsltx<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  description<-sapply(p, function(z){z$description})

  rslt<-data.frame(value=value, type=type)
  row.names(rslt)<-description

  return (rslt)
}


p2r_parameters_estimation<-function(p){
  if (is.null(p))
    return (NULL)
  return (list(val=p$value, score=p$score, cov=p2r_matrix(p$covariance), description=p$description))
}


p2r_sarima<-function(p){
  return (structure(list(period=p$period, p = p$p, d=p$d, q=p$q, bp = p$bp, bd = p$bd, bq = p$bq,
               parameters=p$parameters, covariance=p2r_matrix(p$covariance), score=p$score), class= "JD3SARIMA"))
}

p2r_arima<-function(p){
  return (structure(list(name=p$name, innovationvariance=p$innovation_variance, ar=p$ar, delta=p$delta, ma=p$ma), class= "JD3ARIMA"))
}

ts_move<-function(period, freq, delta){
  if (delta == 0)return (period)
  if (freq == 1)return (c(period[1]+delta, 1))
  x<-period[1]*freq+(period[2]+delta-1)
  return (c(x %/% freq, (x %% freq)+1))
}

p2r_component<-function(p){
  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts

  val<-ts(s[(nb+1):(n-nf)], frequency = freq, start=ts_move(start, freq, nb))
  rslt<-list(data=val)
  if (nb > 0){
    bcasts<-ts(s[1:nb], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
  }
  if (nf > 0){
    fcasts<-ts(s[(n-nf+1):n], frequency = freq, start=ts_move(start, freq, n-nf))
    rslt[['fcasts']]<-fcasts
  }
  return (rslt)
}

p2r_sacomponent<-function(p){
  e<-p$stde
  if (length(e) == 0) return (p2r_component(p))

  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts
  dstart<-ts_move(start, freq, nb)
  fstart<-ts_move(start, freq, n-nf)

  idx<-(nb+1):(n-nf)
  data<-ts(s[idx], frequency = freq, dstart)
  edata<-ts(e[idx], frequency = freq, dstart)

  rslt<-list(data=data, data.stde=edata)
  if (nb > 0){
    idx<-1:nb
    bcasts<-ts(s[idx], frequency = freq, start=start)
    ebcasts<-ts(e[idx], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
    rslt[['bcasts.stde']]<-ebcasts
  }
  if (nf > 0){
    idx<-(n-nf+1):n
    fcasts<-ts(s[idx], frequency = freq, start=fstart)
    efcasts<-ts(e[idx], frequency = freq, start=fstart)
    rslt[['fcasts']]<-fcasts
    rslt[['fcasts.stde']]<-efcasts
  }

  return (rslt)
}

p2r_sa_decomposition<-function(p, full=F){
  if (full){
    return (list(mode = enum_extract(sa.DecompositionMode, p$mode),
                 series=p2r_sacomponent(p$series),
                 sa=p2r_sacomponent(p$seasonally_adjusted),
                 t=p2r_sacomponent(p$trend),
                 s=p2r_sacomponent(p$seasonal),
                 i=p2r_sacomponent(p$irregular)
    ))
  }else{
    return (list(mode = enum_extract(sa.DecompositionMode, p$mode),
                 series=p2r_component(p$series),
                 sa=p2r_component(p$seasonally_adjusted),
                 t=p2r_component(p$trend),
                 s=p2r_component(p$seasonal),
                 i=p2r_component(p$irregular)
    ))
  }
}

p2r_sa_diagnostics<-function(p){
  return (list(vardecomposition =p$variance_decomposition$as.list(),
               seas.ftest.i=p2r_test(p$seasonal_ftest_on_irregular),
               seas.ftest.sa=p2r_test(p$seasonal_ftest_on_sa),
               seas.qstest.i=p2r_test(p$seasonal_qtest_on_irregular),
               seas.qstest.sa=p2r_test(p$seasonal_qtest_on_sa),
               td.ftest.i=p2r_test(p$td_ftest_on_irregular),
               td.ftest.sa=p2r_test(p$td_ftest_on_sa)
  ))

}

p2r_ucarima<-function(p){

  return (structure
          (list(
    model=p2r_arima(p$model),
    components=lapply(p$components, function(z){p2r_arima(z)})),
    class= "JD3UCARIMA"))
}

fixedParameters<-function(coef){
  if (length(coef) == 0) return (NULL)
  return (lapply(coef, function(z){list(value=z, type="FIXED")}))
}

#' Title
#'
#' @param name
#' @param id
#' @param lag0
#' @param lag1
#' @param regeffect
#'
#' @return
#' @export
#'
#' @examples
createVariable<-function(id, name = NULL, lag0 = 0, lag1 = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {name<-id}

  return (list(id=id, name=name, lags=rlags(lag0, lag1), coef=fixedParameters(coef), regeffect=regeffect))
}

#' Title
#'
#' @param start
#' @param end
#' @param name
#' @param coef
#'
#' @return
#' @export
#'
#' @examples
createRamp<-function(start, end, name = NULL, coef=0){
  s<-parseDate(start)
  e<-parseDate(end)
  return (list(name=name, start=s, end=e, coef=fixedParameters(coef) ))
}

#' Title
#'
#' @param code
#' @param pos
#' @param name
#' @param coef
#'
#' @return
#' @export
#'
#' @examples
createOutlier<-function(code, pos, name = NULL, coef=0){
  p<-parseDate(po)
  return (list(name=name, code=code, pos=p, coef=fixedParameters(coef)))
}

