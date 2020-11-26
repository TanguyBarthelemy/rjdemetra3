jd_span <- function(jspan){

  type<-jspan$getType()$name()
  d0<-jspan$getD0()
  d1<-jspan$getD1()
  n0<-jspan$getN0()
  n1<-jspan$getN1()

  return (structure(list(type=type, d0=d0, d1=d1, n0=n0, n1=n1), class= "JD3SPAN"))
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



arimaCoef_jd2r <- function(jparams){
  if (is.jnull(jparams))
    return(NULL)
  param<-.jcastToArray(jparams)
  len <- length(param)
  if (len==0)
    return (NULL)
  param_name <- deparse(substitute(jparams))
  Type <- sapply(param, function(x) x$getType()$name())
  Value <- sapply(param, function(x) x$getValue())
  data_param <- data.frame(Type = Type, Value = Value)
  rownames(data_param) <- sprintf("%s(%i)",
                                  param_name,
                                  1:len)
  data_param
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
spec_tramo_jd2r <- function(spec = NA, context_dictionary = NULL,
                            extra_info = FALSE, freq = NA){

  # Transform
  jtransform<-spec$getTransform()
  jspan<-jtransform$getSpan()
  span<-jd_span(jspan)
  preliminary.check<-jtransform$isPreliminaryCheck()
  transform.fct<-jtransform$getFct()
  transform.function<-jtransform$getFunction()$name()

  # Estimate
  jestimate<-spec$getEstimate()
  jspan <- jestimate$getSpan()
  estimate.span<-jd_span(jspan)
  estimate.tol <-jestimate$getTol()
  estimate.eml <-jestimate$isMaximumLikelihood()
  estimate.urfinal <-jestimate$getUbp()

  #ARIMA / Auto-modelling

  jarima<-spec$getArima()

  arima.p <-jarima$getP()
  arima.d <-jarima$getD()
  arima.q <-jarima$getQ()
  arima.bp <-jarima$getBp()
  arima.bd <-jarima$getBd()
  arima.bq <-jarima$getBq()
  arima.coef <- FALSE
  arima.coef.spec <- NA
  if (! jarima$isUndefined()){
    arima.coef <- TRUE
    phi <- jarima$getPhi()
    bphi <- jarima$getBPhi()
    theta <- jarima$getTheta()
    btheta <- jarima$getBTheta()
    arima_coefficients_spec <-
      rbind(arimaCoef_jd2r(phi),
            arimaCoef_jd2r(bphi),
            arimaCoef_jd2r(theta),
            arimaCoef_jd2r(btheta))
  }

  jami<-spec$getAutoModel()
  automdl.enabled <-jami$isEnabled()
  automdl.acceptdefault <-jami$isAcceptDefault()
  automdl.cancel <-jami$getCancel()
  automdl.ub1 <-jami$getUb1()
  automdl.ub2 <-jami$getUb2()
  automdl.armalimit <-jami$getTsig()
  automdl.reducecv <-jami$getPcr()
  automdl.ljungboxlimit <-jami$getPc()
  automdl.compare <-jami$isAmiCompare()

  # Regression
  jregression<-spec$getRegression()
  arima.mu <-jregression$isMean()

  #Calendar
  jcal<-jregression$getCalendar()
  jtd<-jcal$getTradingDays()
  tradingdays.option<-"None"
  tradingdays.stocktd<-0
  tradingdays.test<-F
  tradingdays.holidays<-NULL
  tradingdays.leapyear<-"None"
  tradingdays.type<-"None"
  tradingdays.mauto <-"None"
  tradingdays.pftd <-0.95
  if (jtd$isUsed()){
    if (jtd$isUserDefined()){
      tradingdays.option <- "UserDefined"
    }else if (jtd$isStockTradingDays()){
      tradingdays.option <- "Stock"
      tradingdays.stocktd<-jtd$stockTradingDays()
      tradingdays.test <-jtd$isTest()
    }else{
      if (jtd$isHolidays()){
        tradingdays.option <- "Holidays"
        jtd$getHolidays()
      }else{
        tradingdays.option <- "Default"
      }
      tradingdays.leapyear<-jtd$getLengthOfPeriodType()$name()
      tradingdays.type<-jtd$getTradingDaysType()$name()
      tradingdays.test <-jtd$isTest()
      tradingdays.mauto <-jtd$getAutomaticMethod()$name()
      tradingdays.pftd <-jtd$getProbabilityForFTest()
     }
  }

  jeaster<-jcal$getEaster()
  easter.type <- jeaster$getType()$name()
  easter.julian <-jeaster$isJulian()
  easter.duration <-jeaster$getDuration()
  easter.test <-jeaster$isTest()

  #Outlier
  joutlier<-spec$getOutliers()
  jspan <-joutlier$getSpan()

  outlier.enabled <-joutlier$isUsed()
  outlier.span<-jd_span(jspan)
  outlier.ao <-joutlier$isAo()
  outlier.tc <-joutlier$isTc()
  outlier.ls <-joutlier$isLs()
  outlier.so <-joutlier$isSo()
  outlier.cv <-joutlier$getCriticalValue()
  outlier.usedefcv <-outlier.cv==0
  outlier.eml <-joutlier$isMaximumLikelihood()
  outlier.tcrate <-joutlier$getDeltaTC()
  userdef_spec<-NULL

  return(structure(list(
    basic=list(preliminary.check = preliminary.check, span=span),
    transform=list(fn = transform.function,fct = transform.fct),
    estimate=list(
        span=estimate.span,
        tol = estimate.tol,eml = estimate.eml, urfinal = estimate.urfinal),
    regression=list(userdef_spec = userdef_spec,
                    tradingdays=list( mauto = tradingdays.mauto,pftd = tradingdays.pftd,option = tradingdays.option,
                 leapyear = tradingdays.leapyear,stocktd = tradingdays.stocktd,test = tradingdays.test),
                 easter=list(type = easter.type,julian = easter.julian,duration = easter.duration,test = easter.test)),
    outlier=list(enabled = outlier.enabled,
                 span= outlier.span,
                 ao = outlier.ao, tc = outlier.tc,ls = outlier.ls,so = outlier.so,usedefcv = outlier.usedefcv,
                 cv = outlier.cv,eml = outlier.eml,tcrate = outlier.tcrate),
    automdl=list(enabled = automdl.enabled, acceptdefault = automdl.acceptdefault,cancel = automdl.cancel,ub1 = automdl.ub1,
                 ub2 = automdl.ub2,armalimit = automdl.armalimit,reducecv = automdl.reducecv,
                 ljungboxlimit = automdl.ljungboxlimit,compare = automdl.compare),
    arima=list(mu = arima.mu,p = arima.p, d = arima.d,q = arima.q,bp = arima.bp,bd = arima.bd,bq = arima.bq,
              coef = arima.coef,coef.spec = arima.coef.spec)), class="JD3TRAMOSPEC"))

}
