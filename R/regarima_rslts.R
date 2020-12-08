#' @include utils.R jd3_rslts.R
NULL

arma_names <- paste0("sarima.",c("p","d","q","bp","bd","bq"))

#' Title
#'
#' @param jrslts
#'
#' @return
#' @export
#'
#' @examples
regarima_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return(structure(list(arma = NULL,
                arima.coefficients = NULL,
                regression.coefficients = NULL,
                loglik = NULL,
                model = NULL,
                residuals = NULL,
                residuals.stat = NULL,
                forecast = NULL), class="JD3REGARIMA"))
  return (structure(list(
    arima=jd2r_rslts_arima(jrslts),
    regression=jd2r_rslts_regression(jrslts),
    likelihood=jd2r_rslts_likelihood(jrslts)
  ), class="JD3REGARIMA"))
}

jd2r_rslts_arima<-function(jrslts){
  # ARIMA model
  arima.orders <- sapply(arma_names,
                 function(diag) {
                   proc_int(jrslts, diag)})

  arima.est <- proc_vector(jrslts,"sarima.parameters")

  # ARIMA coefficients
  if (!is.null(arima.est)){

    pcov <- proc_matrix(jrslts,"max.pcovar")
    arima.se  <- sqrt(diag(pcov))
    arima.tstat <- arima.est/arima.se
    arima.tstat[arima.se==0] <- NA
    arima.coefficients <- cbind(arima.est,arima.se,arima.tstat)

    arima.description <- c()

    if (arima.orders[1]!=0){
      arima.description <- c(arima.description,paste0("Phi(",1:arima.orders[1],")"))
    }
    if (arima.orders[4]!=0){
      arima.description <- c(arima.description, paste0("BPhi(",1:arima.orders[4],")"))
    }
    if (arima.orders[3]!=0){
      arima.description <- c(arima.description, paste0("Theta(",1:arima.orders[3],")"))
    }
    if (arima.orders[6]!=0){
      arima.description <- c(arima.description, paste0("BTheta(",1:arima.orders[6],")"))
    }
    rownames(arima.coefficients) <- arima.description
    colnames(arima.coefficients) <- c("Estimate","Std. Error","T-stat")
  }else{
    arima.coefficients = NULL
  }

  return (list(orders=arima.orders, coefficients=arima.coefficients))

}

jd2r_rslts_likelihood<-function(jrslts){
  scores<-proc_vector(jrslts,"max.pscore")
  np<-proc_int(jrslts, "likelihood.nparams")
  nobs<-proc_int(jrslts, "likelihood.nobs")
  neff<-proc_int(jrslts, "likelihood.neffective")
  ll<-proc_numeric(jrslts, "likelihood.ll")
  adjll<-proc_numeric(jrslts, "likelihood.adjustedll")
  aic<-proc_numeric(jrslts, "likelihood.aic")
  aicc<-proc_numeric(jrslts, "likelihood.aicc")
  bic<-proc_numeric(jrslts, "likelihood.bic")
  bicc<-proc_numeric(jrslts, "likelihood.bicc")

  return (list(nparams=np, nobs=nobs, neffective=neff, loglikelihood=ll, adjustedll=adjll, aic=aic,
               aicc=aicc, bic=bic, bicc=bicc, scores=scores))
}

jd2r_rslts_regression<-function(jrslts){
  log<-proc_bool(jrslts, "log")
  adjust<-proc_bool(jrslts, "adjust")
  coef<-proc_vector(jrslts,"regression.coefficients")
  coefdesc<-proc_vector(jrslts,"regression.description")
  covar<-proc_matrix(jrslts, "regression.covar")
  return (list(log=log, adjust=adjust, coef=coef, coefdesc=coefdesc, covar=covar))
}

