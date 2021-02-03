#' @include utils.R jd3_rslts.R regarima_rslts.R
NULL

tramoseats_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(tramoseats.TramoSeatsResults, q)
  return (p2r_tramoseats_rslts(rq))
}

p2r_tramoseats_rslts<-function(p){

  return (structure(
    list(
      preprocessing=p2r_regarima_rslts(p$preprocessing),
      decomposition=p2r_seats_rslts(p$decomposition),
      final=p2r_sa_decomposition(p$final),
      vardecomposition=p$variance_decomposition$as.list()
    ),
    class= "JD3TRAMOSEATS"))
}



p2r_seats_rslts<-function(p){
  return (structure(
    list(
      seatsmodel=p2r_arima(p$seats_model),
      canonicaldecomposition=p2r_ucarima(p$canonical_decomposition),
      stochastics=p2r_sa_decomposition(p$stochastics, T)),
    class= "JD3SEATS"))

}
