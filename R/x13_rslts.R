#' @include utils.R jd3_rslts.R x13_fastspec.R
NULL

X11_RSLTS<-c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b13", "b17", "b20",
                 "c1", "c2", "c4", "c5", "c6", "c7", "c9", "c10", "c11", "c13", "c17", "c20",
                 "d1", "d2", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13")


R_START <- 1; R_N <- R_START + 3; R_MODE <- R_N+1; R_SFILTER <- R_MODE + 1; R_HEND <- R_SFILTER + 12;
IC_RATIO <- R_SFILTER + 1; D9_DEF <- IC_RATIO + 1; D9_FILTER <- D9_DEF + 1;
D9_PTR_MSR <- D9_FILTER + 1; X11_PTR_TABLES <- D9_PTR_MSR + 1;
X11_INFO_SIZE <- 32; X11_TABLES_SIZE <- 38;

#' Title
#'
#' @param jrslts
#'
#' @return
#' @export
#'
#' @examples
x11_rslts<-function(jrslts){

  jbuffer<-.jcall(jrslts, "Ldemetra/x13/r/X11ResultsBuffer;", "buffer")
  buffer<-.jcall(jbuffer, "[D", "data")
  p<-buffer[R_START]
  start<-c(buffer[R_START+1], buffer[R_START+2]+1)
  n<-buffer[R_N]
  mode<-X11_MODE[buffer[R_MODE]+1]
  filters<-buffer[R_SFILTER:(R_SFILTER+p-1)]
  nf<-match(0, filters)-1
  sfilters<-sapply(filters[1:nf], function(z){X11_FILTER[z]})
  henderson<-buffer[R_HEND]
  icratio<-buffer[IC_RATIO]
  nt<-n*X11_TABLES_SIZE
  tstart<-buffer[X11_PTR_TABLES]+1
  mdata<-matrix(data=buffer[tstart:(tstart+nt-1)], ncol = X11_TABLES_SIZE, nrow = n)
  tables<-ts(data=mdata, frequency = p, start = start)
  colnames(tables)<-X11_RSLTS
  d9filter<-X11_FILTER[buffer[D9_FILTER]]

  return (list(mode=mode, sfilters=sfilters, henderson=henderson, icratio=icratio, d9filter=d9filter, tables=tables))
}
