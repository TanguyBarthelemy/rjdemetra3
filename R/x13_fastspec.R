#' @include utils.R
NULL

# X11
MAXFREQ<-12
MODE<-1
SEASONAL<-MODE + 1
FILTER<-SEASONAL + 1
LSIG<-FILTER + MAXFREQ
USIG<-LSIG + 1
HEND<-USIG + 1
NFCASTS<-HEND + 1
NBCASTS<-NFCASTS + 1
CSIG<-NBCASTS + 1
SIGV<-CSIG + 1
EXFCASTS<-SIGV + MAXFREQ
BIAS<-EXFCASTS + 1
SIZE<-BIAS

X11_MODE=c("Undefined","Additive","Multiplicative","LogAdditive","PseudoAdditive")
X11_FILTER=c("Msr","Stable","X11Default","S3X1","S3X3","S3X5","S3X9","S3X15")
X11_BIAS=c("None","Legacy","Smooth","Ratio")
X11_CALSIGMA=c("None","Signif","All","Select")

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
r2jd_fastspec_x11<-function(x){

  rspec<-rep(0, SIZE)
  rspec[MODE]<-pmatch(x$mode, X11_MODE)-1
  rspec[SEASONAL]<-if (x$seasonal) 1 else 0
  f<-sapply(x$filters, function(z){pmatch(z,X11_FILTER)})
  rspec[FILTER:(FILTER+length(f)-1)]<-f
  rspec[LSIG]<-x$sigma.lower
  rspec[USIG]<-x$sigma.upper
  rspec[HEND]<-x$henderson
  rspec[NFCASTS]<-x$nforecasts
  rspec[NBCASTS]<-x$nbackcasts
  rspec[EXFCASTS]<-if (x$excludeforecast) 1 else 0
  rspec[CSIG]<-pmatch(x$calendarsigma, X11_CALSIGMA)-1
  if (! is.null(x$sigmavec)){
    sigv<-sapply(x$sigmavec, function(z){if (z == "Group1") return(1) else if (z == "Group2") return (2)else return (0)})
    rspec[SIGV:(SIGV+length(sigv)-1)]<-sigv
  }
  rspec[BIAS]<-pmatch(x$bias, X11_BIAS)-1
  jbuffer<-.jnew("demetra/x13/r/X11Buffer", rspec)
  return (.jcall(jbuffer, "Ldemetra/x11/X11Spec;", "build"))
}

#' Title
#'
#' @param spec
#'
#' @return
#' @export
#'
#' @examples
jd2r_fastspec_x11<-function(jspec){
  jbuffer<-.jcall("demetra/x13/r/X11Buffer","Ldemetra/x13/r/X11Buffer;", "of", jspec)
  rspec<-.jcall(jbuffer, "[D", "data")
  filters<-rspec[FILTER:(FILTER+MAXFREQ-1)]
  n<-match(0, filters)-1
  filters<-sapply(filters[1:n], function(z){X11_FILTER[z]})
  vsig<-rspec[SIGV:(SIGV+MAXFREQ-1)]
  n<-match(0, vsig)-1
  if (n>0){
    sigv<-sapply(vsig[1:n], function(z){if (z == 1) return("Group1")else return ("Group2")})
  }else
    sigv<-NULL
  return (list(mode=X11_MODE[rspec[MODE]+1], seasonal=rspec[SEASONAL]!=0, filters=filters,sigma.lower=rspec[LSIG], sigma.upper=rspec[USIG],
               henderson=rspec[HEND], nforecasts=rspec[NFCASTS], nbackcasts=rspec[NBCASTS], excludeforecast=rspec[EXFCASTS]!=0,
               calendarsigma=X11_CALSIGMA[rspec[CSIG]+1], sigmavec=sigv, bias=X11_BIAS[rspec[BIAS]+1]
               ))
}

stresstest<-function(){
  start_time = Sys.time()
  for (i in 1:2000){
    spec<-spec_x11_default()
    q<-.jcall("demetra/x13/io/protobuf/X13ProtosUtility", "[B", "toBuffer", spec)
    rq<-RProtoBuf::read(x13.X11Spec, q)
    nq<-RProtoBuf::serialize(rq, NULL)
    nspec<-.jcall("demetra/x13/io/protobuf/X13ProtosUtility", "Ldemetra/x11/X11Spec;", "x11SpecOf", nq)
  }
  end_time = Sys.time()
  print(end_time-start_time)
}

stresstest2<-function(){
  start_time = Sys.time()
  for (i in 1:2000){
    spec<-spec_x11_default()
    q<-jd2r_fastspec_x11(spec)
    nspec<-r2jd_fastspec_x11(q)
  }
  end_time = Sys.time()
  print(end_time-start_time)
}

