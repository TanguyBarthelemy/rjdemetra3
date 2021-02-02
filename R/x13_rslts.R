#' @include utils.R jd3_rslts.R
NULL


x13_rslts<-function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(x13.X13Results, q)
  return (p2r_x13_rslts(rq))
}

x11_rslts<-function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(x13.X11Results, q)
  return (p2r_x11_rslts(rq))
}

p2r_x13_rslts<-function(p){

  return (structure(
    list(
      preprocessing=p2r_regarima_rslts(p$preprocessing),
      decomposition=p2r_x11_rslts(p$decomposition),
      final=p2r_x13_final(p$final),
      mstats=list(
        m1=p$mstatistics$m1,
        m2=p$mstatistics$m2,
        m3=p$mstatistics$m3,
        m4=p$mstatistics$m4,
        m5=p$mstatistics$m5,
        m6=p$mstatistics$m6,
        m7=p$mstatistics$m7,
        m8=p$mstatistics$m8,
        m9=p$mstatistics$m9,
        m10=p$mstatistics$m10,
        m11=p$mstatistics$m11,
        q=p$mstatistics$q,
        qm2=p$mstatistics$qm2
      ))
    ,
    class= "JD3X13"))
}



p2r_x11_rslts<-function(p){
  return (structure(
    list(
      d1=p2r_ts(p$d1),
      d2=p2r_ts(p$d2),
      d4=p2r_ts(p$d4),
      d5=p2r_ts(p$d5),
      d6=p2r_ts(p$d6),
      d7=p2r_ts(p$d7),
      d8=p2r_ts(p$d8),
      d9=p2r_ts(p$d9),
      d10=p2r_ts(p$d10),
      d11=p2r_ts(p$d11),
      d12=p2r_ts(p$d12),
      d13=p2r_ts(p$d13),
      final_henderson=p$final_henderson_filter
    ),
    class= "JD3X11"))
}


p2r_x13_final<-function(p){
  return (structure(
    list(
      d10final=p2r_ts(p$d10final),
      d11final=p2r_ts(p$d11final),
      d12final=p2r_ts(p$d12final),
      d13final=p2r_ts(p$d13final),
      d16=p2r_ts(p$d16),
      d18=p2r_ts(p$d18),
      d10a=p2r_ts(p$d10a),
      d11a=p2r_ts(p$d11a),
      d12a=p2r_ts(p$d12a),
      d16a=p2r_ts(p$d16a),
      d18a=p2r_ts(p$d18a)
    ),
    class= "JD3X11"))
}

