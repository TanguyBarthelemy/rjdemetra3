stresstest<-function(){
  start_time = Sys.time()
  for (i in 1:1000){
    spec<-spec_x11_default()
    q<-.jcall("demetra/x13/r/X11", "[B", "toBuffer", spec)
    rq<-RProtoBuf::read(x13.X11Spec, q)
    nq<-RProtoBuf::serialize(rq, NULL)
    nspec<-.jcall("demetra/x13/r/X11", "Ldemetra/x11/X11Spec;", "of", nq)
  }
  end_time = Sys.time()
  print(end_time-start_time)
}

stresstest2<-function(){
  start_time = Sys.time()
  for (i in 1:1000){
    spec<-spec_x13_default("rsa5")
    q<-.jcall("demetra/x13/r/X13", "[B", "toBuffer", spec)
    rq<-RProtoBuf::read(x13.Spec, q)
    nq<-RProtoBuf::serialize(rq, NULL)
    nspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "of", nq)
  }
  end_time = Sys.time()
  print(end_time-start_time)
}


