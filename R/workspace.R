#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
jws.open<-function(file){
  jws<-.jcall("demetra/workspace/r/Ws", "Ldemetra/workspace/r/Ws;", "open", file)
  return (jws)
}

#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
jws.new<-function(){
  jws<-.jnew("demetra/workspace/r/Ws")
  return (jws)
}

#' Title
#'
#' @param jws
#'
#' @return
#' @export
#'
#' @examples
jws.multiprocessing.count<-function(jws){
  return (.jcall(jws, "I", "getMultiProcessingCount"))
}



#' Title
#'
#' @param jws
#' @param idx
#'
#' @return
#' @export
#'
#' @examples
jws.multiprocessing<-function(jws, idx){
  return (.jcall(jws, "Ldemetra/workspace/r/MultiProcessing;", "getMultiProcessing", as.integer(idx-1)))
}

#' Title
#'
#' @param jws
#' @param name
#'
#' @return
#' @export
#'
#' @examples
jws.multiprocessing.new<-function(jws, name){
  return (.jcall(jws, "Ldemetra/workspace/r/MultiProcessing;", "newMultiProcessing", name))
}

#' Title
#'
#' @param jws
#'
#' @return
#' @export
#'
#' @examples
jws.compute<-function(jws){
  .jcall(jws, "V", "computeAll")
}
