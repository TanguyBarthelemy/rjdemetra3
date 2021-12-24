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

