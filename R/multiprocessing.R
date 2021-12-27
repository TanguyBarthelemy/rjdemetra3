#' Title
#'
#' @param jmp
#'
#' @return
#' @export
#'
#' @examples
jmp.sa.count<-function(jmp){
  return (.jcall(jmp, "I", "size"))
}

#' Title
#'
#' @param jmp
#'
#' @return
#' @export
#'
#' @examples
jmp.name<-function(jmp){
  return (.jcall(jmp, "S", "getName"))
}


#' Title
#'
#' @param jmp
#' @param idx
#'
#' @return
#' @export
#'
#' @examples
jmp.sa<-function(jmp, idx){
  return (.jcall(jmp, "Ldemetra/sa/SaItem;", "get", as.integer(idx-1)))
}


