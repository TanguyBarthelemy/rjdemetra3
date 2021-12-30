#' @include saitem.R
NULL


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

#' Title
#'
#' @param jmp
#'
#' @return
#' @export
#'
#' @examples
jmp.load<-function(jmp){
  n<-.jcall(jmp, "I", "size")
  all<-lapply(1:n, function(i){jsa.read(jmp.sa(jmp, i))})
  names<-lapply(1:n, function(i){jsa.name(jmp.sa(jmp, i))})
  names(all)<-names
  return (all)
}

