#' @include saitem.R
NULL

#' @rdname count
#' @export
.jmp.sa.count<-function(jmp){
  return (.jcall(jmp, "I", "size"))
}

#' Get the name of a multiprocessing or a SaItem
#'
#' Functions to retrieve the name of a multiprocessing (`jmp.name()`) or SaItem (`jsa.name()`).
#'
#' @param jmp,jsa the object to retrieve the name from.
#'
#' @export
.jmp.name<-function(jmp){
  return (.jcall(jmp, "S", "getName"))
}


#' @name jws.multiprocessing
#' @export
.jmp.sa<-function(jmp, idx){
  if (idx < 1) return (NULL)
  return (.jcall(jmp, "Ldemetra/sa/SaItem;", "get", as.integer(idx-1)))
}


#' @name jws.load
#' @export
.jmp.load<-function(jmp){
  n<-.jcall(jmp, "I", "size")
  if (n == 0){ return (NULL)}
  all<-lapply(1:n, function(i){.jsa.read(.jmp.sa(jmp, i))})
  names<-lapply(1:n, function(i){.jsa.name(.jmp.sa(jmp, i))})
  names(all)<-names
  return (all)
}

