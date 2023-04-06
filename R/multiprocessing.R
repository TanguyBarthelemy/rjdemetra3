#' @include saitem.R
NULL

#' @rdname count
#' @export
.jmp_sa_count<-function(jmp){
  return (.jcall(jmp, "I", "size"))
}

#' Get the name of a multiprocessing or a SaItem
#'
#' Functions to retrieve the name of a multiprocessing (`jmp_name()`) or SaItem (`jsa_name()`).
#'
#' @param jmp,jsa the object to retrieve the name from.
#'
#' @export
.jmp_name<-function(jmp){
  return (.jcall(jmp, "S", "getName"))
}


#' @rdname jmp_sa
#' @export
.jmp_sa<-function(jmp, idx){
  if (idx < 1) return (NULL)
  return (.jcall(jmp, "Ljdplus/sa/base/api/SaItem;", "get", as.integer(idx-1)))
}


#' @name jmp_load
#' @export
.jmp_load<-function(jmp){
  n<-.jcall(jmp, "I", "size")
  if (n == 0){ return (NULL)}
  all<-lapply(1:n, function(i){.jsa_read(.jmp_sa(jmp, i))})
  names<-lapply(1:n, function(i){.jsa_name(.jmp_sa(jmp, i))})
  names(all)<-names
  return (all)
}

