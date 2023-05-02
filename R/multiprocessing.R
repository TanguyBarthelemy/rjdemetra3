#' @include saitem.R
NULL

#' @rdname count
#' @export
.jmp_sa_count<-function(jmp){
  return (.jcall(jmp, "I", "size"))
}

#' Get the name of a multiprocessing or a SaItem
#'
#' Functions to retrieve the name of a multiprocessing (`.jmp_name()`) or SaItem (`.jsa_name()`).
#'
#' @param jmp,jsa the object to retrieve the name from.
#'
#' @export
.jmp_name<-function(jmp){
  return (.jcall(jmp, "S", "getName"))
}


#' @name .jws_multiprocessing
#' @export
.jmp_sa<-function(jmp, idx){
  if (idx < 1) return (NULL)
  return (.jcall(jmp, "Ljdplus/sa/base/api/SaItem;", "get", as.integer(idx-1)))
}


#' @name load_workspace
#' @export
.jmp_load<-function(jmp){
  n<-.jcall(jmp, "I", "size")
  if (n == 0){ return (NULL)}
  all<-lapply(1:n, function(i){.jsa_read(.jmp_sa(jmp, i))})
  names<-lapply(1:n, function(i){.jsa_name(.jmp_sa(jmp, i))})
  names(all)<-names
  return (all)
}

#' Add SAItem to Multiprocessing
#'
#' @param jmp the multiprocessing.
#' @param name the name of SAItem.
#' @param x either a seasonal adjustment model (from [rjd3x13::x13()] or [rjd3tramoseats::tramoseats()]) or a `"ts"` object.
#' @param spec the specification to use when `x` is a `"ts"` object.
#' @param ... other unused parameters.
#'
#' @examples
#' dir <- tempdir()
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' jws <- .jws_new()
#' jmp1 <- .jws_multiprocessing_new(jws, "sa1")
#' add_sa_item(jmp1, name = "x13", x = rjd3x13::x13(y))
#' add_sa_item(jmp1, name = "tramo", x = rjd3tramoseats::tramoseats(y))
#' add_sa_item(jmp1, name = "x13-2", x = y, rjd3x13::spec_x13())
#' add_sa_item(jmp1, name = "tramo-2", x = y, rjd3tramoseats::spec_tramoseats())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#' @export
add_sa_item <- function(jmp, name, x, spec, ...){
  UseMethod("add_sa_item", x)
}
#'@export
add_sa_item.ts <- function(jmp, name, x, spec, ...) {
  jts <- rjd3toolkit::.r2jd_ts(x)
  if (inherits(spec, "JD3_X13_SPEC")) {
    jspec <- rjd3x13::.r2jd_spec_x13(spec)
  } else if (inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
    jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(spec)
  } else {
    stop("wrong type of spec")
  }
  .jcall(jmp, "V", "add",
         name,
         jts,
         .jcast(jspec, "jdplus/sa/base/api/SaSpecification"))
}
#'@export
add_sa_item.default <- function(jmp, name, x, spec, ...) {
  if (inherits(x, "JD3_X13_OUTPUT")) {
    y <- x$result$preadjust$a1
  } else if (inherits(x, "JD3_TRAMOSEATS_OUTPUT")) {
    y <- x$result$final$series$data
  } else {
    stop("wrong type of spec")
  }
  spec <- x$estimation_spec
  add_sa_item.ts(jmp = jmp,
              x = y,
              spec = spec,
              name = name,
              ...)
}
