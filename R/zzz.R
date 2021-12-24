
.onLoad <- function(libname, pkgname) {
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")
}

