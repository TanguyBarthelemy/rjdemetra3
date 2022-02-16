
.onLoad <- function(libname, pkgname) {

  suppressMessages(require(rjd3tramoseats, quietly = T))
  suppressMessages(require(rjd3x13, quietly = T))

  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")
}

