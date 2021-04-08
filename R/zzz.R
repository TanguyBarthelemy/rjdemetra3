
.onLoad <- function(libname, pkgname) {
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

}

