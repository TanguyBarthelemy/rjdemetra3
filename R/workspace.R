#' @include multiprocessing.R
NULL

#' Create a workspace or a multi-processing
#'
#' Functions to create a 'JDemetra+' workspace (\code{.jws_new()}) and
#' to add a new multi-processing (\code{.jws_multiprocessing_new()}).
#'
#' @param modelling_context the context (from [rjd3toolkit::modelling_context()]).
#' @param jws a workspace object.
#' @param name character name of the new multiprocessing.
#'
#' @examples
#' # To create an empty 'JDemetra+' workspace
#' wk <- .jws_new()
#' mp <- .jws_multiprocessing_new(wk, "sa1")
#'
#'
#' @name .jws_new
#' @rdname .jws_new
#' @export
.jws_new<-function(modelling_context=NULL){
    jws<-.jnew("jdplus/sa/base/workspace/Ws")
  if (! is.null(modelling_context)){
    jcontext<-rjd3toolkit::.r2jd_modellingcontext(modelling_context)
    .jcall("jdplus/sa/base/workspace/Ws", "V", "setContext", jcontext)
  }
  return (jws)
}

#' @name .jws_new
#' @export
.jws_multiprocessing_new<-function(jws, name){
  return (.jcall(jws, "Ljdplus/sa/base/workspace/MultiProcessing;", "newMultiProcessing", name))
}


#' Count the number of objects inside a workspace or multiprocessing
#'
#' Functions to count the number of multiprocessing inside a workspace (`jws_multiprocessing_count`) or
#' the number of SaItem inside a multiprocessing (`jmp_sa_count`).
#'
#' @param jws,jmp the workspace or the multiprocessing.
#'
#' @name count
#' @rdname count
#' @export
.jws_multiprocessing_count<-function(jws){
  return (.jcall(jws, "I", "getMultiProcessingCount"))
}



#' Extract a Multiprocessing or a SaItem
#'
#' @param jws,jmp the workspace or the multiprocessing.
#' @param idx index of the object to extract.
#'
#' @export
.jws_multiprocessing<-function(jws, idx){
  return (.jcall(jws, "Ljdplus/sa/base/workspace/MultiProcessing;", "getMultiProcessing", as.integer(idx-1)))
}




#' Load a 'JDemetra+' workpace
#'
#' `.jws_open()` loads a workspace and `.jws_compute()` computes it (to be able to get all the models).
#'
#' @param file the path to the 'JDemetra+' workspace to load.
#' By default a dialog box opens.
#' @param jws the workspace.
#'
#' @seealso [load_workspace()] to directly load a workspace and import all the models.
#'
#' @export
.jws_open<-function(file){
  if (missing(file) || is.null(file)) {
    if (Sys.info()[['sysname']] == "Windows") {
      file <- utils::choose.files(caption = "Select a workspace",
                                  filters = c("JDemetra+ workspace (.xml)", "*.xml"))
    }else{
      file <- base::file.choose()
    }
    if (length(file) == 0)
      stop("You have to choose a file !")
  }
  if (!file.exists(file) | length(grep("\\.xml$",file)) == 0)
    stop("The file doesn't exist or isn't a .xml file !")

  jws<-.jcall("jdplus/sa/base/workspace/Ws", "Ljdplus/sa/base/workspace/Ws;", "open", file)
  return (jws)
}

#' @name .jws_open
#' @export
.jws_compute<-function(jws){
  .jcall(jws, "V", "computeAll")
}

#' Get The context from Workspace
#'
#' @param jws the workspace.
#'
#' @export
.jws_context<-function(jws){
  .jcall(jws, "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;", "getContext")
}


#' Read all SaItems
#'
#' Functions to read all the SAItem of a multiprocessing (`jmp_load()`)
#' or a workspace (`load_workspace()`).
#'
#' @inheritParams .jws_open
#' @param jmp a multiprocessing.
#'
#' @export
load_workspace<-function(file){
  if (missing(file) || is.null(file)) {
    if (Sys.info()[['sysname']] == "Windows") {
      file <- utils::choose.files(caption = "Select a workspace",
                                  filters = c("JDemetra+ workspace (.xml)", "*.xml"))
    }else{
      file <- base::file.choose()
    }
    if (length(file) == 0)
      stop("You have to choose a file !")
  }
  if (!file.exists(file) | length(grep("\\.xml$",file)) == 0)
    stop("The file doesn't exist or isn't a .xml file !")

  jws<-.jws_open(file)
  .jws_compute(jws)
  n<-.jws_multiprocessing_count(jws)
  jmps<-lapply(1:n, function(i){.jmp_load(.jws_multiprocessing(jws,i))})
  names<-lapply(1:n, function(i){.jmp_name(.jws_multiprocessing(jws, i))})
  names(jmps)<-names
  jcntxt<-.jws_context(jws)
  cntxt<-rjd3toolkit::.jd2r_modellingcontext(jcntxt)

  return (list(processing=jmps, context=cntxt))

}

#' Save Workspace
#'
#' @param jws the workspace object to export.
#' @param file the path where to export the 'JDemetra+' workspace (.xml file).
#' @param version JDemetra+ version used for the export
#' @param replace boolean indicating if the workspace should be replaced if it already exists.
#' @examples
#' dir <- tempdir()
#' jws <- .jws_new()
#' jmp1 <- .jws_multiprocessing_new(jws, "sa1")
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' add_sa_item(jmp1, name = "x13", x = y, rjd3x13::spec_x13())
#' save_workspace(jws, file.path(dir, "workspace.xml"))
#'
#' @export
save_workspace <- function(jws, file, version = c("jd3", "jd2"), replace = FALSE) {
  version <- match.arg(tolower(version)[1], c("jd3", "jd2"))
  .jcall(jws, "Z", "saveAs", file, version, !replace)
}
