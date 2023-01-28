#' @include multiprocessing.R
NULL

#' Create a workspace or a multi-processing
#'
#' Functions to create a 'JDemetra+' workspace (\code{jws.new()}) and
#' to add a new multi-processing (\code{jws.multiprocessing.new()}).
#'
#' @param jws a workspace object.
#' @param name character name of the new multiprocessing.
#'
#' @examples
#' # To create and export an empty 'JDemetra+' workspace
#' wk <- .jws.new()
#' mp <- .jws.multiprocessing.new(wk, "sa1")
#'
#'
#' @name jws.new
#' @rdname jws.new
#' @export
.jws.new<-function(modelling_context=NULL){
    jws<-.jnew("demetra/workspace/r/Ws")
  if (! is.null(modelling_context)){
    jcontext<-rjd3toolkit::.r2jd_modellingcontext(modelling_context)
    .jcall("demetra/workspace/r/Ws", "V", "setContext", jcontext)
  }
  return (jws)
}

#' @name jws.new
#' @rdname jws.new
#' @export
.jws.multiprocessing.new<-function(jws, name){
  return (.jcall(jws, "Ldemetra/workspace/r/MultiProcessing;", "newMultiProcessing", name))
}


#' Count the number of objects inside a workspace or multiprocessing
#'
#' Functions to count the number of multiprocessing inside a workspace (`jws.multiprocessing.count`) or
#' the number of SaItem inside a multiprocessing (`jmp.sa.count`).
#'
#' @param jws,jmp the workspace or the multiprocessing.
#'
#' @name count
#' @rdname count
#' @export
.jws.multiprocessing.count<-function(jws){
  return (.jcall(jws, "I", "getMultiProcessingCount"))
}



#' Extract a Multiprocessing or a SaItem
#'
#' @param jws,jmp the workspace or the multiprocessing.
#' @param idx index of the object to extract.
#'
#' @export
.jws.multiprocessing<-function(jws, idx){
  return (.jcall(jws, "Ldemetra/workspace/r/MultiProcessing;", "getMultiProcessing", as.integer(idx-1)))
}




#' Load a 'JDemetra+' workpace
#'
#' `.jws.open()` loads a workspace and `.jws.compute()` computes it (to be able to get all the models).
#'
#' @param file the path to the 'JDemetra+' workspace to load.
#' By default a dialog box opens.
#' @param jws the workspace.
#'
#' @seealso [.jws.load()] to directly load a workspace and import all the models.
#'
#' @export
.jws.open<-function(file){
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

  jws<-.jcall("demetra/workspace/r/Ws", "Ldemetra/workspace/r/Ws;", "open", file)
  return (jws)
}

#' @name jws.open
#' @export
.jws.compute<-function(jws){
  .jcall(jws, "V", "computeAll")
}

#' Get The context from Workspace
#'
#' @param jws the workspace.
#'
#' @export
.jws.context<-function(jws){
  .jcall(jws, "Ldemetra/timeseries/regression/ModellingContext;", "getContext")
}


#' Read all SaItems
#'
#' Functions to read all the SAItem ([jsa.read()]) of a multiprocessing (`jmp.load()`)
#' or a workspace (`.jws.load()`).
#'
#' @inheritParams jws.open
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

  jws<-.jws.open(file)
  .jws.compute(jws)
  n<-.jws.multiprocessing.count(jws)
  jmps<-lapply(1:n, function(i){.jmp.load(.jws.multiprocessing(jws,i))})
  names<-lapply(1:n, function(i){.jmp.name(.jws.multiprocessing(jws, i))})
  names(jmps)<-names
  jcntxt<-.jws.context(jws)
  cntxt<-rjd3toolkit::.jd2r_modellingcontext(jcntxt)

  return (list(processing=jmps, context=cntxt))

}
