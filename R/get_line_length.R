#' Get line length
#'
#' @param context Object of class 'document_context'
#' @param prim_selection A list, derived from context with rstudioapi::primary_selection()
#' @param line.number A line number castable to integer
#' @param verbose Print messages
#' @return Line lengt (int)
#'
get_line_length <- function(context=NULL, prim_selection=NULL, line.number=NULL, verbose=TRUE){
  if(is.null(context)) context <- rstudioapi::getActiveDocumentContext()
  if(is.null(prim_selection)) prim_selection <- rstudioapi::primary_selection(context)
  if(is.null(line.number)){
    n =  nchar(context$contents[[prim_selection$range$start["row"]]])
    if (verbose) message(paste0("Line #", prim_selection$range$start["row"], " contains ", n, " characters "))
    return(n)
  } else{
    if(is.na(supressWarnings(as.integer(line.number)))) stop("Please provide a readable line number! ")
    if(as.integer(line.number)>length(context$contents)) stop(paste0("Line not found, document contains only ", length(context$contents), " lines..."))
    return(length(context$contents[[as.integer(line.number)]]))
  }
}

