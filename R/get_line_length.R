#' Get line length
#'
#' @param line.number A line number castable to integer
#' @param verbose Print messages
#' @return Line lengt (int)
#'
get_line_length <- function(line.number=NULL, verbose=TRUE){
  context <- rstudioapi::getActiveDocumentContext()
  prim_selection <- rstudioapi::primary_selection(context)
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
