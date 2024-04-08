#' Add a delimiter comment
#'
#' @param colwidth Width of the delimiter comment, defaults to the current RStudio option
#' @param delim A character to repeat for the delimiter line, defaults to "-"
#'
#' @export
#'
add_delimiter <- function(colwidth=rstudioapi::readRStudioPreference("margin_column", NULL), delim="-"){
  pos = rstudioapi::getActiveDocumentContext()$selection[[1]]$range$end["column"]
  if(is.null(colwidth)) stop("Code margin could not be read from RStudio, please specify explicitly with 'colwidth=...'")
  if (pos < colwidth) {
   rstudioapi::insertText(strrep(delim, colwidth + 1 - pos))
    return(message("Inserted delimiter"))
  }
}
