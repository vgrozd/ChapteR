#' Add a delimiter comment
#'
#' @param colwidth Width of the delimiter comment, defaults to the current RStudio option
#' @param delim A character to repeat for the delimiter line, defaults to "-"
#'
#' @export
#'
add_delimiter <- function(colwidth=rstudioapi::readRStudioPreference("margin_column", NULL), delim="-"){
  pos = rstudioapi::getActiveDocumentContext()$selection[[1]]$range$end["column"]
  n=ChapteR:::get_line_length(verbose=FALSE)
  if(is.null(colwidth)) stop("Code margin could not be read from RStudio, please specify explicitly with 'colwidth=...'")
  if(pos<=n) rstudioapi::setCursorPosition(rstudioapi::document_position(
    row=rstudioapi::getActiveDocumentContext()$selection[[1]]$range$end["row"],
    column=n+1
  ))
  if (n < colwidth) {
    rstudioapi::insertText(strrep(delim, colwidth - n))
    return(message("Inserted delimiter"))
  }else{
    rstudioapi::insertText(strrep(delim, 4))
    return(message("Line longer than column width, minimal code folding delimiter inserted!"))
  }
}




