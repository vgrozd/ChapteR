#' Add a delimiter comment
#'
#' @param colwidth Width of the delimiter comment, defaults to the current RStudio option
#' @param delim A character to repeat for the delimiter line, defaults to "-"
#' @param verbose Logical, print messages?
#'
#' @export
#'
add_delimiter <- function(colwidth=rstudioapi::readRStudioPreference("margin_column", NULL), delim="-", verbose=FALSE){
  context = rstudioapi::getActiveDocumentContext()
  prim_selection <- rstudioapi::primary_selection(context)
  pos = context$selection[[1]]$range$end["column"]
  n=get_line_length(context=context, prim_selection=prim_selection, verbose=FALSE)
  if(is.null(colwidth)) stop("Code margin could not be read from RStudio, please specify explicitly with 'colwidth=...'")
  if(pos<=n) rstudioapi::setCursorPosition(rstudioapi::document_position(
    row=context$selection[[1]]$range$end["row"],
    column=n+1
  ))
  if (n < colwidth) {
    if(
      !is_last_char_whitespace(context$contents[[prim_selection$range$start["row"]]]) &
        !is_last_char_same_delim(context$contents[[prim_selection$range$start["row"]]], delim)
      ){
      rstudioapi::insertText(" ")
      n=n+1
    }
    rstudioapi::insertText(strrep(delim, colwidth - n))
    rstudioapi::insertText("\n\n")
    if(verbose) return(message("Inserted delimiter")) else return()
  }else{
    if(
      !is_last_char_whitespace(context$contents[[prim_selection$range$start["row"]]]) &
        !is_last_char_same_delim(context$contents[[prim_selection$range$start["row"]]], delim)
      ){
      rstudioapi::insertText(" ")
      n=n+1
    }
    rstudioapi::insertText(strrep(delim, 4))
    rstudioapi::insertText("\n\n")
    if(verbose) return(message("Line longer than column width, minimal code folding delimiter inserted!")) else return()
  }
}
