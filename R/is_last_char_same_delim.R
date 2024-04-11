#' Check if last character is the used delimiter
#'
#' @param str String to test
#' @param delim A delimiter character
#'
#' @return Logical
#'
is_last_char_same_delim <- function(str, delim=NULL){
  if (is.null(delim)) stop("Delimiter not provided for checking ")
  substr(str, nchar(str), nchar(str)) == delim
}
