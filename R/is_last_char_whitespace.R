#' Check if last character is whitespace
#'
#' @param str String to test
#' @param whitespace A character vector defining whitespace
#'
#' @return Logical
#'
is_last_char_whitespace <- function(str, whitespace = c(" ", " \t")){
  return(
    substr(str, nchar(str), nchar(str)) %in% whitespace
  )
}

