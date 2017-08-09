#' Does a string contain a character neither enclosed in brackets nor
#' double quotes?
#'
#' @param string Character vector of length 1.
#' @param character Single character to search for. Cannot be any of
#' \code{[]{}"}.
#' @return Logical.
#'
#' @keywords internal
hasUnclosedChar <- function(string, char) {
  stopifnot(isTruthyChr(string), isTruthyChr(char), nchar(char) == 1L)
  # split to single characters
  chars <- strsplit(string, '')[[1L]]
  # setup
  opbr <- 0L     # if opbr is zero we r not in a struct
  qtct <- 0L     # if even we r not in a string
  prev <- chars[1L]
  # peep through --- THIS VERSION IS COOL
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"' && prev != '\\') qtct <- qtct + 1L
    if (chars[i] == char && (opbr == 0L && qtct %% 2L == 0L)) {
      return(TRUE)
    }
    prev <- chars[i]
  }
  return(FALSE)
}
