#' Splits a string on given character neither enclosed in brackets nor
#' double quotes
#'
#' @param string Character vector of length 1L.
#' @param character Single character to split on. Cannot be any of
#' \code{[]{}"}.
#' @return character. Vector of splits.
#'
#' @keywords internal
splitOnUnclosedChar <- function(string, char, keep=FALSE) {
  stopifnot(isTruthyChr(string), isTruthyChr(char), nchar(char) == 1L,
            is.logical(keep))
  # split to single characters
  chars <- strsplit(string, '', fixed=TRUE)[[1L]]
  # setup
  opbr <- 0L      # if opbr is zero we r not in a struct
  qtct <- 0L      # if even we r not in a string
  last.cut <- 0L  # tracks last slice index
  accu <- vector('character')
  prev <- chars[1L]
  # peep through --- NEED 2 MAKE THIS ONE WORK LIKE THE OTHER !!!
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"' && prev != '\\') qtct <- qtct + 1L
    if (chars[i] == char && (opbr == 0L && qtct %% 2L == 0L)) {
      if (!keep) {
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
      } else {  # keep split character
        # get pre
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
        last.cut <- i - 1L
        # get split character
        accu <- append(accu, substr(string, last.cut + 1L, last.cut + 1L))
      }
      last.cut <- i
    }
    prev <- chars[i]
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
}
