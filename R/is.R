#' Is character vector with number of characters >= 1?
#'
#' @param x R object.
#' @return Logical.
#'
#' @keywords internal
isTruthyChr <- function(x) {
  if (is.character(x) && length(x) == 1L && nchar(x) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is JSON a (non-empty) array(-like)?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isArray <- function(json) grepl('^\\[.+\\]$', json, perl=TRUE)

#' Is JSON a (non-empty) object(-like)?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isObject <- function(json) grepl('^\\{.+\\}$', json, perl=TRUE)

#' Is JSON a (non-empty) array(-like) or object(-like)?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isStruct <- function(json) isArray(json) || isObject(json)
