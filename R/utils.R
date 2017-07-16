# boxjson utils

#' Is character vector with number of characters >= 1?
#'
#' @param x R object.
#' @return Logical.
#'
#' @keywords internal
isTruthyChr <- function(x) {
  if (is.character(x) && nchar(x) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is JSON an array?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isArray <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\[.+\\]$', json, perl=TRUE))
}

#' Is JSON an object?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isObject <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\{.+\\}$', json, perl=TRUE))
}

#' Is JSON an array or object?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isStruct <- function(json) {
  stopifnot(isTruthyChr(json))
  return(isArray(json) || isObject(json))
}

#' Strips an array's outer brackets
#'
#' @param json JSON array.
#' @return Stripped JSON array.
#'
#' @keywords internal
stripArray <- function(json) {
  stopifnot(isTruthyChr(json))
  if (isArray(json)) {
    return(gsub('^\\[|\\]$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}

#' Strips an object's outer brackets
#'
#' @param json JSON object.
#' @return Stripped JSON object.
#'
#' @keywords internal
stripObject <- function(json) {
  stopifnot(isTruthyChr(json))
  if (isObject(json)) {
    return(gsub('^\\{|\\}$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}

#' Mutates input JSON for safe processing
#'
#' @param json Input JSON.
#' @return JSON string.
#'
#' @keywords internal
mutateInputJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  # allow file references
  if (file.exists(json)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  # quoting
  json <- gsub('\'', '"', json, fixed=TRUE)
  return(json)  # serve
}

#' Does a string contain a character neither enclosed in brackets nor
#' double quotes?
#'
#' @param string Character vector of length 1L.
#' @param character Single character to search for.
#' @return Logical.
#'
#' @keywords internal
hasUnclosedChar <- function(string, char) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L)
  # split to single characters
  chars <- strsplit(string, '')[[1L]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  # peep through
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char &&
        (opbr == 0L && opqt %% 2L == 0L  &&
         all(unlist(nsqt) %% 2L == 0L))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Splits a string on given character neither enclosed in brackets nor
#' double quotes
#'
#' @param string Character vector of length 1L.
#' @param character Single character to split on.
#' @return Chr vector.
#'
#' @keywords internal
splitOnUnclosedChar <- function(string, char, keep=FALSE) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L,
            is.logical(keep))
  # split to single characters
  chars <- strsplit(string, '', fixed=TRUE)[[1]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  last.cut <- 0L    # tracks last slice index
  accu <- vector('character')
  # peep through
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char &&
        (opbr == 0L && opqt %% 2L == 0L  &&
         all(unlist(nsqt) %% 2L == 0L))) {
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
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
}
