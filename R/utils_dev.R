# boxjson utils

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

#' Strips an array's outer brackets
#'
#' @param json JSON array.
#' @return Stripped JSON array.
#'
#' @keywords internal
stripArray <- function(json) {
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
  return(json)  # serve
}

#' Does what its name says
#'
#' @param string character. Input string to \code{hasUnclosedChar}.
#' @param index numeric. aka 'start searching after the character at index'.
#' @return logical. See name.
#'
#' @keywords internal
charIndexIsFollowedByUnevenNumberOfOuterDoubleQuotes <- function(string, index) {
  stopifnot(isTruthyChr(string), is.numeric(index), index %% 1L == 0L)
  trail <- strsplit(string, '')[[1L]]
  countr <- 0L
  for (i in (index + 1L):length(trail)) {
    if (identical(trail[i], '"') &&
        !grepl('^\\\\+$', trail[i - 1L], perl=TRUE)) {
      countr <- countr + 1L
    }
  }
  return(countr %% 2L != 0L)
}

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
##opqt <- FALSE  # if FALSE we r not in a string
  qtct <- 0L     # if even we r not in a string
  prev <- chars[1L]
  # peep through --- THIS VERSION IS COOL
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
  ##if (chars[i] == '"' && chars[i - 1L] != '\\') qtct <- qtct + 1L
    if (chars[i] == '"' && prev != '\\') qtct <- qtct + 1L
    # if no following closing bracket || opbr == 0 !!!
    # !any(grepl("[]}]", chars[i:length(chars)]))
    if (chars[i] == char &&
        ((!any(grepl("[]}]", chars[i:length(chars)])) || opbr == 0L) &&
         qtct %% 2L == 0L)) {
      return(TRUE)
    }
    # if (chars[i] == char && (opbr == 0L && qtct %% 2L == 0L)) {
    #   return(TRUE)
    # }
    prev <- chars[i]
    # if (chars[i] == '"') {
    #   if (charIndexIsFollowedByUnevenNumberOfOuterDoubleQuotes(string, i)) {
    #     opqt <- TRUE
    #   } else {
    #     opqt <- FALSE
    #   }
    # }
    # if (chars[i] == char && (opbr == 0L && !opqt)) {  # opqt %% 2L == 0L
    #   return(TRUE)
    # }
  }
  return(FALSE)
}

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
##opqt <- FALSE   # if FALSE we r not in a string
  qtct <- 0L      # if even we r not in a string
  last.cut <- 0L  # tracks last slice index
  accu <- vector('character')
  prev <- chars[1L]
  # peep through --- NEED 2 MAKE THIS ONE WORK LIKE THE OTHER !!!
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
##### if (chars[i] == '"' && chars[i - 1L] != '\\') qtct <- qtct + 1L
    if (chars[i] == '"' && prev != '\\') qtct <- qtct + 1L
    if (chars[i] == char &&
        ((!any(grepl("[]}]", chars[i:length(chars)])) || opbr == 0L) &&
         qtct %% 2L == 0L)) {
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
    # if (chars[i] == '"') {
    #   if (charIndexIsFollowedByUnevenNumberOfOuterDoubleQuotes(string, i)) {
    #     opqt <- TRUE
    #   } else {
    #     opqt <- FALSE
    #   }
    # }
    # if (chars[i] == char && (opbr == 0L && !opqt)) {
    #   if (!keep) {
    #     accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
    #   } else {  # keep split character
    #     # get pre
    #     accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
    #     last.cut <- i - 1L
    #     # get split character
    #     accu <- append(accu, substr(string, last.cut + 1L, last.cut + 1L))
    #   }
    #   last.cut <- i
    # }
    prev <- chars[i]
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
}
