# boxjson

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Mutates input JSON for safe processing
#' 
#' @param json Input JSON.
#' @return JSON string.
#' 
#' @internal
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

#' Check if JSON contains unboxed atoms
#'
#' @param json JSON string or file reference.
#' @return Logical.
#'
#' @export
hasUnboxedAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # regex                                              # boundaries
  rex <- paste0('(?:"[[:alnum:]]+"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})|',
                '(?:(?:^(?:"[[:print:]]+",*)+$)|',     # string atoms
                '(?:^(?:\\d+,*)+$)|',                  # digit atoms
                '(?:^(?:(?:null|false|true),*)+$))')   # boolean atoms
  # reason
  return(grepl(rex, json, perl=TRUE))
}

#' Box atoms in JSON
#'
#' @param json JSON string or file reference.
#' @return JSON string.
#'
#' @export
boxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # split on some boundaries
  spl <- strsplit(json, 
                  paste0('(?<=[[:print:]]"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})|', 
                         '(?<=,)(?="[[:print:]]+"\\:)'), 
                  perl=TRUE)[[1L]]
  # peep through
  boxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
    if (!is.null(pre) &&
        grepl('"[[:print:]]+"\\:$', pre, perl=TRUE) &&
        !grepl('\\[[^\\[]*\\],?', cur, perl=TRUE)) {
      # box atomic data
      sub('([^,\\}]*)(,)?', '[\\1]\\2', cur, perl=TRUE)
    } else if (is.null(pre) && is.null(nxt) && 
               grepl('^(?:"*[[:print:]]+"*,*)+$', cur, perl=TRUE) &&
               !grepl('^\\{.*\\}$', cur, perl=TRUE)) {
      paste0('[', cur, ']')
    } else {
      cur
    }
  })
  # glue pieces to one
  glued <- paste0(unlist(boxd), collapse='')
  # serve
  return(structure(glued, class='json'))
}

#' Unbox atoms in JSON
#'
#' @param json JSON string or file reference.
#' @return JSON string.
#'
#' @export
unboxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # check if input json is an array of length 1
  if (grepl('^\\[.*\\]$', json, perl=TRUE)  && 
      !grepl(',(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    unboxd <- gsub('^\\[|\\]$', '', json, perl=TRUE)
    return(structure(unboxd, class='json'))  # early exit
  }
  # split on some boundaries
  spl <- strsplit(json, 
                  paste0('(?<=[[:print:]]"\\:)(?=\\[[^\\[,]*\\])|', 
                         '(?<=,)(?="[[:print:]]+"\\:)'), 
                  perl=TRUE)[[1L]]
  # peep through
  unboxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
    if (!is.null(pre) &&
        grepl('"[[:print:]]+"\\:$', pre, perl=TRUE) &&
        grepl('\\[[^\\[,]*\\],?', cur, perl=TRUE)) {
      # unbox atomic data
      gsub('[\\[\\]]', '', cur, perl=TRUE)
    } else {
      cur
    }
  })
  # glue pieces to one
  glued <- paste0(unlist(unboxd), collapse='')
  # serve
  return(structure(glued, class='json'))
}
