# jsonbox

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check if JSON contains unboxed atoms
#'
#' @param json JSON string or file reference.
#' @return Logical.
#'
#' @export
hasUnboxedAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input
  if (file.exists(json)) {  # allow file references
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  return(grepl('(?:"[[:alnum:]]+"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})', 
               json, perl=TRUE))
}

#' Box atoms in JSON
#'
#' @param json JSON string or file reference.
#' @return JSON string.
#'
#' @export
boxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input
  if (file.exists(json)) {  # allow file references
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  # split on some boundaries
  spl <- strsplit(json, 
                  paste0('(?<=[[:alnum:]]"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})|', 
                         '(?<=,)(?="[[:alnum:]]+"\\:)'), 
                  perl=TRUE)[[1]]
  # peep through
  boxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
    if (!is.null(pre) &&
        grepl('"[[:alnum:]]+"\\:$', pre, perl=TRUE) &&
        !grepl('\\[[^\\[]*\\],?', cur, perl=TRUE)) {
      # box atomic data
      sub('([^,\\}]*)(,)?', '[\\1]\\2', cur, perl=TRUE)
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
  # mutate input
  if (file.exists(json)) {  # allow file references
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  # split on some boundaries
  spl <- strsplit(json, 
                  paste0('(?<=[[:alnum:]]"\\:)(?=\\[[^\\[,]*\\])|', 
                         '(?<=,)(?="[[:alnum:]]+"\\:)'), 
                  perl=TRUE)[[1]]
  # peep through
  unboxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
    if (!is.null(pre) &&
        grepl('"[[:alnum:]]+"\\:$', pre, perl=TRUE) &&
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
