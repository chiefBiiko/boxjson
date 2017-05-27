# jsonbox

source('https://github.com/chiefBiiko/octostep/raw/master/R/octostep.R')

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

hasUnboxedAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('(?:"[[:alnum:]]"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})', 
               json, perl=TRUE))
}

boxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  # split on some boundaries
  spl <- strsplit(json, 
                  paste0('(?<="[[:alnum:]]"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})|', 
                         '(?<=,)(?="[[:alnum:]]+"\\:)'), 
                  perl=TRUE)[[1]]
  # peep through
  boxd <- octostep(as.list(spl), function(pre, cur, nxt) {
    if (!is.null(pre) &&
        grepl('"[[:alnum:]]+"\\:$', pre, perl=TRUE) &&
        !grepl('\\[[^\\[]*\\],?', cur, perl=TRUE)) {
      # box atomic data
      sub('([^,]*)(,)?', '[\\1]\\2', cur, perl=TRUE)
    } else {
      cur
    }
  })
  # glue pieces to one
  glued <- paste0(unlist(boxd), collapse='')
  # serve
  return(structure(glued, class='json'))
}

unboxAtoms <- function() {
  
}