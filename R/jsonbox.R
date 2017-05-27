# jsonbox

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @export
hasUnboxedAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('(?:"[[:alnum:]]+"\\:)(?!\\[[^\\[]*\\])(?!\\{[^\\{]*\\})', 
               json, perl=TRUE))
}

#' @export
boxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
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

#' @export
unboxAtoms <- function(json) {
  stopifnot(isTruthyChr(json))
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
