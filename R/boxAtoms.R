#' Box atoms in JSON
#'
#' @param json JSON string or file reference.
#' @param strict Throw an error if input is not valid JSON?
#' @return JSON string.
#'
#' @export
boxAtoms <- function(json, strict=TRUE) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # use strict
  if (strict && !jsonlite::validate(json)) stop('invalid json')
  # boxing
  if (isArray(json) && !hasUnclosedChar(json, char=',')) {
    if (hasUnclosedChar(json, ',')) {                        # case pseudo array
      return(structure(paste0('[', json, ']'), class='json'))
    } else if (!hasUnclosedChar(stripArray(json), ',')) {    # case atom array
      return(structure(json, class='json'))
    } else {                                                 # case real array
      spl <- splitOnUnclosedChar(stripArray(json), ',')
      bxd <- sapply(as.list(spl), function(s) {
        if (isStruct(s) && hasUnboxedAtom(s)) boxAtoms(s) else s
      })
      glued <- paste0('[', paste0(bxd, collapse=','), ']')
      return(structure(glued, class='json'))
    }
  } else if (isObject(json) && !hasUnclosedChar(json, char=',')) {
    # split on unclosed comma
    cpl <- splitOnUnclosedChar(stripObject(json), ',', keep=TRUE)
    # split on unclosed colon
    spl <- unlist(lapply(cpl, splitOnUnclosedChar, char=':', keep=TRUE))
    # peep through spl
    pre <- NA
    bxd <- lapply(spl, function(cur) {
      on.exit(pre <<- cur)
      if (identical(pre, ':')) {
        if (isStruct(cur) && hasUnboxedAtom(cur)) {
          boxAtoms(cur)
        } else if (!isStruct(cur)) {
          paste0('[', cur, ']')
        } else {
          cur
        }
      } else {
        cur
      }
    })
    # glue things
    glued <- paste0('{', paste0(bxd, collapse=''), '}')
    return(structure(glued, class='json'))
  } else {                        # case atom
    bxd <- paste0('[', json, ']')
    return(structure(bxd, class='json'))
  }
}
