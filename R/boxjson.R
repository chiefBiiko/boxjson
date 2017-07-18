# boxjson

#' Check if JSON contains unboxed atoms
#'
#' @param json JSON string or file reference.
#' @param strict Throw an error if input is not valid JSON?
#' @return Logical.
#'
#' @export
hasUnboxedAtom <- function(json, strict=TRUE) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # use strict
  if (strict && !jsonlite::validate(json)) stop('invalid json')
  # checking
  if (isArray(json)) {
    spl <- splitOnUnclosedChar(stripArray(json), ',')
    if (length(spl) > 1L) {
      return(any(grepl('^[^\\[\\{].*[^\\]\\}]$', spl, perl=TRUE)))
    } else if (length(spl) == 1L) {
      return(FALSE)
    }
  } else if (isObject(json)) {
    cpl <- splitOnUnclosedChar(stripObject(json), ',')  # split on unclosed comma
    # split on unclosed colon
    spl <- unlist(lapply(cpl, splitOnUnclosedChar, char=':', keep=TRUE))
    # peep through spl
    pre <- NA
    for (chunk in spl) {
      if (identical(pre, ':')) {  # obj vals
        if (isStruct(chunk)) return(hasUnboxedAtom(chunk)) else return(TRUE)
      }
      pre <- chunk
    }
  } else {
    return(TRUE)
  }
}

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

#' Unbox atoms in JSON
#'
#' @param json JSON string or file reference.
#' @param strict Throw an error if input is not valid JSON?
#' @return JSON string.
#'
#' @export
unboxAtoms <- function(json, strict=TRUE) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # use strict
  if (strict && !jsonlite::validate(json)) stop('invalid json')
  # check if input json is an array of length 1 else ...
  if (isArray(json)  && !hasUnclosedChar(stripArray(json), ',')) {
    return(structure(stripArray(json), class='json'))
  } else if (isArray(json) &&
             any(grepl('^[\\[\\{].*[\\]\\}]$',  # ... has any boxed atoms?
                       splitOnUnclosedChar(stripArray(json), ','),
                       perl=TRUE))) {
    # then unbox boxed atoms in array
    spl <- splitOnUnclosedChar(stripArray(json), ',', keep=TRUE)
    unboxd <- paste0('[',
                     paste0(gsub('^\\[(.*)\\]$', '\\1', spl, perl=TRUE),
                            collapse=''),
                     ']')
    return(structure(unboxd, class='json'))
  } else if (isObject(json)) {
    # split on unclosed comma
    cpl <- splitOnUnclosedChar(stripObject(json), ',', keep=TRUE)
    # split on unclosed colon
    spl <- unlist(lapply(as.list(cpl), splitOnUnclosedChar, char=':', keep=TRUE))
    # peep through
    pre <- NA
    unbxd <- lapply(as.list(spl), function(cur) {
      on.exit(pre <<- cur)
      if (identical(pre, ':') && isStruct(cur)) unboxAtoms(cur) else cur
    })
    # reglue object components
    glued <- paste0('{', paste0(unbxd, collapse=''), '}')
    # serve
    return(structure(glued, class='json'))
  } else  {
    return(structure(json, class='json'))
  }
}
