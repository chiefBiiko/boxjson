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
                       splitOnUnclosedChar(stripArray(json), peep=','),
                       perl=TRUE))) {
    # then unbox boxed atoms in array
    spl <- splitOnUnclosedChar(stripArray(json), peep=',', keep=TRUE)
    unboxd <- paste0('[',
                     paste0(gsub('^\\[(.*)\\]$', '\\1', spl, perl=TRUE),
                            collapse=''),
                     ']')
    return(structure(unboxd, class='json'))
  } else if (isObject(json)) {
    # split on unclosed comma
    cpl <- splitOnUnclosedChar(stripObject(json), peep=',', keep=TRUE)
    # split on unclosed colon
    spl <- unlist(lapply(as.list(cpl), splitOnUnclosedChar, peep=':', keep=TRUE))
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
