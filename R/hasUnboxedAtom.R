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
    return(FALSE)
  } else if (isObject(json)) {
    cpl <- splitOnUnclosedChar(stripObject(json), ',')  # split on unclosed comma
    # split on unclosed colon
    spl <- unlist(lapply(cpl, splitOnUnclosedChar, char=':', keep=TRUE))
    # peep through spl
    pre <- NA
    for (chunk in spl) {
      if (identical(pre, ':') && (!isStruct(chunk) || hasUnboxedAtom(chunk))) {
        return(TRUE)
      }
      pre <- chunk
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
