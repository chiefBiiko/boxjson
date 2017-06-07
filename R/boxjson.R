# boxjson

#' Check if JSON contains unboxed atoms
#'
#' @param json JSON string or file reference.
#' @return Logical.
#'
#' @export
hasUnboxedAtom <- function(json) {
  stopifnot(isTruthyChr(json))
  # mutate input for safe processing
  json <- mutateInputJSON(json)
  # extra check 4 arrays - is json an array of length > 1?
  if (isArray(json)) {                                  # case array
    spl <- splitOnUnclosedChar(stripArray(json), ',')
    if (length(spl) > 1L) {
      return(any(grepl('^[^\\[\\{].*[^\\]\\}]$', spl, perl=TRUE)))
    } else if (length(spl) == 1L) {
      return(FALSE)
    }
  } else if (isObject(json)) {                          # case object
    cpl <- splitOnUnclosedChar(stripObject(json), ',')  # split on unclosed comma
    spl <- vector('character')                          # helper accu
    for (cp in cpl) {                                   # split on unclosed colon
      spl <- append(spl, splitOnUnclosedChar(cp, ':', keep=TRUE))
    }
    # peep through spl
    hub <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
      if (!is.null(pre) && pre == ':' && !is.null(cur)) {  # glimpse at object values
        if (isArray(cur) | isObject(cur)) {  # case struct - recursive
          hasUnboxedAtom(cur)
        } else {                             # case atom object value
          TRUE
        }
      }
    })
    # reason
    return(any(unlist(hub)))
  } else {                                              # case atom
    return(TRUE)
  }
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
  # boxing
  if (isArray(json)) {                                       # case array
    if (hasUnclosedChar(json, ',')) {                        # case pseudo array
      return(structure(paste0('[', json, ']'), class='json'))
    } else if (!hasUnclosedChar(stripArray(json), ',')) {    # case atom array
      return(structure(json, class='json'))
    } else {                                                 # case real array
      spl <- splitOnUnclosedChar(stripArray(json), ',')
      bxd <- sapply(as.list(spl), function(s) {
        if (isObject(s) && hasUnboxedAtom(s)) {
          boxAtoms(s)
        } else {
          s
        }
      })
      glued <- paste0('[', paste0(bxd, collapse=','), ']')
      return(structure(glued, class='json'))
    }
  } else if (isObject(json)) {  # case object
    # split on unclosed comma
    cpl <- splitOnUnclosedChar(stripObject(json), ',', keep=TRUE)
    spl <- vector('character')  # helper accu
    for (cp in cpl) {           # split on unclosed colon
      spl <- append(spl, splitOnUnclosedChar(cp, ':', keep=TRUE))
    }
    # peep through spl
    bxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
      if (!is.null(pre) && pre == ':' && !is.null(cur)) {  # cur object values
        if ((isArray(cur) | isObject(cur)) && hasUnboxedAtom(cur)) {
          boxAtoms(cur)
        } else if (!(isArray(cur) | isObject(cur))) {
          paste0('[', cur, ']')
        } else {  # case boxed struct
          cur
        }
      } else {
        cur
      }
    })
    # glue things
    glued <- paste0('{', paste0(bxd, collapse=''), '}')
    return(structure(glued, class='json'))
  } else {                      # case atom
    bxd <- paste0('[', json, ']')
    return(structure(bxd, class='json'))
  }
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
  # check if input json is an array of length 1 else ...
  if (isArray(json)  && !hasUnclosedChar(stripArray(json), ',')) {
    return(structure(stripArray(json), class='json'))  # early exit
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
    return(structure(unboxd, class='json'))            # early exit
  } else if (isObject(json)) {
    # split on unclosed comma
    cpl <- splitOnUnclosedChar(stripObject(json), ',', keep=TRUE)
    spl <- vector('character')  # helper accu
    for (cp in cpl) {           # split on unclosed colon
      spl <- append(spl, splitOnUnclosedChar(cp, ':', keep=TRUE))
    }
    # peep through
    unbxd <- octostep::octostep(as.list(spl), function(pre, cur, nxt) {
      if (!is.null(pre) && pre == ':' && !is.null(cur)) {  # cur object values
        if ((isArray(cur) | isObject(cur))) {
          unboxAtoms(cur)                              # case boxed struct
        } else if (!(isArray(cur) | isObject(cur))) {
          cur                                          # case already unboxed
          #paste0('[', cur, ']')
        } else if (isArray(cur) | isObject(cur)) {
          cur                                          # case unboxed struct
        }
      } else {
        cur
      }
    })
    # reglue object components
    glued <- paste0('{', paste0(unbxd, collapse=''), '}')
    # serve
    return(structure(glued, class='json')) 
  } else if (!(isObject(json) && isArray(json))) {
    return(structure(json, class='json'))
  }
}
