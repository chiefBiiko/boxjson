# boxjson

#' Is character vector with number of characters >= 1?
#'
#' @param x R object.
#' @return Logical.
#'
#' @internal
isTruthyChr <- function(x) {
  if (is.character(x) && nchar(x) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is JSON an array?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @internal
isArray <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\[.*\\]$', json, perl=TRUE))
}

#' Is JSON an object?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @internal
isObject <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\{.*\\}$', json, perl=TRUE))
}

#' Strips an array's outer brackets
#'
#' @param json JSON array.
#' @return Stripped JSON array.
#'
#' @internal
stripArray <- function(json) {
  stopifnot(isTruthyChr(json))
  if (grepl('\\[.+\\]', json, perl=TRUE)) {
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
#' @internal
stripObject <- function(json) {
  stopifnot(isTruthyChr(json))
  return(gsub('^\\{|\\}$', '', json, perl=TRUE))
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

#' Does a string contain a character neither enclosed in brackets nor 
#' double quotes?
#'
#' @param string Character vector of length 1L.
#' @param character Single character to search for.
#' @return Logical.
#'
#' @internal
hasUnclosedChar <- function(string, char) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L)
  # split to single characters
  chars <- strsplit(string, '')[[1]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  # peep through
  for (i in seq(length(chars))) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char && 
        (opbr == 0L && opqt %% 2L == 0L  && 
         all(unlist(nsqt) %% 2L == 0L))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Splits a string on given character neither enclosed in brackets nor 
#' double quotes
#'
#' @param string Character vector of length 1L.
#' @param character Single character to split on.
#' @return Chr vector.
#'
#' @internal
splitOnUnclosedChar <- function(string, char, keep=FALSE) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L,
            is.logical(keep))
  # split to single characters
  chars <- strsplit(string, '', fixed=TRUE)[[1]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  last.cut <- 0L    # tracks last slice index
  accu <- vector('character')
  # peep through
  for (i in seq(length(chars))) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char && 
        (opbr == 0L && opqt %% 2L == 0L  && 
         all(unlist(nsqt) %% 2L == 0L))) {
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
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
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
          hasUnboxedAtoms(cur)
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
  
  if (isArray(json)) {          # case array
    spl <- splitOnUnclosedChar(stripArray(json), ',')
    if (length(spl == 1L)) return(structure(json, class='json'))  # early exit
    bxd <- sapply(as.list(spl), function(s) {
      if ((isArray(s) | isObject(s)) && hasUnboxedAtoms(s)) {
        boxAtoms(s)
      } else if (s != '[]') {
        paste0('[', s, ']')
      } else if (s == '[]') {
        ''  # 2 avoid overboxing
      }
    })
    glued <- paste0('[', paste0(bxd, collapse=','), ']')
    return(structure(glued, class='json'))
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
        if ((isArray(cur) | isObject(cur)) && hasUnboxedAtoms(cur)) {
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
