#' Logical Operators
#' @name Logical
#' @rdname Logical
#' @description Ternary and in-between operators used to simplify if procedures and && operations especially useful for quick returning of the row indices.
NULL

#' @rdname Ternary
#' @title Ternary operator
#' @name Ternary operator
#' @param x first op
#' @param y second op
#' @usage ? x : y
#' @export
`?` <- function(x, y) {
  if(missing(x) | missing(y)) {
    xs<-as.character(substitute(x))
    if(xs[[1]] == '?') {
      return(help.search(xs[[2]]))
    } else {
      return(help(xs))
    }
  }
  xs <- as.list(substitute(x))
  if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
  r <- eval(
    sapply(
      strsplit(
        deparse(
          substitute(y)), ":"),
      function(e) {
        parse(text = e)
      })[[2 - as.logical(x)]])
  if (xs[[1]] == as.name("<-")) { # Add the "=", "<<-" assigniment operators
    xs[[3]] <- r
    eval.parent(as.call(xs))
  } else {
    r
  }
}

#' @rdname Logical
#' @name between
#' @export
`%><%` <- function(a, b) {
  a > b[1] & a < b[2]
}

#' @rdname Logical
#' @name between_eq
#' @export
`%>=<%` <- function(a, b) {
  a >= b[1] & a <= b[2]
}
