#' Arithmetic operators
#' @name Arithmetic
#' @rdname Arithmetic
#' @description Basic arithmetic operators that substitute original object with new value.
NULL

#' @rdname Arithmetic
#' @export
`%+=%` <- function(e1,e2) {
  eval.parent(substitute(e1 <- e1 + e2))
}

#' @rdname Arithmetic
#' @export
`%-=%` <- function(e1,e2) {
  eval.parent(substitute(e1 <- e1 - e2))
}

#' @rdname Arithmetic
#' @export
`%*=%` <- function(e1,e2) {
  eval.parent(substitute(e1 <- e1 * e2))
}

#' @rdname Arithmetic
#' @export
`%/=%` <- function(e1,e2) {
  eval.parent(substitute(e1 <- e1 / e2))
}

#' @rdname Arithmetic
#' @export
`%^=%` <- function(e1,e2) {
  eval.parent(substitute(e1 <- e1 ^ e2))
}

#' @rdname Arithmetic
#' @export
`+` <- function(x, y) {
  if(missing(x) | missing(y)) {
    xs<-as.list(substitute(x))
    print(xs)
    if(xs[[1]] == '+') {
      xp <- xs[[2]]
      return(eval(xp, parent.frame(2))+1)
    } else {
      return(help(xs))
    }
  }
  else {
    return(base::`+`(x,y))
  }
}
