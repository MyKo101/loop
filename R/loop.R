#' @name loop
#'
#' @title loop on input
#'
#' @description
#' `loop()` unifies `lapply` and `Map`. It supports a dollar notation so
#' that we can leverage the autompletion of function arguments.
#' Arguments to be looped on are prefixed with `+`.
#'
#' @param fun
#' function to be looped over
#'
#' @param type
#' type of output, created via a call to `as._type_()`. A few special
#' tokens are permitted:
#' * `df_rows` - returns a data.frame, where each result is a set of
#'    rows
#' * `mat_rows` - returns a matrix, where each result is a row
#' * `mat_cols` - returns a matrix, where each result is a column
#' * `simplify` - returns a simplified output, similar to `sapply()`
#'
#' @examples
#' x <- loop(rnorm)(10,mean=+1:3)
#' x
#'
#' loop(mean,numeric)(+x)
#'
#' l <- list(iris, cars)
#' loop$head(+l, 1)
#' loop$head(+l, +1:2) # no need to worry about operator precedence
#'
#'@export
loop <- function(fun,type = list) {
  f <- args(fun)
  as.type_str <- paste0("as.",deparse(substitute(type)))
  as.type <- as.name(as.type_str)
  body(f) <- substitute({
    args <- as.list(match.call())[-1]
    plus_args <- unlist(lapply(args, function(x) {
      x <- deparse1(x)
      if(startsWith(x,"+")) {
        str2lang(substr(x,2, nchar(x)))
      }
    }))
    other_args <- args[!names(args) %in% names(plus_args)]
    if(length(plus_args) == 0) stop("No '+' arguments provided to looped function",call.=F)
    call <- bquote(Map(function(...) fun(..., ..(other_args)), ..(plus_args)), splice = TRUE)
    as.type(eval.parent(call))
  })

  if(as.type_str %in% ls(envir=asNamespace("loop"),pattern="^as\\.(.*?)$")){
    environment(f) <- new.env(parent=parent.frame())
    environment(f)[[as.type_str]] <- get(as.type_str,mode="function",envir = asNamespace("loop"))
  }

  f
}

class(loop) <- "loop"

#' @export
`$.loop` <- function(loop_fun,fun){
  eval.parent(bquote(.(loop_fun)(.(as.symbol(fun)))))
}
