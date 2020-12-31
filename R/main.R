#' loop on input
#'
#' @description
#'
#' `loop()` unifies `lapply` and `Map`. It supports a dollar notation so
#' that we can leverage the autompletion of function arguments.
#' Arguments to be looped on are prefixed with `+`.
#'
#' `looper()` works similarly to `loop()`, but it takes the looping
#' arguments first, and then an expression to loop over. This is closer
#' in appearance to a `for()` loop
#'
#' @param fun
#' function to be looped over
#'
#' @param type
#' type of output, created via `as.type()` function call. A few special
#' tokens are permitted:
#' * `df_rows` - returns a data.frame, where each result is a row
#' * `mat_rows` - returns a matrix, where each result is a row
#' * `mat_cols` - returns a matrix, where each result is a column
#' * `simplify` - returns a simplified output, á la `sapply()`
#'
#' @return
#' @export
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
#'
#' looper(n=+1:3,mean=+1:3,
#'   {
#'     data.frame(col_1 = rnorm(n,mean),
#'                col_2 = rnorm(n,2*mean)
#'     )
#'   }
#' )
#'
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
`$.loop` <- function(loop, fun) {
  eval.parent(bquote(loop(.(as.symbol(fun)))))
}

#' @export
#' @rdname loop
looper <- function(...,type=list){
  all_args <- as.list(match.call())[-1]
  dots <- all_args[names(all_args) != "type"]

  expr <- dots[[length(dots)]]
  args <- dots[-length(dots)]

  arg_nms <- names(args)
  args <- args[arg_nms != ""]
  arg_nms <- names(args)

  arg_list <- vector("pairlist",length(args))
  names(arg_list) <- arg_nms


  loop_env <- new.env(parent=parent.frame())

  loop_env[["looping_fun"]] <- as.function(c(as.pairlist(arg_list),expr))


  loop_call <- call("loop",quote(looping_fun),type = substitute(type))

  out_call <- as.call(c(loop_call,args))

  eval(out_call,loop_env)
}



as.df_rows <- function(x){
  res <- Reduce(rbind,x)
  n_rows <- vapply(x,nrow,numeric(1))
  row_names <- rep(1:length(x),n_rows)
  row_subs <- unlist(lapply(n_rows,seq,from=1))

  rownames(res) <- paste0(row_names,".",row_subs)

  res

}

as.mat_rows <- function(x){
  Reduce(rbind,lapply(x,matrix,nrow=1))
}

as.mat_cols <- function(x){
  Reduce(cbind,lapply(x,matrix,ncol=1))
}

as.simplify <- function(x){
  simplify2array(x,higher = TRUE)
}

