#' Loop over an expression
#'
#' `looper()` is a wrapper around `loop()`. It takes a set of looping
#' arguments first, and then an expression to loop over as it's last
#' argument. This is closer in appearance to a `for()` loop, but can
#' loop over multiple vectors.
#'
#' @inheritParams loop
#'
#' @param ...
#' arguments to loop over, followed by a expression to perform
#'
#' @examples
#' looper(n = +1:3,mean=+0:2,sd=2,
#'   type = df_rows,
#'   {
#'     data.frame(
#'       col_1 = rnorm(n),
#'       col_2 = rnorm(n,mean,sd)
#'       )
#'   })
#'
#' @export
#'
#'
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
