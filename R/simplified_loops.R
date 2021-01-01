#' @name simplified_loops
#' @rdname simplified_loops
#'
#' @title Simplifying Loops
#'
#' @description
#' These functions are wrappers around `loop()` with a
#' pre-specified return type. This allows the `$` notation to be used
#' with a predictable outcome
#'
#'
#' @examples
#' x <- loop$rnorm(n=100,mean=+1:20)
#' loop_dbl$mean(+x)
#' loop_int$length(+x)
#' loop_chr$paste0(+letters,+LETTERS)
#' loop_mtr$rnorm(n=10,mean=+5:8)
#' loop_mtc$rnorm(n=10,mean=+5:8)
#' loop_smp$rnorm(n=10,mean=+5:8)

NULL
.loop_wrap <- function(type){
  f <- function(fun) NULL
  body(f) <- substitute({
    eval.parent(call("loop",substitute(fun),type=quote(type)))
  })
  class(f) <- "loop"
  f
}

#' @export
#' @describeIn simplified_loops outputs a double vector
#' @inheritParams loop
loop_dbl <- .loop_wrap(double)

#' @export
#' @describeIn simplified_loops outputs an integer vector
loop_int <- .loop_wrap(integer)

#' @export
#' @describeIn simplified_loops outputs a character vector
loop_chr <- .loop_wrap(character)

#' @export
#' @describeIn simplified_loops outputs a logical vector
loop_lgl <- .loop_wrap(logical)

#' @export
#' @describeIn simplified_loops outputs a data.frame
loop_dfr <- .loop_wrap(dat_rows)

#' @export
#' @describeIn simplified_loops outputs a matrix joined by rows
loop_mtr <- .loop_wrap(mat_rows)

#' @export
#' @describeIn simplified_loops outputs a matrix joined by columns
loop_mtc <- .loop_wrap(mat_cols)

#' @export
#' @describeIn simplified_loops outputs a `sapply()` style output
loop_smp <- .loop_wrap(simplify)



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

