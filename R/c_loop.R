#' loop vector
#'
#' `c_loop()` takes in a set of vectors and transposes them into a
#' list of vectors. The length of the resulting list is the same as
#' each of the values passed to it. vectors of length 1 are recycled
#' throughout the results.
#'
#' @param ...
#' vectors to be transformed into a list
#'
#' @examples
#' c_loop(1:3,2:4,3:5,-1)
#'
#' @export
c_loop <- function(...){
  dots <- list(...)
  len_dots <- vapply(dots,length,numeric(1))
  len_dots <- len_dots[len_dots != 1]

  len_out <- unique(len_dots)
  if(length(len_out) > 1)
    stop("Lengths differ in c_loop",call.=F)

  mode_dots <- vapply(dots,mode,character(1))
  mode_out <- unique(mode_dots)
  if(length(mode_out) > 1)
    stop("data types differ in c_loop",call.=F)

  width_out <- length(dots)

  res <- replicate(len_out,vector(mode_out,width_out),simplify=F)

  for(i in 1:len_out){
    res[[i]] <- vapply(1:width_out,
                       function(j){
                         c_dots <- dots[[j]]
                         if(length(c_dots) == 1){
                           c_dots
                         } else {
                           c_dots[[i]]
                         }
                       },
                       vector(mode_out,length=1))
    names(res[[i]]) <- names(dots)
    class(res[[i]]) <- c("c_loop_item",class(res[[i]]))
  }

  structure(
    res,
    class="c_loop"
  )

}

#' @export
print.c_loop <- function(x,...){
  print(unclass(x),...)
}

#' @export
print.c_loop_item <- function(x,...){
  print(unclass(x),...)
}

#' @export
`$.c_loop_item` <- function(x,var){
  x[[substitute(var)]]
}

#' @export
`$.c_loop` <- function(x,var){
  res <- NULL
  fn <- function(y) y[[substitute(var)]]
  for(i in 1:length(x)){
    y <- x[[i]]
    res <- c(res,y[[substitute(var)]])
  }
  res

}
