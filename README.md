
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loop

**`{loop}`** contains two functions:

  - `loop()`, which unifies `lapply()`, `vapply()`, `sapply()` and
    `Map()`.
  - `looper()`, which acts the same, but looks closer to a `for()` loop.

`loop` supports a dollar notation so that we can leverage the
autompletion of function arguments. Arguments to be looped on are
prefixed with `+`. Outputs are simplified by `type` if provided.

``` r
library(loop)
loop(rep)(+11:12, 2)   # rather than `lapply(11:12, rep, 2)`
#> [[1]]
#> [1] 11 11
#> 
#> [[2]]
#> [1] 12 12
loop$rep(+11:12, 2)    # same thing, but we benefit from autocomplete
#> [[1]]
#> [1] 11 11
#> 
#> [[2]]
#> [1] 12 12
loop$rep(+11:12, +1:2) # rather than `Map(rep, 11:12, 1:2)`
#> [[1]]
#> [1] 11
#> 
#> [[2]]
#> [1] 12 12

# rather than `Map(function(...) rep(..., each =2), 11:12, 1:2)` :
loop$rep(+11:12, +1:2, each = 2) 
#> [[1]]
#> [1] 11 11
#> 
#> [[2]]
#> [1] 12 12 12 12

l <- list(iris, cars)
loop(nrow, numeric)(+l) # rather than `vapply(l, nrow, numeric(1))`
#> [1] 150  50
```

The `type` can be any of the usual atomic types such as `double`,
`numeric`, `character`, `logical`, plus any of four special tokens:

  - `df_rows` - returns a data.frame, where each result is a row
  - `mat_rows` - returns a matrix, where each result is a row
  - `mat_cols` - returns a matrix, where each result is a column
  - `simplify` - returns a simplified output, á la `sapply()`

<!-- end list -->

``` r
x <- loop(rnorm)(1000,+1:10)
loop(mean,numeric)(+x)
#>  [1] 0.9776082 1.9395727 2.9770537 3.9329127 5.0152034 6.0604742 7.0422687
#>  [8] 8.0472034 9.0415219 9.9954185

loop(rnorm,mat_rows)(3,+1:4) # returns a 4x3 matrix
#>           [,1]      [,2]      [,3]
#> [1,] 0.8402669 0.8102785 0.2513152
#> [2,] 2.6838310 1.7661374 1.6171371
#> [3,] 4.1577052 1.8985105 3.2110182
#> [4,] 3.5847946 3.8773371 2.7513435
loop(rnorm,mat_cols)(3,+1:4) # returns a 3x4 matrix
#>          [,1]     [,2]     [,3]     [,4]
#> [1,] 2.692200 3.720136 3.987687 6.158450
#> [2,] 1.190785 3.866688 1.478082 3.423218
#> [3,] 2.233132 2.252980 3.708424 2.626643
```

`looper()` behaves more like `for()` in that it takes an *expression*
provided between curly braces, `{` and `}`. The difference is that
instead of looping over a single variable, `looper()` loops over
multiple at once, just like `loop()`

``` r
looper(
  seed = +c(100,300,500),
  n = +1:3, mean = +2:4,  sd = 1,
            lower = +0:2, upper=5,
  type = df_rows,
  {
    set.seed(seed)
    data.frame(
      col_1 = rnorm(n,mean,sd),
      col_2 = runif(n,lower,upper)
    )
  }
)
#>        col_1    col_2
#> 1.1 1.497808 2.761612
#> 2.1 4.373791 3.728271
#> 2.2 3.862107 1.048121
#> 3.1 4.968489 3.536546
#> 3.2 5.965368 4.776398
#> 3.3 4.886323 4.486494
```

## Installation

Install with :

``` r
remotes::install_github("moodymudskipper/loop")
```

## Why ?

We get used to the apply functions, but :

  - They’re hard to grasp at first
  - No way to leverage the applied function’s autocomplete
  - The IDE highlights the apply function, not the applied function
  - No need to shuffle the arguments when going from `lapply` to `Map`
    or `sapply` to `mapply`
  - Much more compact to use constant arguments with `Map` and `sapply`
  - I believe its more intuitive to have the applied function first and
    distinct from the arguments
