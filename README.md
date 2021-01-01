
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loop

**`{loop}`** contains several functions to perform loops in interesting
ways. The simplest function is `loop()`, which is built on by the other
functions.

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
```

Outputs from `loop()` can be simplified by type

``` r

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
#>  [1] 0.933951 2.020204 3.034272 3.999003 4.992944 6.071358 6.958827 8.024268
#>  [9] 9.029840 9.943802

loop(rnorm,mat_rows)(3,+1:4) # returns a 4x3 matrix
#>            [,1]      [,2]     [,3]
#> [1,] -0.1004785 0.5808227 2.633305
#> [2,]  1.5716095 2.9093270 2.822132
#> [3,]  3.7435092 3.3989739 2.812274
#> [4,]  4.7588610 3.0590316 4.834673
loop(rnorm,mat_cols)(3,+1:4) # returns a 3x4 matrix
#>             [,1]      [,2]     [,3]     [,4]
#> [1,]  1.97397792 1.5689950 1.545367 3.797918
#> [2,]  0.03702945 0.8972713 2.708236 3.301417
#> [3,] -1.43275088 2.2960348 2.537689 4.621955
```

To make use of both the type return and the dollar notation, we also
provide various wrapper functions around `loop(fun,type)` such as
`loop_dbl()`, `loop_int()` and `loop_chr()`.

``` r
loop_dbl$mean(+x)
#>  [1] 0.933951 2.020204 3.034272 3.999003 4.992944 6.071358 6.958827 8.024268
#>  [9] 9.029840 9.943802
loop_int$length(+x)
#>  [1] 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
loop_chr$paste0(+letters,+LETTERS)
#>  [1] "aA" "bB" "cC" "dD" "eE" "fF" "gG" "hH" "iI" "jJ" "kK" "lL" "mM" "nN" "oO"
#> [16] "pP" "qQ" "rR" "sS" "tT" "uU" "vV" "wW" "xX" "yY" "zZ"
```

When working with functions which take vectors as arguments, we would
normally wrap variables in `c()` to pass them as a single argument. The
parallel to this in `{loop}` is `c_loop()`

``` r
x <- rnorm(10)
y <- rnorm(10)
z <- rnorm(10)
loop_dbl$mean(+c_loop(x,y,z))
#>  [1]  0.61583026 -0.21181982  0.18549941  1.34721870  0.51858922 -0.01599143
#>  [7] -1.23745289 -0.64744562  0.21819208  0.75766111
```

This is particularly useful in `tidyverse` evaluations, and allows for
row-wise evaluation of commands.

``` r
df <- data.frame(
  x = rnorm(3),
  y = rnorm(3),
  z = rnorm(3)
)

dplyr::mutate(df,m = loop_dbl$mean(+c_loop(x,y,z)))
#>           x            y           z          m
#> 1 0.1913415 -0.739228384 -1.10237129 -0.5500861
#> 2 0.1450227 -0.005073902  0.91832300  0.3527573
#> 3 1.5028149  0.082177053  0.02354768  0.5361799
```

A final example of where `c_loop()` can make iteration easier is in a
`for()` loop. We can iterate through the `c_loop()` elements in
parallel, by referring to them as elements of the iterator, either using
`$` for named elements or their positional number

``` r
for(. in c_loop(a=1:3,b=2:4)){
  print(.$a*.$b)
}
#> [1] 2
#> [1] 6
#> [1] 12

for(. in c_loop(1:3,3:5)){
  print(.[1]*.[2])
}
#> [1] 3
#> [1] 8
#> [1] 15
```

`looper()` also behaves like `for()` in that it takes an *expression*
provided between curly braces, `{` and `}`. Much like the above, it can
iterate over several variables at once using the same notation as
`loop()`, except we don’t need to pre-pend variables with a `$`. Much
like `loop()`, it returns a variable, which can be simplified with
`type=`

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
