Iteration and listcols
================

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.60589 -0.78931  0.04341 -0.02784  0.68392  2.81016

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.525518 4.405625 3.486753 2.587241 2.538295 2.340198 2.789205 3.193873
    ##  [9] 3.009838 2.647424 1.993563 4.033733 3.646319 4.335051 4.333553 4.087606
    ## [17] 2.664054 2.973302 2.440714 2.530321
    ## 
    ## $b
    ##  [1]  0.4100785 -2.3101498 -1.4041674  0.4221478  1.6258593 -0.8794995
    ##  [7] -3.3056651 -7.5277917  8.3963038 -8.6119925 -9.1407611  0.6467784
    ## [13]  2.4064919  0.5676097 -4.5821630 -1.7360761 -6.8639338  2.7789916
    ## [19]  1.6262001  0.5261943 -1.2540264  0.1540868 -3.3468084 -3.9415406
    ## [25] -5.7850502  3.9219734 -3.8327243  4.2838301  5.5230548  3.4789106
    ## 
    ## $c
    ##  [1]  9.890662 10.091320  9.838674 10.023546  9.710967 10.199216  9.983706
    ##  [8] 10.203145  9.654291 10.280487 10.351690 10.071830  9.997273 10.291342
    ## [15] 10.130918  9.868018 10.226052  9.902032 10.002668 10.070048 10.077876
    ## [22]  9.805829 10.292676  9.955475  9.875277  9.941399 10.101658  9.961176
    ## [29]  9.970850  9.846850  9.789327 10.183461  9.753552 10.113743 10.174731
    ## [36] 10.254618 10.017794  9.923554 10.318331 10.035288
    ## 
    ## $d
    ##  [1] -3.423308 -3.181422 -2.138920 -3.653791 -1.894176 -1.251540 -2.549081
    ##  [8] -3.271485 -3.992268 -2.933049 -3.816609 -3.391930 -2.524869 -2.203118
    ## [15] -1.251846 -3.848458 -2.459453 -1.401955 -1.996887 -2.887156

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.13 0.762

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.925  4.24

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.178

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.70 0.873

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for(i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```
