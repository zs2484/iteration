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
    ## -3.55038 -0.73145 -0.07913 -0.04729  0.64513  2.36229

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
    ##  [1] 3.588483 2.302145 2.472944 2.500463 3.109650 4.620747 4.388342 2.748211
    ##  [9] 3.589897 3.538607 4.019879 3.354766 3.938056 2.565290 4.061357 2.056429
    ## [17] 2.196203 3.302708 2.222399 2.530261
    ## 
    ## $b
    ##  [1] -0.5197743 -1.5732706 -2.9181790  3.4956011 -4.7093270 -2.5376201
    ##  [7]  2.7882978  3.4942595 -2.8370901  7.6176729  6.3573085 -2.7844490
    ## [13]  5.7815267  1.6817583 -2.8285133  5.4580931  9.7976420 -2.4838715
    ## [19]  2.0424021  2.1530001  1.8250549  1.5236883  0.5689308 -1.6784293
    ## [25]  3.2793245  1.3141798  5.6755809  9.7051517  2.8003162 -2.8092368
    ## 
    ## $c
    ##  [1]  9.803093  9.929591 10.025511  9.937146 10.342209 10.204827  9.930603
    ##  [8]  9.830642  9.777716  9.879778 10.009388  9.819809 10.179329 10.107223
    ## [15]  9.775319 10.189445 10.060632 10.143856  9.715120 10.428079  9.859509
    ## [22]  9.738372 10.099511  9.954509 10.295683 10.012053 10.420868  9.441749
    ## [29] 10.535770 10.255358 10.174931 10.193237  9.905161  9.909026 10.267991
    ## [36]  9.912351  9.979838  9.959157 10.058505  9.933068
    ## 
    ## $d
    ##  [1] -3.4571597 -3.1078477 -3.3337808 -1.2763918 -2.8595828 -3.4770540
    ##  [7] -2.3524186 -3.5854317 -3.2273742 -4.3333446 -0.8962083 -1.3618574
    ## [13] -1.5159060 -3.8026514 -1.5833944 -2.2568377 -2.5069895 -4.4636014
    ## [19] -3.1044588 -2.6179549

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
    ## 1  3.16 0.793

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.66  3.95

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.223

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.76  1.03

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for(i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function..?

``` r
output = map(list_norm, IQR)
```

``` r
output = map_dbl(list_norm, median, .id = "input")
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```
