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
    ## -2.00617 -0.69360 -0.12083 -0.05139  0.44510  3.21207

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
    ##  [1] 2.8020935 0.7133164 0.9522128 1.5679372 2.4059047 2.3459239 2.9860879
    ##  [8] 2.5827118 3.6895276 3.3205306 2.9155694 2.3438225 4.7615930 2.3899742
    ## [15] 2.0524281 3.4054891 3.6241092 4.3728649 1.6230209 2.6472053
    ## 
    ## $b
    ##  [1] 10.1263691 11.1707903 -8.9660027  1.9022856 -6.7314155  5.7683836
    ##  [7]  0.3404222  3.5762539 -0.2591242  8.8608567  6.8169462  3.8392061
    ## [13]  0.0898868  2.0654853 10.1782564  3.6942141  2.8570745 -0.8800374
    ## [19] -5.8288526  5.1353258 -8.2358981  1.8804436 -7.0879010  4.1786315
    ## [25] -2.5991986  1.5843340  4.0088154  0.6160156 12.5524504  0.3064646
    ## 
    ## $c
    ##  [1] 10.149498  9.716365  9.932779 10.062078  9.905004 10.355229  9.819088
    ##  [8] 10.152699  9.917520 10.273536 10.219619  9.943135  9.774731 10.004638
    ## [15] 10.227008 10.023351  9.717104  9.687774 10.013416 10.059832 10.221189
    ## [22] 10.203033  9.843056 10.052482 10.483649  9.898172  9.907352 10.115660
    ## [29] 10.073047  9.914194 10.151573  9.836866  9.863749  9.730124 10.058519
    ## [36]  9.869447  9.748359  9.993569  9.939145  9.749240
    ## 
    ## $d
    ##  [1] -2.180198 -3.749505 -3.327621 -1.581648 -2.989231 -1.852249 -3.777034
    ##  [8] -2.175177 -2.331716 -2.942597 -4.574207 -2.326819 -2.767105 -2.010105
    ## [15] -3.513536 -2.479370 -1.706385 -4.026559 -2.742946 -2.035552

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
    ## 1  2.68  1.03

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.03  5.67

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.190

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.75 0.841

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

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.8020935 0.7133164 0.9522128 1.5679372 2.4059047 2.3459239 2.9860879
    ##  [8] 2.5827118 3.6895276 3.3205306 2.9155694 2.3438225 4.7615930 2.3899742
    ## [15] 2.0524281 3.4054891 3.6241092 4.3728649 1.6230209 2.6472053
    ## 
    ## $b
    ##  [1] 10.1263691 11.1707903 -8.9660027  1.9022856 -6.7314155  5.7683836
    ##  [7]  0.3404222  3.5762539 -0.2591242  8.8608567  6.8169462  3.8392061
    ## [13]  0.0898868  2.0654853 10.1782564  3.6942141  2.8570745 -0.8800374
    ## [19] -5.8288526  5.1353258 -8.2358981  1.8804436 -7.0879010  4.1786315
    ## [25] -2.5991986  1.5843340  4.0088154  0.6160156 12.5524504  0.3064646
    ## 
    ## $c
    ##  [1] 10.149498  9.716365  9.932779 10.062078  9.905004 10.355229  9.819088
    ##  [8] 10.152699  9.917520 10.273536 10.219619  9.943135  9.774731 10.004638
    ## [15] 10.227008 10.023351  9.717104  9.687774 10.013416 10.059832 10.221189
    ## [22] 10.203033  9.843056 10.052482 10.483649  9.898172  9.907352 10.115660
    ## [29] 10.073047  9.914194 10.151573  9.836866  9.863749  9.730124 10.058519
    ## [36]  9.869447  9.748359  9.993569  9.939145  9.749240
    ## 
    ## $d
    ##  [1] -2.180198 -3.749505 -3.327621 -1.581648 -2.989231 -1.852249 -3.777034
    ##  [8] -2.175177 -2.331716 -2.942597 -4.574207 -2.326819 -2.767105 -2.010105
    ## [15] -3.513536 -2.479370 -1.706385 -4.026559 -2.742946 -2.035552

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
    ## -2.00617 -0.69360 -0.12083 -0.05139  0.44510  3.21207

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.68  1.03

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.03  5.67

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.68  1.03
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.03  5.67
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.190
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.75 0.841

So…can I add a list column?

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median)
    )
```
