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
    ## -2.53907 -0.64930  0.16278  0.05472  0.73155  2.33635

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
    ##  [1] 2.782465 2.924489 2.193330 3.538420 1.528874 2.988736 2.628414 4.092106
    ##  [9] 2.161606 3.014375 1.770553 4.035185 1.335467 2.579335 3.823358 4.680469
    ## [17] 4.878192 3.513860 1.853989 3.845429
    ## 
    ## $b
    ##  [1]  5.88029629 -7.17247185 -0.28094079  2.32934892 -1.01646905  0.88719541
    ##  [7] -5.52404152 -8.36892208  7.07695853  6.15267467  3.43311959 -4.46203148
    ## [13]  5.66578430 -1.45237348  9.72587244 -1.79934095  2.80388365  1.57029056
    ## [19] -2.82618400  3.48601865 -0.48809192 -0.05509166 -5.45084034 -5.40435657
    ## [25] -0.07764675 -0.81808535 -0.11649957  0.55957654 -5.86037069 -5.01414791
    ## 
    ## $c
    ##  [1] 10.075250  9.692229 10.286324  9.994036  9.734613 10.051962 10.192186
    ##  [8] 10.157350 10.152244  9.905339  9.805524  9.932631  9.879979 10.103834
    ## [15]  9.669292 10.106910 10.038049  9.707678  9.949209  9.952734  9.886749
    ## [22]  9.935573 10.260773 10.062489  9.573672 10.139993 10.004779  9.850550
    ## [29]  9.895586 10.090239  9.922961 10.240433 10.226114 10.205618 10.240175
    ## [36]  9.901761  9.539794  9.774197 10.260915  9.780172
    ## 
    ## $d
    ##  [1] -4.4562906 -5.7086708 -3.4381223 -1.8408032 -1.9272967 -2.7228493
    ##  [7] -4.1767575 -3.7359634 -2.6817142 -4.3917276 -2.3572089 -2.5183927
    ## [13] -0.8401766 -2.4294243 -2.6843998 -4.7262251 -4.8425707 -3.4690288
    ## [19] -2.8242770 -2.0425377

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
    ## 1  3.01  1.03

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.221  4.53

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.199

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.19  1.23

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
    ##  [1] 2.782465 2.924489 2.193330 3.538420 1.528874 2.988736 2.628414 4.092106
    ##  [9] 2.161606 3.014375 1.770553 4.035185 1.335467 2.579335 3.823358 4.680469
    ## [17] 4.878192 3.513860 1.853989 3.845429
    ## 
    ## $b
    ##  [1]  5.88029629 -7.17247185 -0.28094079  2.32934892 -1.01646905  0.88719541
    ##  [7] -5.52404152 -8.36892208  7.07695853  6.15267467  3.43311959 -4.46203148
    ## [13]  5.66578430 -1.45237348  9.72587244 -1.79934095  2.80388365  1.57029056
    ## [19] -2.82618400  3.48601865 -0.48809192 -0.05509166 -5.45084034 -5.40435657
    ## [25] -0.07764675 -0.81808535 -0.11649957  0.55957654 -5.86037069 -5.01414791
    ## 
    ## $c
    ##  [1] 10.075250  9.692229 10.286324  9.994036  9.734613 10.051962 10.192186
    ##  [8] 10.157350 10.152244  9.905339  9.805524  9.932631  9.879979 10.103834
    ## [15]  9.669292 10.106910 10.038049  9.707678  9.949209  9.952734  9.886749
    ## [22]  9.935573 10.260773 10.062489  9.573672 10.139993 10.004779  9.850550
    ## [29]  9.895586 10.090239  9.922961 10.240433 10.226114 10.205618 10.240175
    ## [36]  9.901761  9.539794  9.774197 10.260915  9.780172
    ## 
    ## $d
    ##  [1] -4.4562906 -5.7086708 -3.4381223 -1.8408032 -1.9272967 -2.7228493
    ##  [7] -4.1767575 -3.7359634 -2.6817142 -4.3917276 -2.3572089 -2.5183927
    ## [13] -0.8401766 -2.4294243 -2.6843998 -4.7262251 -4.8425707 -3.4690288
    ## [19] -2.8242770 -2.0425377

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
    ## -2.53907 -0.64930  0.16278  0.05472  0.73155  2.33635

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
    ## 1  3.01  1.03

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.221  4.53

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.01  1.03
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.221  4.53
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.199
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.19  1.23

So…can I add a list column?

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median)
    )
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\DELL\AppData\Local\Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-05 16:32:26 (7.537)

    ## file min/max dates: 1869-01-01 / 2020-10-31

    ## using cached file: C:\Users\DELL\AppData\Local\Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-05 16:32:52 (1.703)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: C:\Users\DELL\AppData\Local\Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-05 16:33:04 (0.882)

    ## file min/max dates: 1999-09-01 / 2020-10-31

Get our list columns ..

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # ... with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # ... with 355 more rows

Suppose i want to regress `tmax` on `tmin` for each station.

This works …

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Let’s write a function.

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}


output = vector("list", 3)

for (i in 1:3) {
  
  output[[i]] = weather_lm(weather_nest$data[[i]])
  
}
```

What about a map …\!?

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column \!\!\!\!\!??

``` r
weather_nest = 
  weather_nest %>% 
  mutate(models = map(data, weather_lm))

weather_nest$models
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221
