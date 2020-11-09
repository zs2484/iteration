Exploratory analysis
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.2     √ purrr   0.3.4
    ## √ tibble  3.0.3     √ dplyr   1.0.2
    ## √ tidyr   1.1.2     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Setting options

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)  

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.57489963  0.85752519  0.56666928 -0.97837082 -0.08468670 -0.39437425
    ##  [7] -1.88181795  0.51861309 -0.59399053 -0.20790774 -0.28705304 -0.86962015
    ## [13]  2.04049750 -1.03516343  1.13951656 -0.52153412 -1.93831904  1.59807830
    ## [19]  0.86061527 -0.65062331  0.87352629 -0.47139041 -1.06698465  1.16986489
    ## [25] -0.73264496  0.70522604 -0.90549250  1.28472611  0.01731539  0.41290005

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.57489963  0.85752519  0.56666928 -0.97837082 -0.08468670 -0.39437425
    ##  [7] -1.88181795  0.51861309 -0.59399053 -0.20790774 -0.28705304 -0.86962015
    ## [13]  2.04049750 -1.03516343  1.13951656 -0.52153412 -1.93831904  1.59807830
    ## [19]  0.86061527 -0.65062331  0.87352629 -0.47139041 -1.06698465  1.16986489
    ## [25] -0.73264496  0.70522604 -0.90549250  1.28472611  0.01731539  0.41290005

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

## Multiple outputs

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.19  2.94

Check that the function works.

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.19  2.94

## Multiple inputs

I’d like to do this with function

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.80  2.68

``` r
sim_mean_sd = function(samp_size, mu = 0, sigma = 1){
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
    )

}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.16  3.05

``` r
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.61  3.23

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0604  1.10
