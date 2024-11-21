stat_learning
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(glmnet)
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-8

``` r
library(broom)
set.seed(123456789)
```

## Try Lasso!!

import and clean birthweight data

``` r
bwt_df = 
  read_csv("data/birthweight.csv") |> 
  janitor::clean_names() |> 
  mutate(
    babysex = 
      case_match(babysex, 
                 1 ~ "male",
                 2 ~ "female"),
     babysex = fct_infreq(babysex),
    frace = 
        case_match(frace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican", 
            8 ~ "other"),
    frace = fct_infreq(frace),
    mrace = 
        case_match(mrace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican",
            8 ~ "other"),
    mrace = fct_infreq(mrace),
    malform = as.logical(malform)
    ) |> 
  sample_n(200)
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Construct inputs for ‘glmnet’

``` r
x = model.matrix(bwt ~ ., data = bwt_df)[, -1]
y =  bwt_df |> pull(bwt)
```

fit lasso for several lambdas

``` r
lambda = 10^seq(-2, 2.75, by = 0.1)

lasso_fit = 
  glmnet(x = x, y = y, lambda = lambda)

lasso_cv = 
  cv.glmnet(x = x, y = y, lambda = lambda)

lambda_opt = lasso_cv[["lambda.min"]]
```

usual lasso plot

``` r
lasso_fit |> 
  broom::tidy() |> 
  filter(term != "(Intercept)") |> 
  select(term, lambda, estimate) |> 
  complete(term, lambda, fill = list(estimate = 0)) |> 
  ggplot(aes(x =lambda, y = estimate, group = term, color = term)) +
  geom_vline(xintercept = lambda_opt,color = "red") +
  geom_line()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
final_lasso_fit = 
  glmnet(x = x, y = y, lambda = lambda_opt)

final_lasso_fit |> 
  broom::tidy()
```

    ## # A tibble: 16 × 5
    ##    term               step  estimate lambda dev.ratio
    ##    <chr>             <dbl>     <dbl>  <dbl>     <dbl>
    ##  1 (Intercept)           1 -6456.      7.94     0.685
    ##  2 babysexfemale         1    15.9     7.94     0.685
    ##  3 bhead                 1   120.      7.94     0.685
    ##  4 blength               1    87.8     7.94     0.685
    ##  5 delwt                 1     0.655   7.94     0.685
    ##  6 fincome               1     1.22    7.94     0.685
    ##  7 fraceblack            1   -91.1     7.94     0.685
    ##  8 fraceother            1  -166.      7.94     0.685
    ##  9 malformTRUE           1   279.      7.94     0.685
    ## 10 menarche              1   -10.2     7.94     0.685
    ## 11 mheight               1    15.5     7.94     0.685
    ## 12 momage                1     3.90    7.94     0.685
    ## 13 mracepuerto rican     1   -79.6     7.94     0.685
    ## 14 mraceasian            1   -84.1     7.94     0.685
    ## 15 smoken                1    -2.78    7.94     0.685
    ## 16 wtgain                1     7.25    7.94     0.685

look at CV results

``` r
lasso_cv |> 
  broom::tidy() |> 
  ggplot(aes(x = log(lambda, 10), y = estimate)) +
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Cluster pokenons

``` r
pokemon_df = 
  read_csv("data/pokemon.csv") |> 
  janitor::clean_names() |> 
  select(hp, speed)
```

    ## Rows: 800 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Name, Type 1, Type 2
    ## dbl (9): #, Total, HP, Attack, Defense, Sp. Atk, Sp. Def, Speed, Generation
    ## lgl (1): Legendary
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pokemon_df |> 
  ggplot(aes(x = hp, y = speed))+
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

let’s use kmeans to cluster these pokemon!

``` r
kmeans_fit = 
  kmeans(x = pokemon_df, centers =4)
```

can I plot these results

``` r
pokemon_df = 
  broom::augment(kmeans_fit, pokemon_df )

pokemon_df |> 
  ggplot(aes(x = hp, y = speed, color = .cluster))+
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

\`\`\`
