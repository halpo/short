# short

[![Build Status](https://travis-ci.org/haplo/short.svg?branch=master)](https://travis-ci.org/tidyverse/pivot)
<!--[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pivot)](https://cran.r-project.org/package=short)-->

# Overview

The `short` package gives several shortcuts to common functionality.

# `using` - easy loading packages

The `using` function is a simpler way of specifying multiple packages to 
load.  It additionally will attempt to download and install the packages
if not already installed.


# `table_1` - Table 1 Demographics and Summaries

The `table_1` function can be used to generate a demographics and summary
of variables; i.e. the veritable "Table 1" for publications.
For each variable given, `table_1` calls `summarise_table_1` which 
will summarise the variable for each value of a key, diferentiating 
variable, as well as overall.  For character and factor variables, 
the counts and percentages of each level is given.
For logical variables, the count of positives and the percentage is 
given as the value.  Numeric Variables are summarised by the minimum, 
median, mean, maximum, and standard deviation.

Additional methods for table_1 can be added by adding methods to 
`summarise_table_1`

### Example


```r
library(tidyverse)
mtcars %>% 
    mutate( gear = ordered(paste(gear, 'Gears'))
          , cyl  = ordered(paste(cyl, 'Cylinders'))
          ) %>% 
    table_1( cyl
           , 'Automatic Transmission' = am == 0
           , 'Number of gears' = gear
           , Horsepower = hp
           )
```

```
## # A tibble: 9 x 6
##   Variable       Level  `(All)`  `4 Cylinders` `6 Cylinders` `8 Cylinders`
##   <chr>          <chr>  <chr>    <chr>         <chr>         <chr>        
## 1 Automatic Tra… Yes    19 (59.… 3 (27.27%)    4 (57.14%)    12 (85.71%)  
## 2 Number of gea… 3 Gea… 15 (46.… 1 (9.09% )    2 (28.57%)    12 (85.71%)  
## 3 Number of gea… 4 Gea… 12 (37.… 8 (72.73%)    4 (57.14%)    <NA>         
## 4 Number of gea… 5 Gea… 5 (15.6… 2 (18.18%)    1 (14.29%)    2 (14.29%)   
## 5 Horsepower     Min    " 52.0"  " 52.0"       105.0         150.0        
## 6 Horsepower     Median 123.0    " 66.0"       110.0         180.0        
## 7 Horsepower     Mean   146.7    " 82.6"       122.3         209.2        
## 8 Horsepower     Max    335.0    113.0         175.0         335.0        
## 9 Horsepower     SD     " 68.6"  " 20.9"       " 24.3"       " 51.0"
```


# `with_margins` - add marginals to and computation.

The `with_margins` function converts another function to opperate over 
all possible subsets of groups, including no groups.

### Example


```r
mtcars %>% 
    mutate( gear = ordered(paste(gear, 'Gears'))
          , cyl  = ordered(paste(cyl, 'Cylinders'))
          ) %>%
    group_by(gear, cyl) %>% 
    with_margins(summarise_at)( vars(mpg, disp, hp, wt)
                              , funs(min, mean, max)
                              )
```

```
## # A tibble: 15 x 14
##    gear   cyl    mpg_min disp_min hp_min wt_min mpg_mean disp_mean hp_mean
##    <ord>  <ord>    <dbl>    <dbl>  <dbl>  <dbl>    <dbl>     <dbl>   <dbl>
##  1 3 Gea… 4 Cyl…    21.5    120.     97.   2.46     21.5      120.    97.0
##  2 3 Gea… 6 Cyl…    18.1    225.    105.   3.22     19.8      242.   108. 
##  3 3 Gea… 8 Cyl…    10.4    276.    150.   3.44     15.0      358.   194. 
##  4 4 Gea… 4 Cyl…    21.4     71.1    52.   1.62     26.9      103.    76.0
##  5 4 Gea… 6 Cyl…    17.8    160.    110.   2.62     19.8      164.   116. 
##  6 5 Gea… 4 Cyl…    26.0     95.1    91.   1.51     28.2      108.   102. 
##  7 5 Gea… 6 Cyl…    19.7    145.    175.   2.77     19.7      145.   175. 
##  8 5 Gea… 8 Cyl…    15.0    301.    264.   3.17     15.4      326.   300. 
##  9 3 Gea… (All)     10.4    120.     97.   2.46     16.1      326.   176. 
## 10 4 Gea… (All)     17.8     71.1    52.   1.62     24.5      123.    89.5
## 11 5 Gea… (All)     15.0     95.1    91.   1.51     21.4      202.   196. 
## 12 (All)  4 Cyl…    21.4     71.1    52.   1.51     26.7      105.    82.6
## 13 (All)  6 Cyl…    17.8    145.    105.   2.62     19.7      183.   122. 
## 14 (All)  8 Cyl…    10.4    276.    150.   3.17     15.1      353.   209. 
## 15 (All)  (All)     10.4     71.1    52.   1.51     20.1      231.   147. 
## # ... with 5 more variables: wt_mean <dbl>, mpg_max <dbl>, disp_max <dbl>,
## #   hp_max <dbl>, wt_max <dbl>
```


# `spread_each` - Spread Multiple Values

The `spread_each` function works similar to the `tidyr::spread` function 
but works for spreading out multiple values.

### Example


```r
mtcars %>% 
    count(gear, cyl) %>% 
    group_by(gear) %>%
    mutate(pct = n/sum(n)) %>% 
    spread_each(cyl, n, pct)
```

```
## # A tibble: 3 x 8
## # Groups:   "gear" [1]
##    gear `4.n` `4.pct` `6.n` `6.pct` `8.n` `8.pct` `"gear"`
##   <dbl> <int>   <dbl> <int>   <dbl> <int>   <dbl> <chr>   
## 1    3.     1  0.0667     2   0.133    12   0.800 gear    
## 2    4.     8  0.667      4   0.333    NA  NA     gear    
## 3    5.     2  0.400      1   0.200     2   0.400 gear
```


# `.T` - Simple text specification

Similar to lazy loading of the tidyverse, the `.T` function provides
a simple way to designate arguments should be interpreted as text.
The limiting factor is that the text must be syntactically correct, 
however it come in very helpful.

### Example


```r
.T(first, second, 'a third argument with spaces')
```

```
## [1] "first"                        "second"                      
## [3] "a third argument with spaces"
```


# Inline functions

The `short` package also provides many inline shortcuts.

## Text concatenations

* `%\\n%` - concatenate with a line break
* `%<<%` - concatenate with a space
* `%<<<%` - concatenate without space.

## Pattern Matching

* `%~%` - Matches the lhs pattern
* `%!~%` - Does not match the lhs pattern

## Other

* `%or%` - Similar to the `rlang` or `purrr` function `%||%` but checks
    on `length` rather than `is.null` so will give the right hand 
    side for length 0 vectors as well.
* `%inherits%` - inline version of `inherits`
* `%is a%` - alternate inline version of `inherits`


# String Filtering

There are two convenience function for filtering character vectors, 
`sift` and the filter out version `seive`.


```r
# limit to Mercs
mtcars %>% rownames() %>% sift("^Merc")
```

```
## [1] "Merc 240D"   "Merc 230"    "Merc 280"    "Merc 280C"   "Merc 450SE" 
## [6] "Merc 450SL"  "Merc 450SLC"
```

```r
# Filter out everything starting with M
mtcars %>% rownames() %>% sieve("^M")
```

```
##  [1] "Datsun 710"          "Hornet 4 Drive"      "Hornet Sportabout"  
##  [4] "Valiant"             "Duster 360"          "Cadillac Fleetwood" 
##  [7] "Lincoln Continental" "Chrysler Imperial"   "Fiat 128"           
## [10] "Honda Civic"         "Toyota Corolla"      "Toyota Corona"      
## [13] "Dodge Challenger"    "AMC Javelin"         "Camaro Z28"         
## [16] "Pontiac Firebird"    "Fiat X1-9"           "Porsche 914-2"      
## [19] "Lotus Europa"        "Ford Pantera L"      "Ferrari Dino"       
## [22] "Volvo 142E"
```
