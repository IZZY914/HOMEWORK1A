Untitled
================

``` r
load("~/Documents/izzy work/BRFSS2022_rev.RData")
```

``` r
attach(brfss22)
```

``` r
hist(SLEPTIM1[(SLEPTIM1 >5) & (SLEPTIM1 < 9)])
```

![](HOMEWORK1A_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
summary(SEXVAR)
```

    ##   Male Female 
    ## 209239 235893

``` r
summary(BIRTHSEX)
```

    ##   male sex at birth female sex at birth                NA's 
    ##               37441               41456              366235

``` r
summary(GENHLTH)
```

    ##            Excellent            Very good                 Good 
    ##                71878               148444               143598 
    ##                 Fair                 Poor Dont know - Not Sure 
    ##                60273                19741                  810 
    ##              Refused                 NA's 
    ##                  385                    3

``` r
summary(EDUCA)
```

    ##                   Never attended school or only kindergarten 
    ##                                                          676 
    ##                              Grades 1 through 8 (Elementary) 
    ##                                                         8381 
    ##                       Grades 9 through 11 (Some high school) 
    ##                                                        16954 
    ##                       Grade 12 or GED (High school graduate) 
    ##                                                       108990 
    ## College 1 year to 3 years (Some college or technical school) 
    ##                                                       120252 
    ##                   College 4 years or more (College graduate) 
    ##                                                       187496 
    ##                                                      Refused 
    ##                                                         2378 
    ##                                                         NA's 
    ##                                                            5

``` r
library(plyr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::arrange()   masks plyr::arrange()
    ## ✖ purrr::compact()   masks plyr::compact()
    ## ✖ dplyr::count()     masks plyr::count()
    ## ✖ dplyr::desc()      masks plyr::desc()
    ## ✖ dplyr::failwith()  masks plyr::failwith()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::id()        masks plyr::id()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::mutate()    masks plyr::mutate()
    ## ✖ dplyr::rename()    masks plyr::rename()
    ## ✖ dplyr::summarise() masks plyr::summarise()
    ## ✖ dplyr::summarize() masks plyr::summarize()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
summary(X_AGEG5YR)
```

    ##              Age 18 to 24              Age 25 to 29              Age 30 to 34 
    ##                     26941                     21990                     25807 
    ##              Age 35 to 39              Age 40 to 44              Age 45 to 49 
    ##                     28526                     29942                     28531 
    ##              Age 50 to 54              Age 55 to 59              Age 60 to 64 
    ##                     33644                     36821                     44511 
    ##              Age 65 to 69              Age 70 to 74              Age 75 to 79 
    ##                     47099                     43472                     32518 
    ##           Age 80 or older Dont know/Refused/Missing 
    ##                     36251                      9079

``` r
summary(INCOME3)
```

    ##                  Household income less than $10,000 
    ##                                               10341 
    ##    Less than $15,000 ($10,000 to less than $15,000) 
    ##                                               11031 
    ##   Less than $20,000 ($15,000 to less than $20,000)  
    ##                                               14300 
    ##   Less than $25,000 ($20,000 to less than $25,000)  
    ##                                               20343 
    ##   Less than $35,000 ($25,000 to less than $35,000)  
    ##                                               42294 
    ##   Less than $50,000 ($35,000 to less than $50,000)  
    ##                                               46831 
    ##    Less than $75,000 ($50,000 to less than $75,000) 
    ##                                               59148 
    ##  Less than $100,000 ($75,000 to less than $100,000) 
    ##                                               48436 
    ## Less than $150,000 ($100,000 to less than $150,000) 
    ##                                               50330 
    ## Less than $200,000 ($150,000 to less than $200,000) 
    ##                                               22553 
    ##                                    $200,000 or more 
    ##                                               23478 
    ##                                  Dont know/Not sure 
    ##                                               36114 
    ##                                             Refused 
    ##                                               47001 
    ##                                                NA's 
    ##                                               12932

``` r
ddply(brfss22, .(INCOME3), summarize, mean = round(mean(SLEPTIM1, na.rm = TRUE), 2), sd = round(sd(SLEPTIM1, na.rm = TRUE), 2), n_obsv = length(is.na(SLEPTIM1) == FALSE) )
```

    ##                                                INCOME3 mean   sd n_obsv
    ## 1                   Household income less than $10,000 6.90 2.26  10341
    ## 2     Less than $15,000 ($10,000 to less than $15,000) 6.94 2.04  11031
    ## 3    Less than $20,000 ($15,000 to less than $20,000)  6.99 1.96  14300
    ## 4    Less than $25,000 ($20,000 to less than $25,000)  7.02 1.83  20343
    ## 5    Less than $35,000 ($25,000 to less than $35,000)  7.02 1.66  42294
    ## 6    Less than $50,000 ($35,000 to less than $50,000)  7.03 1.50  46831
    ## 7     Less than $75,000 ($50,000 to less than $75,000) 7.00 1.36  59148
    ## 8   Less than $100,000 ($75,000 to less than $100,000) 7.01 1.27  48436
    ## 9  Less than $150,000 ($100,000 to less than $150,000) 6.99 1.19  50330
    ## 10 Less than $200,000 ($150,000 to less than $200,000) 6.98 1.16  22553
    ## 11                                    $200,000 or more 7.03 1.17  23478
    ## 12                                  Dont know/Not sure 7.12 1.76  36114
    ## 13                                             Refused 7.11 1.42  47001
    ## 14                                                <NA> 6.99 1.63  12932

``` r
ddply(brfss22, .(INCOME3), summarize, sleep90th = quantile(SLEPTIM1,probs = 0.9, na.rm = TRUE), sleep10th = quantile(SLEPTIM1,probs = 0.1, na.rm = TRUE), n_obs = length(is.na(SLEPTIM1) == FALSE) )
```

    ##                                                INCOME3 sleep90th sleep10th
    ## 1                   Household income less than $10,000         9         4
    ## 2     Less than $15,000 ($10,000 to less than $15,000)         9         4
    ## 3    Less than $20,000 ($15,000 to less than $20,000)          9         5
    ## 4    Less than $25,000 ($20,000 to less than $25,000)          9         5
    ## 5    Less than $35,000 ($25,000 to less than $35,000)          9         5
    ## 6    Less than $50,000 ($35,000 to less than $50,000)          8         5
    ## 7     Less than $75,000 ($50,000 to less than $75,000)         8         6
    ## 8   Less than $100,000 ($75,000 to less than $100,000)         8         6
    ## 9  Less than $150,000 ($100,000 to less than $150,000)         8         6
    ## 10 Less than $200,000 ($150,000 to less than $200,000)         8         6
    ## 11                                    $200,000 or more         8         6
    ## 12                                  Dont know/Not sure         9         5
    ## 13                                             Refused         8         6
    ## 14                                                <NA>         8         5
    ##    n_obs
    ## 1  10341
    ## 2  11031
    ## 3  14300
    ## 4  20343
    ## 5  42294
    ## 6  46831
    ## 7  59148
    ## 8  48436
    ## 9  50330
    ## 10 22553
    ## 11 23478
    ## 12 36114
    ## 13 47001
    ## 14 12932

``` r
table(GENHLTH,SEXVAR)
```

    ##                       SEXVAR
    ## GENHLTH                 Male Female
    ##   Excellent            36008  35870
    ##   Very good            69033  79411
    ##   Good                 67482  76116
    ##   Fair                 26974  33299
    ##   Poor                  9156  10585
    ##   Dont know - Not Sure   385    425
    ##   Refused                201    184

``` r
xtabs(~GENHLTH + SEXVAR)
```

    ##                       SEXVAR
    ## GENHLTH                 Male Female
    ##   Excellent            36008  35870
    ##   Very good            69033  79411
    ##   Good                 67482  76116
    ##   Fair                 26974  33299
    ##   Poor                  9156  10585
    ##   Dont know - Not Sure   385    425
    ##   Refused                201    184

``` r
prop.table(table(GENHLTH,SEXVAR))
```

    ##                       SEXVAR
    ## GENHLTH                        Male       Female
    ##   Excellent            0.0808934039 0.0805833814
    ##   Very good            0.1550853797 0.1783999694
    ##   Good                 0.1516009966 0.1709976209
    ##   Fair                 0.0605981637 0.0748075277
    ##   Poor                 0.0205693181 0.0237796234
    ##   Dont know - Not Sure 0.0008649178 0.0009547794
    ##   Refused              0.0004515545 0.0004133633

``` r
mean(SLEPTIM1[(EDUCA == "College 4 years or more (College graduate)")], na.rm = TRUE)
```

    ## [1] 7.070843
