# Econometrics with R
# Chapter 10 Regression with Panel Data ----------------------------------------
library(AER)
library(plm)
library(stargazer)
library(tidyverse)
library(ggplot2)


# 10.1 Panel Data --------------------------------------------------------------

# load the packagees and the dataset
# library(AER)
# library(plm)
data(Fatalities)
# pdata.frame() declares the data as panel data.
Fatalities <- pdata.frame(Fatalities, index = c("state", "year"))


# obtain the dimension and inspect the structure
is.data.frame(Fatalities)
#> [1] TRUE
dim(Fatalities)
#> [1] 336  34


str(Fatalities)
#> Classes 'pdata.frame' and 'data.frame':  336 obs. of  34 variables:
#>  $ state       : Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ year        : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ spirits     : 'pseries' Named num  1.37 1.36 1.32 1.28 1.23 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ unemp       : 'pseries' Named num  14.4 13.7 11.1 8.9 9.8 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ income      : 'pseries' Named num  10544 10733 11109 11333 11662 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ emppop      : 'pseries' Named num  50.7 52.1 54.2 55.3 56.5 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ beertax     : 'pseries' Named num  1.54 1.79 1.71 1.65 1.61 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ baptist     : 'pseries' Named num  30.4 30.3 30.3 30.3 30.3 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ mormon      : 'pseries' Named num  0.328 0.343 0.359 0.376 0.393 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ drinkage    : 'pseries' Named num  19 19 19 19.7 21 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ dry         : 'pseries' Named num  25 23 24 23.6 23.5 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ youngdrivers: 'pseries' Named num  0.212 0.211 0.211 0.211 0.213 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ miles       : 'pseries' Named num  7234 7836 8263 8727 8953 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ breath      : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ jail        : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 2 2 2 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ service     : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 2 2 2 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ fatal       : 'pseries' Named int  839 930 932 882 1081 1110 1023 724 675 869 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ nfatal      : 'pseries' Named int  146 154 165 146 172 181 139 131 112 149 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ sfatal      : 'pseries' Named int  99 98 94 98 119 114 89 76 60 81 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ fatal1517   : 'pseries' Named int  53 71 49 66 82 94 66 40 40 51 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ nfatal1517  : 'pseries' Named int  9 8 7 9 10 11 8 7 7 8 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ fatal1820   : 'pseries' Named int  99 108 103 100 120 127 105 81 83 118 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ nfatal1820  : 'pseries' Named int  34 26 25 23 23 31 24 16 19 34 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ fatal2124   : 'pseries' Named int  120 124 118 114 119 138 123 96 80 123 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ nfatal2124  : 'pseries' Named int  32 35 34 45 29 30 25 36 17 33 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ afatal      : 'pseries' Named num  309 342 305 277 361 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ pop         : 'pseries' Named num  3942002 3960008 3988992 4021008 4049994 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ pop1517     : 'pseries' Named num  209000 202000 197000 195000 204000 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ pop1820     : 'pseries' Named num  221553 219125 216724 214349 212000 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ pop2124     : 'pseries' Named num  290000 290000 288000 284000 263000 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ milestot    : 'pseries' Named num  28516 31032 32961 35091 36259 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ unempus     : 'pseries' Named num  9.7 9.6 7.5 7.2 7 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ emppopus    : 'pseries' Named num  57.8 57.9 59.5 60.1 60.7 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  $ gsp         : 'pseries' Named num  -0.0221 0.0466 0.0628 0.0275 0.0321 ...
#>   ..- attr(*, "names")= chr [1:336] "al-1982" "al-1983" "al-1984" "al-1985" ...
#>   ..- attr(*, "index")=Classes 'pindex' and 'data.frame':    336 obs. of  2 variables:
#>   .. ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   .. ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...
#>  - attr(*, "index")=Classes 'pindex' and 'data.frame':   336 obs. of  2 variables:
#>   ..$ state: Factor w/ 48 levels "al","az","ar",..: 1 1 1 1 1 1 1 2 2 2 ...
#>   ..$ year : Factor w/ 7 levels "1982","1983",..: 1 2 3 4 5 6 7 1 2 3 ...


# list the first few observations
head(Fatalities)
#>         state year spirits unemp   income   emppop  beertax baptist  mormon
#> al-1982    al 1982    1.37  14.4 10544.15 50.69204 1.539379 30.3557 0.32829
#> al-1983    al 1983    1.36  13.7 10732.80 52.14703 1.788991 30.3336 0.34341
#> al-1984    al 1984    1.32  11.1 11108.79 54.16809 1.714286 30.3115 0.35924
#> al-1985    al 1985    1.28   8.9 11332.63 55.27114 1.652542 30.2895 0.37579
#> al-1986    al 1986    1.23   9.8 11661.51 56.51450 1.609907 30.2674 0.39311
#> al-1987    al 1987    1.18   7.8 11944.00 57.50988 1.560000 30.2453 0.41123
#>         drinkage     dry youngdrivers    miles breath jail service fatal nfatal
#> al-1982    19.00 25.0063     0.211572 7233.887     no   no      no   839    146
#> al-1983    19.00 22.9942     0.210768 7836.348     no   no      no   930    154
#> al-1984    19.00 24.0426     0.211484 8262.990     no   no      no   932    165
#> al-1985    19.67 23.6339     0.211140 8726.917     no   no      no   882    146
#> al-1986    21.00 23.4647     0.213400 8952.854     no   no      no  1081    172
#> al-1987    21.00 23.7924     0.215527 9166.302     no   no      no  1110    181
#>         sfatal fatal1517 nfatal1517 fatal1820 nfatal1820 fatal2124 nfatal2124
#> al-1982     99        53          9        99         34       120         32
#> al-1983     98        71          8       108         26       124         35
#> al-1984     94        49          7       103         25       118         34
#> al-1985     98        66          9       100         23       114         45
#> al-1986    119        82         10       120         23       119         29
#> al-1987    114        94         11       127         31       138         30
#>          afatal     pop  pop1517  pop1820  pop2124 milestot unempus emppopus
#> al-1982 309.438 3942002 208999.6 221553.4 290000.1    28516     9.7     57.8
#> al-1983 341.834 3960008 202000.1 219125.5 290000.2    31032     9.6     57.9
#> al-1984 304.872 3988992 197000.0 216724.1 288000.2    32961     7.5     59.5
#> al-1985 276.742 4021008 194999.7 214349.0 284000.3    35091     7.2     60.1
#> al-1986 360.716 4049994 203999.9 212000.0 263000.3    36259     7.0     60.7
#> al-1987 368.421 4082999 204999.8 208998.5 258999.8    37426     6.2     61.5
#>                 gsp
#> al-1982 -0.02212476
#> al-1983  0.04655825
#> al-1984  0.06279784
#> al-1985  0.02748997
#> al-1986  0.03214295
#> al-1987  0.04897637


# summarize the variables 'state' and 'year'
summary(Fatalities[, c(1, 2)])
#>      state       year   
#>  al     :  7   1982:48  
#>  az     :  7   1983:48  
#>  ar     :  7   1984:48  
#>  ca     :  7   1985:48  
#>  co     :  7   1986:48  
#>  ct     :  7   1987:48  
#>  (Other):294   1988:48


# define the fatality rate
# Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000
Fatalities <- Fatalities %>% 
    mutate(fatal_rate = fatal / pop * 10000)

# subset the data
# Fatalities1982 <- subset(Fatalities, year == "1982")
# Fatalities1988 <- subset(Fatalities, year == "1988")
Fatalities1982 <- Fatalities %>% 
    filter(year == "1982")
Fatalities1988 <- Fatalities %>%
    filter(year == "1988")


# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  2.01038    0.14957 13.4408   <2e-16 ***
#> beertax      0.14846    0.13261  1.1196   0.2687    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)  1.85907    0.11461 16.2205 < 2.2e-16 ***
#> beertax      0.43875    0.12786  3.4314  0.001279 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# plot the observations and add the estimated regression line for 1982 data
# plot(x = as.double(Fatalities1982$beertax), 
#      y = as.double(Fatalities1982$fatal_rate), 
#      xlab = "Beer tax (in 1988 dollars)",
#      ylab = "Fatality rate (fatalities per 10000)",
#      main = "Traffic Fatality Rates and Beer Taxes in 1982",
#      ylim = c(0, 4.5),
#      pch = 20, 
#      col = "steelblue")

# abline(fatal1982_mod, lwd = 1.5, col="darkred")
# legend("topright",lty=1,col="darkred","Estimated Regression Line")
ggplot(Fatalities1982, aes(x = beertax, y = fatal_rate)) +
    geom_point(color = "steelblue") +
    geom_abline(aes(intercept = coef(fatal1982_mod)[1], 
                    slope = coef(fatal1982_mod)[2],
                    color = "Estimated Regression Line")) +
    scale_color_manual(name = NULL, 
                       values = c("Estimated Regression Line" = "darkred")) +
    labs(x = "Beer tax (in 1988 dollars)",
         y = "Fatality rate (fatalities per 10000)",
         title = "Traffic Fatality Rates and Beer Taxes in 1982") +
    ylim(0, 4.5) +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))



# plot observations and add estimated regression line for 1988 data
# plot(x = as.double(Fatalities1988$beertax), 
#      y = as.double(Fatalities1988$fatal_rate), 
#      xlab = "Beer tax (in 1988 dollars)",
#      ylab = "Fatality rate (fatalities per 10000)",
#      main = "Traffic Fatality Rates and Beer Taxes in 1988",
#      ylim = c(0, 4.5),
#      pch = 20, 
#      col = "steelblue")
# 
# abline(fatal1988_mod, lwd = 1.5,col="darkred")
# legend("bottomright",lty=1,col="darkred","Estimated Regression Line")
ggplot(Fatalities1988, aes(x = beertax, y = fatal_rate)) +
    geom_point(color = "steelblue") +
    geom_abline(aes(intercept = coef(fatal1988_mod)[1], 
                    slope = coef(fatal1988_mod)[2],
                    color = "Estimated Regression Line")) +
    scale_color_manual(name = NULL, 
                       values = c("Estimated Regression Line" = "darkred")) +
    labs(x = "Beer tax (in 1988 dollars)",
         y = "Fatality rate (fatalities per 10000)",
         title = "Traffic Fatality Rates and Beer Taxes in 1988") +
    ylim(0, 4.5) +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0))


# 10.2 Panel Data with Two Time Periods: “Before and After” Comparisons --------

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)  -0.072037   0.065355 -1.1022 0.276091   
#> diff_beertax -1.040973   0.355006 -2.9323 0.005229 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# plot the differenced data
# plot(x = as.double(diff_beertax), 
#      y = as.double(diff_fatal_rate), 
#      xlab = "Change in beer tax (in 1988 dollars)",
#      ylab = "Change in fatality rate (fatalities per 10000)",
#      main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
#      cex.main=1,
#      xlim = c(-0.6, 0.6),
#      ylim = c(-1.5, 1),
#      pch = 20, 
#      col = "steelblue")

# add the regression line to plot
# abline(fatal_diff_mod, lwd = 1.5,col="darkred")
#add legend
# legend("topright",lty=1,col="darkred","Estimated Regression Line")
ggplot(data = data.frame(diff_beertax, diff_fatal_rate), 
       aes(x = diff_beertax, y = diff_fatal_rate)) +
    geom_point(color = "steelblue") +
    geom_abline(aes(intercept = coef(fatal_diff_mod)[1], 
                    slope = coef(fatal_diff_mod)[2],
                    color = "Estimated Regression Line")) +
    scale_color_manual(name = NULL, 
                       values = c("Estimated Regression Line" = "darkred")) +
    labs(x = "Change in beer tax (in 1988 dollars)",
         y = "Change in fatality rate (fatalities per 10000)",
         title = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988") +
    xlim(-0.6, 0.6) +
    ylim(-1.5, 1) +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))


# compute mean fatality rate over all states for all time periods
mean(Fatalities$fatal_rate)
#> [1] 2.040444


# 10.3 Fixed Effects Regression ------------------------------------------------

fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod
#> 
#> Call:
#> lm(formula = fatal_rate ~ beertax + state - 1, data = Fatalities)
#> 
#> Coefficients:
#> beertax  stateal  stateaz  statear  stateca  stateco  statect  statede  
#> -0.6559   3.4776   2.9099   2.8227   1.9682   1.9933   1.6154   2.1700  
#> statefl  statega  stateid  stateil  statein  stateia  stateks  stateky  
#>  3.2095   4.0022   2.8086   1.5160   2.0161   1.9337   2.2544   2.2601  
#> statela  stateme  statemd  statema  statemi  statemn  statems  statemo  
#>  2.6305   2.3697   1.7712   1.3679   1.9931   1.5804   3.4486   2.1814  
#> statemt  statene  statenv  statenh  statenj  statenm  stateny  statenc  
#>  3.1172   1.9555   2.8769   2.2232   1.3719   3.9040   1.2910   3.1872  
#> statend  stateoh  stateok  stateor  statepa  stateri  statesc  statesd  
#>  1.8542   1.8032   2.9326   2.3096   1.7102   1.2126   4.0348   2.4739  
#> statetn  statetx  stateut  statevt  stateva  statewa  statewv  statewi  
#>  2.6020   2.5602   2.3137   2.5116   2.1874   1.8181   2.5809   1.7184  
#> statewy  
#>  3.2491


# obtain demeaned data
# fatal_demeaned <- with(Fatalities,
#                        data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
#                                   beertax = beertax - ave(beertax, state)))
fatal_demeaned <- Fatalities %>%
    group_by(state) %>%
    transmute(fatal_rate = fatal_rate - mean(fatal_rate),
              beertax = beertax - mean(beertax)) %>%
    ungroup()


# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = fatal_demeaned))
#> Call:
#> lm(formula = fatal_rate ~ beertax - 1, data = fatal_demeaned)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.58696 -0.08284 -0.00127  0.07955  0.89780 
# 
#> Coefficients:
#>         Estimate Std. Error t value Pr(>|t|)    
#> beertax  -0.6559     0.1739  -3.772 0.000191 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#>
#> Residual standard error: 0.1757 on 335 degrees of freedom
#> Multiple R-squared:  0.04074,	Adjusted R-squared:  0.03788 
#> F-statistic: 14.23 on 1 and 335 DF,  p-value: 0.0001913


# install and load the 'plm' package
## install.packages("plm")
# library(plm)


# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")


coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>         Estimate Std. Error t value Pr(>|t|)  
#> beertax -0.65587    0.28880  -2.271  0.02388 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 10.4 Regression with Time Fixed Effects --------------------------------------

# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod
#> 
#> Call:
#> lm(formula = fatal_rate ~ beertax + state + year - 1, data = Fatalities)
#> 
#> Coefficients:
#>  beertax   stateal   stateaz   statear   stateca   stateco   statect   statede  
#> -0.63998   3.51137   2.96451   2.87284   2.02618   2.04984   1.67125   2.22711  
#>  statefl   statega   stateid   stateil   statein   stateia   stateks   stateky  
#>  3.25132   4.02300   2.86242   1.57287   2.07123   1.98709   2.30707   2.31659  
#>  statela   stateme   statemd   statema   statemi   statemn   statems   statemo  
#>  2.67772   2.41713   1.82731   1.42335   2.04488   1.63488   3.49146   2.23598  
#>  statemt   statene   statenv   statenh   statenj   statenm   stateny   statenc  
#>  3.17160   2.00846   2.93322   2.27245   1.43016   3.95748   1.34849   3.22630  
#>  statend   stateoh   stateok   stateor   statepa   stateri   statesc   statesd  
#>  1.90762   1.85664   2.97776   2.36597   1.76563   1.26964   4.06496   2.52317  
#>  statetn   statetx   stateut   statevt   stateva   statewa   statewv   statewi  
#>  2.65670   2.61282   2.36165   2.56100   2.23618   1.87424   2.63364   1.77545  
#>  statewy  year1983  year1984  year1985  year1986  year1987  year1988  
#>  3.30791  -0.07990  -0.07242  -0.12398  -0.03786  -0.05090  -0.05180

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>         Estimate Std. Error t value Pr(>|t|)  
#> beertax -0.63998    0.35015 -1.8277  0.06865 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# check the class of 'state' and 'year'
class(Fatalities$state)
#> [1] "pseries" "factor"
class(Fatalities$year)
#> [1] "pseries" "factor"


# 10.5 The Fixed Effects Regression Assumptions and Standard Errors for Fixed Effects Regression ----

# check class of the model object
class(fatal_tefe_lm_mod)
#> [1] "lm"

# obtain a summary based on heteroskedasticity-robust standard errors 
# (no adjustment for heteroskedasticity only)
coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]
#>   Estimate Std. Error    t value   Pr(>|t|) 
#> -0.6399800  0.2547149 -2.5125346  0.0125470

# check class of the (plm) model object
class(fatal_tefe_mod)
#> [1] "plm"        "panelmodel"

# obtain a summary based on clustered standard errors 
# (adjustment for autocorrelation + heteroskedasticity)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>         Estimate Std. Error t value Pr(>|t|)  
#> beertax -0.63998    0.35015 -1.8277  0.06865 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 10.6 Drunk Driving Laws and Traffic Deaths -----------------------------------

# discretize the minimum legal drinking age
# Fatalities$drinkagec <- cut(Fatalities$drinkage,
#                             breaks = 18:22, 
#                             include.lowest = TRUE, 
#                             right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
# Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandatory jail or community service?
# Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
#                                              labels = c("no", "yes")))
Fatalities <- Fatalities %>%
    mutate(drinkagec = cut(drinkage, breaks = 18:22, 
                           include.lowest = TRUE, right = FALSE),
           drinkagec = relevel(drinkagec, "[21,22]"),
           punish = ifelse(jail == "yes" | service == "yes", "yes", "no"))

# the set of observations on all variables for 1982 and 1988
# fatal_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]
fatal_1982_1988 <- Fatalities %>%
    filter(year == 1982 | year == 1988)


# estimate all seven models
fat_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fat_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fat_mod3 <- plm(fatal_rate ~ beertax + state + year,
                index = c("state","year"),
                model = "within",
                effect = "twoways", 
                data = Fatalities)

fat_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                + punish + miles + unemp + log(income), 
                index = c("state", "year"),
                model = "within",
                effect = "twoways",
                data = Fatalities)

fat_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                + punish + miles,
                index = c("state", "year"),
                model = "within",
                effect = "twoways",
                data = Fatalities)

fat_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                + punish + miles + unemp + log(income), 
                index = c("state", "year"),
                model = "within",
                effect = "twoways",
                data = Fatalities)

fat_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                + punish + miles + unemp + log(income), 
                index = c("state", "year"),
                model = "within",
                effect = "twoways",
                data = fatal_1982_1988)


# library(stargazer)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(fat_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fat_mod7, type = "HC1"))))

# generate the table
stargazer(fat_mod1, 
          fat_mod2, 
          fat_mod3, 
          fat_mod4,
          fat_mod5,
          fat_mod6,
          fat_mod7,
          digits = 3,
          header = FALSE,
          # type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities
                                                      due to Drunk Driving",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          type = "html", out = "output/table_CH10_1.html")


# test if legal drinking age has no explanatory power
linearHypothesis(fat_mod4,
                 test = "F",
                 c("drinkagec[18,19)=0", "drinkagec[19,20)=0", "drinkagec[20,21)"), 
                 vcov. = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> drinkagec[18,19) = 0
#> drinkagec[19,20) = 0
#> drinkagec[20,21) = 0
#> 
#> Model 1: restricted model
#> Model 2: fatal_rate ~ beertax + state + year + drinkagec + punish + miles + 
#>     unemp + log(income)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    276                 
#> 2    273  3 0.3782 0.7688


# test if economic indicators have no explanatory power
linearHypothesis(fat_mod4, 
                 test = "F",
                 c("log(income)", "unemp"), 
                 vcov. = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> log(income) = 0
#> unemp = 0
#> 
#> Model 1: restricted model
#> Model 2: fatal_rate ~ beertax + state + year + drinkagec + punish + miles + 
#>     unemp + log(income)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1    275                        
#> 2    273  2 31.577 4.609e-13 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 10.7 Exercises ---------------------------------------------------------------

# 1. The Guns Dataset
# attach the `AER` package and load the `Guns` dataset
# library(AER)  
data("Guns")  

# obtain an overview over the dataset
summary(Guns)

# verify that `Guns` is a balanced panel
years  <- length(levels(Guns$year))
states <- length(levels(Guns$state))
years*states == nrow(Guns)


# 2. Strict or Loose? Gun Laws and the Effect on Crime I
model <- lm(log(violent) ~ law, data = Guns)
# estimate a model with state fixed effects using plm()
model_se <- plm(log(violent) ~ law, 
                data = Guns, 
                index = c("state", "year"), 
                model = "within")

# print a summary using robust standard errors
coeftest(model_se, vcov. = vcovHC, type = "HC1")

# test whether the state fixed effects are jointly significant from zero
pFtest(model_se, model)
#>         F test for individual effects
#> 
#> data:  log(violent) ~ law
#> F = 260.5, df1 = 50, df2 = 1121, p-value < 2.2e-16
#> alternative hypothesis: significant effects


# 3. Strict or Loose? Gun Laws and the Effect on Crime II
# estimate a model with state and time fixed effects using plm()
model_sete <- plm(log(violent) ~ law, 
                  data = Guns, 
                  index = c("state", "year"), 
                  model = "within", 
                  effect = "twoways")

# print a summary using robust standard errors
coeftest(model_sete, vcov. = vcovHC, type = "HC1")

# test whether state and time fixed effects are jointly significant from zero
pFtest(model_sete, model)


# 4. Strict or Loose? Gun Laws and the Effect on Crime III
# estimate the extended model
model_sete_ext <- plm(log(violent) ~ law + prisoners + density + income + 
                          population + afam + cauc + male, 
                      data = Guns, 
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

# print a summary using robust standard errors
coeftest(model_sete_ext, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>               Estimate  Std. Error t value Pr(>|t|)
#> lawyes     -2.7994e-02  3.9933e-02 -0.7010   0.4834
#> prisoners   7.5994e-05  2.0388e-04  0.3727   0.7094
#> density    -9.1555e-02  1.2148e-01 -0.7537   0.4512
#> income      9.5859e-07  1.6176e-05  0.0593   0.9528
#> population -4.7545e-03  1.4936e-02 -0.3183   0.7503
#> afam        2.9186e-02  4.8587e-02  0.6007   0.5482
#> cauc        9.2500e-03  2.3299e-02  0.3970   0.6914
#> male        7.3326e-02  5.1463e-02  1.4248   0.1545


# 5. Fixed Effects Regression - Two Time Periods
Guns78 <- Guns %>% 
    filter(year == "1978")
Guns84 <- Guns %>%
    filter(year == "1984")

# create the necessary variables to estimate the first model
diff_logv <- log(Guns84$violent/Guns78$violent)
# diff_law  <- Guns84$law - Guns78$law
diff_law <- ifelse(Guns84$law == "yes", 1, 0) - 
    ifelse(Guns78$law == "yes", 1, 0)

# estimate the first model using the differenced data
mod_diff <- lm(diff_logv ~ diff_law - 1)
coef_diff <- round(mod_diff$coef, 4)

# estimate the second model using plm()
mod_plm <- plm(log(violent) ~ law, data = Guns, subset = (year == "1978" | year == "1984"), index = c("state", "year"), model = "within")
coef_plm <- round(mod_plm$coef, 4)  
# verify that both estimates are numerically identical
coef_diff == coef_plm
#> diff_law
#>     TRUE