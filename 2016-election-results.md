2016 Election Results analysis using dplyr and ggplot2

I have used the following datasets to analyze the US 2016 election
results by county.

US\_County\_Level\_Presidential\_Results\_08-16.csv.bz2
county\_data.csv.bz2

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(gmodels)
library(rpart)
library(ggmap)
library(fiftystater)
library(choroplethr)
```

    ## Loading required package: acs

    ## Loading required package: stringr

    ## Loading required package: XML

    ## 
    ## Attaching package: 'acs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:base':
    ## 
    ##     apply

``` r
library(maps)
library(pscl)
```

    ## Classes and Methods for R developed in the
    ## Political Science Computational Laboratory
    ## Department of Political Science
    ## Stanford University
    ## Simon Jackman
    ## hurdle and zeroinfl functions by Achim Zeileis

``` r
library(mfx)
```

    ## Loading required package: sandwich

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## Loading required package: betareg

``` r
#Read the data
pres_results <- read.csv("US_County_Level_Presidential_Results_08-16.csv.bz2")
county_data <-read.csv("county_data.csv.bz2")

#Statistics for presidential results
str(pres_results)
```

    ## 'data.frame':    3112 obs. of  14 variables:
    ##  $ fips_code : int  26041 48295 1127 48389 56017 20043 37183 37147 48497 21207 ...
    ##  $ county    : Factor w/ 1845 levels "Abbeville County",..: 468 973 1728 1381 779 495 1725 1306 1809 1432 ...
    ##  $ total_2008: int  19064 1256 28652 3077 2546 3564 442245 74884 20639 7475 ...
    ##  $ dem_2008  : int  9974 155 7420 1606 619 1115 250891 40501 4471 1569 ...
    ##  $ gop_2008  : int  8763 1093 20722 1445 1834 2372 187001 33927 15973 5779 ...
    ##  $ oth_2008  : int  327 8 510 26 93 77 4353 456 195 127 ...
    ##  $ total_2012: int  18043 1168 28497 2867 2495 3369 526805 76814 20692 7907 ...
    ##  $ dem_2012  : int  8330 119 6551 1649 523 885 286939 40701 3219 1445 ...
    ##  $ gop_2012  : int  9533 1044 21633 1185 1894 2397 232933 35534 17178 6346 ...
    ##  $ oth_2012  : int  180 5 313 33 78 87 6933 579 295 116 ...
    ##  $ total_2016: int  18467 1322 29243 3184 2535 3366 510940 78264 24661 8171 ...
    ##  $ dem_2016  : int  6431 135 4486 1659 400 584 298353 40967 3412 1093 ...
    ##  $ gop_2016  : int  11112 1159 24208 1417 1939 2601 193607 35191 20655 6863 ...
    ##  $ oth_2016  : int  924 28 549 108 196 181 18980 2106 594 215 ...

``` r
#Statistics for county data
str(county_data)
```

    ## 'data.frame':    3193 obs. of  116 variables:
    ##  $ SUMLEV               : int  40 50 50 50 50 50 50 50 50 50 ...
    ##  $ REGION               : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ DIVISION             : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE                : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY               : int  0 1 3 5 7 9 11 13 15 17 ...
    ##  $ STNAME               : Factor w/ 51 levels "Alabama","Alaska",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CTYNAME              : Factor w/ 1927 levels "Abbeville County",..: 10 87 94 105 154 169 230 240 253 302 ...
    ##  $ CENSUS2010POP        : int  4779736 54571 182265 27457 22915 57322 10914 20947 118572 34215 ...
    ##  $ ESTIMATESBASE2010    : int  4780131 54571 182265 27457 22919 57324 10911 20946 118586 34170 ...
    ##  $ POPESTIMATE2010      : int  4785492 54742 183199 27348 22861 57376 10892 20938 118468 34101 ...
    ##  $ POPESTIMATE2011      : int  4799918 55255 186653 27326 22736 57707 10722 20848 117736 34006 ...
    ##  $ POPESTIMATE2012      : int  4815960 55027 190403 27132 22645 57772 10654 20665 117208 34084 ...
    ##  $ POPESTIMATE2013      : int  4829479 54792 195147 26938 22501 57746 10576 20330 116475 34123 ...
    ##  $ POPESTIMATE2014      : int  4843214 54977 199745 26763 22511 57621 10712 20283 115837 33996 ...
    ##  $ POPESTIMATE2015      : int  4853875 55035 203690 26270 22561 57676 10455 20126 115285 34043 ...
    ##  $ POPESTIMATE2016      : int  4863300 55416 208563 25965 22643 57704 10362 19998 114611 33843 ...
    ##  $ NPOPCHG_2010         : int  5361 171 934 -109 -58 52 -19 -8 -118 -69 ...
    ##  $ NPOPCHG_2011         : int  14426 513 3454 -22 -125 331 -170 -90 -732 -95 ...
    ##  $ NPOPCHG_2012         : int  16042 -228 3750 -194 -91 65 -68 -183 -528 78 ...
    ##  $ NPOPCHG_2013         : int  13519 -235 4744 -194 -144 -26 -78 -335 -733 39 ...
    ##  $ NPOPCHG_2014         : int  13735 185 4598 -175 10 -125 136 -47 -638 -127 ...
    ##  $ NPOPCHG_2015         : int  10661 58 3945 -493 50 55 -257 -157 -552 47 ...
    ##  $ NPOPCHG_2016         : int  9425 381 4873 -305 82 28 -93 -128 -674 -200 ...
    ##  $ BIRTHS2010           : int  14231 151 516 70 44 183 39 65 318 81 ...
    ##  $ BIRTHS2011           : int  59689 636 2188 335 266 744 169 276 1384 401 ...
    ##  $ BIRTHS2012           : int  59066 614 2092 300 245 711 122 241 1357 393 ...
    ##  $ BIRTHS2013           : int  57939 574 2161 283 258 646 131 240 1309 406 ...
    ##  $ BIRTHS2014           : int  58906 640 2214 265 254 620 124 250 1317 425 ...
    ##  $ BIRTHS2015           : int  59034 636 2237 258 259 689 115 248 1289 423 ...
    ##  $ BIRTHS2016           : int  58556 631 2274 253 266 663 114 236 1237 419 ...
    ##  $ DEATHS2010           : int  11086 154 532 128 34 132 52 60 313 80 ...
    ##  $ DEATHS2011           : int  48817 507 1825 318 277 568 132 261 1326 441 ...
    ##  $ DEATHS2012           : int  48372 560 1882 293 239 593 117 272 1357 475 ...
    ##  $ DEATHS2013           : int  50845 582 1903 295 281 584 120 261 1411 452 ...
    ##  $ DEATHS2014           : int  49693 575 1989 313 250 587 115 288 1392 454 ...
    ##  $ DEATHS2015           : int  51407 475 2080 319 207 634 108 268 1427 465 ...
    ##  $ DEATHS2016           : int  52405 494 2113 314 237 622 111 241 1441 476 ...
    ##  $ NATURALINC2010       : int  3145 -3 -16 -58 10 51 -13 5 5 1 ...
    ##  $ NATURALINC2011       : int  10872 129 363 17 -11 176 37 15 58 -40 ...
    ##  $ NATURALINC2012       : int  10694 54 210 7 6 118 5 -31 0 -82 ...
    ##  $ NATURALINC2013       : int  7094 -8 258 -12 -23 62 11 -21 -102 -46 ...
    ##  $ NATURALINC2014       : int  9213 65 225 -48 4 33 9 -38 -75 -29 ...
    ##  $ NATURALINC2015       : int  7627 161 157 -61 52 55 7 -20 -138 -42 ...
    ##  $ NATURALINC2016       : int  6151 137 161 -61 29 41 3 -5 -204 -57 ...
    ##  $ INTERNATIONALMIG2010 : int  1360 33 66 2 2 5 7 0 6 7 ...
    ##  $ INTERNATIONALMIG2011 : int  4816 18 183 -4 10 -3 19 2 39 31 ...
    ##  $ INTERNATIONALMIG2012 : int  4695 2 176 -10 13 18 16 5 63 19 ...
    ##  $ INTERNATIONALMIG2013 : int  4179 2 209 -9 13 29 9 7 26 17 ...
    ##  $ INTERNATIONALMIG2014 : int  4732 6 239 -8 18 32 10 8 24 19 ...
    ##  $ INTERNATIONALMIG2015 : int  5110 8 257 -6 18 36 9 8 29 18 ...
    ##  $ INTERNATIONALMIG2016 : int  4738 7 243 -5 18 38 9 8 27 18 ...
    ##  $ DOMESTICMIG2010      : int  866 134 867 -54 -69 -3 -13 -11 -124 -71 ...
    ##  $ DOMESTICMIG2011      : int  -1416 321 2731 -31 -123 104 -242 -105 -788 -85 ...
    ##  $ DOMESTICMIG2012      : int  414 -294 3333 -192 -111 -67 -90 -160 -591 138 ...
    ##  $ DOMESTICMIG2013      : int  1619 -253 4178 -190 -148 -94 -92 -312 -647 70 ...
    ##  $ DOMESTICMIG2014      : int  420 118 3759 -113 2 -161 117 -8 -518 -117 ...
    ##  $ DOMESTICMIG2015      : int  -3114 -154 3492 -440 2 -81 -280 -150 -457 49 ...
    ##  $ DOMESTICMIG2016      : int  -864 228 4046 -248 34 -65 -101 -127 -462 -155 ...
    ##  $ NETMIG2010           : int  2226 167 933 -52 -67 2 -6 -11 -118 -64 ...
    ##  $ NETMIG2011           : int  3400 339 2914 -35 -113 101 -223 -103 -749 -54 ...
    ##  $ NETMIG2012           : int  5109 -292 3509 -202 -98 -49 -74 -155 -528 157 ...
    ##  $ NETMIG2013           : int  5798 -251 4387 -199 -135 -65 -83 -305 -621 87 ...
    ##  $ NETMIG2014           : int  5152 124 3998 -121 20 -129 127 0 -494 -98 ...
    ##  $ NETMIG2015           : int  1996 -146 3749 -446 20 -45 -271 -142 -428 67 ...
    ##  $ NETMIG2016           : int  3874 235 4289 -253 52 -27 -92 -119 -435 -137 ...
    ##  $ RESIDUAL2010         : int  -10 7 17 1 -1 -1 0 -2 -5 -6 ...
    ##  $ RESIDUAL2011         : int  154 45 177 -4 -1 54 16 -2 -41 -1 ...
    ##  $ RESIDUAL2012         : int  239 10 31 1 1 -4 1 3 0 3 ...
    ##  $ RESIDUAL2013         : int  627 24 99 17 14 -23 -6 -9 -10 -2 ...
    ##  $ RESIDUAL2014         : int  -630 -4 375 -6 -14 -29 0 -9 -69 0 ...
    ##  $ RESIDUAL2015         : int  1038 43 39 14 -22 45 7 5 14 22 ...
    ##  $ RESIDUAL2016         : int  -600 9 423 9 1 14 -4 -4 -35 -6 ...
    ##  $ GQESTIMATESBASE2010  : int  116185 455 2307 3193 2224 489 1690 333 2933 458 ...
    ##  $ GQESTIMATES2010      : int  116214 455 2307 3193 2224 489 1690 333 2934 458 ...
    ##  $ GQESTIMATES2011      : int  115521 455 2263 3379 2224 489 1690 333 2883 458 ...
    ##  $ GQESTIMATES2012      : int  115697 455 2242 3388 2225 489 1776 333 2959 458 ...
    ##  $ GQESTIMATES2013      : int  116984 455 2296 3388 2224 489 1717 333 2813 458 ...
    ##  $ GQESTIMATES2014      : int  119189 455 2333 3352 2241 489 1758 333 2796 458 ...
    ##  $ GQESTIMATES2015      : int  120174 455 2339 3198 2255 489 1656 333 2773 458 ...
    ##  $ GQESTIMATES2016      : int  119659 455 2341 3186 2252 489 1653 333 2776 458 ...
    ##  $ RBIRTH2011           : num  12.5 11.6 11.8 12.3 11.7 ...
    ##  $ RBIRTH2012           : num  12.3 11.1 11.1 11 10.8 ...
    ##  $ RBIRTH2013           : num  12 10.5 11.2 10.5 11.4 ...
    ##  $ RBIRTH2014           : num  12.18 11.66 11.21 9.87 11.29 ...
    ##  $ RBIRTH2015           : num  12.18 11.56 11.09 9.73 11.49 ...
    ##  $ RBIRTH2016           : num  12.05 11.43 11.03 9.69 11.77 ...
    ##  $ RDEATH2011           : num  10.19 9.22 9.87 11.63 12.15 ...
    ##  $ RDEATH2012           : num  10.06 10.16 9.98 10.76 10.53 ...
    ##  $ RDEATH2013           : num  10.54 10.6 9.87 10.91 12.45 ...
    ##  $ RDEATH2014           : num  10.3 10.5 10.1 11.7 11.1 ...
    ##  $ RDEATH2015           : num  10.6 8.64 10.31 12.03 9.19 ...
    ##  $ RDEATH2016           : num  10.79 8.95 10.25 12.02 10.49 ...
    ##  $ RNATURALINC2011      : num  2.268 2.346 1.963 0.622 -0.482 ...
    ##  $ RNATURALINC2012      : num  2.224 0.979 1.114 0.257 0.264 ...
    ##  $ RNATURALINC2013      : num  1.471 -0.146 1.338 -0.444 -1.019 ...
    ##  $ RNATURALINC2014      : num  1.905 1.184 1.14 -1.788 0.178 ...
    ##  $ RNATURALINC2015      : num  1.573 2.927 0.778 -2.3 2.307 ...
    ##  $ RNATURALINC2016      : num  1.266 2.481 0.781 -2.336 1.283 ...
    ##  $ RINTERNATIONALMIG2011: num  1.005 0.327 0.99 -0.146 0.439 ...
    ##   [list output truncated]

``` r
county_data$fips_code <-paste(county_data$STATE*1000 + county_data$COUNTY)

pres_county_data <- merge(pres_results,county_data,by="fips_code",all.x=TRUE)

pres_county_data_1 <-subset(pres_county_data, select = c(fips_code,REGION, DIVISION,STATE,COUNTY,STNAME,CTYNAME,POPESTIMATE2016,total_2016,dem_2016,gop_2016,oth_2016))
```

I have tidied the data. Merged these datasets, retained only more
interesting variables, compute additional variables I found interesting.
First, I read the two datasets. In order to merge the two datasets, I
had to first compute the fips code for the county\_data dataframe. To
compute it, in the same format as given in the other dataset, I
multiplied the STATE variable with 1000 and added the COUNTY variable to
it, giving us a 5 digit FIPS code. I then merged the two datasets using
a left\_join, in order to preserve county data, and to avoid too many
NAs in the dataset that would be coerced in, on using a full\_join. I
then created a final dataset, using a subset of only the variables, that
I thought were pertinent to problem 1.

``` r
head(pres_county_data_1)
```

    ##   fips_code REGION DIVISION STATE COUNTY  STNAME        CTYNAME
    ## 1      1001      3        6     1      1 Alabama Autauga County
    ## 2      1003      3        6     1      3 Alabama Baldwin County
    ## 3      1005      3        6     1      5 Alabama Barbour County
    ## 4      1007      3        6     1      7 Alabama    Bibb County
    ## 5      1009      3        6     1      9 Alabama  Blount County
    ## 6      1011      3        6     1     11 Alabama Bullock County
    ##   POPESTIMATE2016 total_2016 dem_2016 gop_2016 oth_2016
    ## 1           55416      24661     5908    18110      643
    ## 2          208563      94090    18409    72780     2901
    ## 3           25965      10390     4848     5431      111
    ## 4           22643       8748     1874     6733      141
    ## 5           57704      25384     2150    22808      426
    ## 6           10362       4701     3530     1139       32

``` r
tail(pres_county_data_1)
```

    ##      fips_code REGION DIVISION STATE COUNTY  STNAME           CTYNAME
    ## 3107     56035      4        8    56     35 Wyoming   Sublette County
    ## 3108     56037      4        8    56     37 Wyoming Sweetwater County
    ## 3109     56039      4        8    56     39 Wyoming      Teton County
    ## 3110     56041      4        8    56     41 Wyoming      Uinta County
    ## 3111     56043      4        8    56     43 Wyoming   Washakie County
    ## 3112     56045      4        8    56     45 Wyoming     Weston County
    ##      POPESTIMATE2016 total_2016 dem_2016 gop_2016 oth_2016
    ## 3107            9769       4297      644     3409      244
    ## 3108           44165      16661     3233    12153     1275
    ## 3109           23191      12176     7313     3920      943
    ## 3110           20773       8053     1202     6154      697
    ## 3111            8235       3715      532     2911      272
    ## 3112            7236       3334      294     2898      142

``` r
str(pres_county_data_1)
```

    ## 'data.frame':    3112 obs. of  12 variables:
    ##  $ fips_code      : int  1001 1003 1005 1007 1009 1011 1013 1015 1017 1019 ...
    ##  $ REGION         : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ DIVISION       : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE          : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY         : int  1 3 5 7 9 11 13 15 17 19 ...
    ##  $ STNAME         : Factor w/ 51 levels "Alabama","Alaska",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CTYNAME        : Factor w/ 1927 levels "Abbeville County",..: 87 94 105 154 169 230 240 253 302 324 ...
    ##  $ POPESTIMATE2016: int  55416 208563 25965 22643 57704 10362 19998 114611 33843 25725 ...
    ##  $ total_2016     : int  24661 94090 10390 8748 25384 4701 8685 47376 13778 10503 ...
    ##  $ dem_2016       : int  5908 18409 4848 1874 2150 3530 3716 13197 5763 1524 ...
    ##  $ gop_2016       : int  18110 72780 5431 6733 22808 1139 4891 32803 7803 8809 ...
    ##  $ oth_2016       : int  643 2901 111 141 426 32 78 1376 212 170 ...

``` r
summary(pres_county_data_1)
```

    ##    fips_code         REGION         DIVISION         STATE      
    ##  Min.   : 1001   Min.   :1.000   Min.   :1.000   Min.   : 1.00  
    ##  1st Qu.:19038   1st Qu.:2.000   1st Qu.:4.000   1st Qu.:19.00  
    ##  Median :29208   Median :3.000   Median :5.000   Median :29.00  
    ##  Mean   :30652   Mean   :2.656   Mean   :5.156   Mean   :30.54  
    ##  3rd Qu.:46006   3rd Qu.:3.000   3rd Qu.:7.000   3rd Qu.:46.00  
    ##  Max.   :56045   Max.   :4.000   Max.   :9.000   Max.   :56.00  
    ##                  NA's   :1       NA's   :1       NA's   :1      
    ##      COUNTY           STNAME                  CTYNAME    
    ##  Min.   :  1.0   Texas   : 254   Washington County:  30  
    ##  1st Qu.: 35.0   Georgia : 159   Jefferson County :  25  
    ##  Median : 78.0   Virginia: 133   Franklin County  :  24  
    ##  Mean   :103.2   Kentucky: 120   Jackson County   :  23  
    ##  3rd Qu.:133.0   Missouri: 115   Lincoln County   :  23  
    ##  Max.   :840.0   (Other) :2330   (Other)          :2986  
    ##  NA's   :1       NA's    :   1   NA's             :   1  
    ##  POPESTIMATE2016      total_2016         dem_2016          gop_2016     
    ##  Min.   :     113   Min.   :     64   Min.   :      4   Min.   :    57  
    ##  1st Qu.:   11194   1st Qu.:   4815   1st Qu.:   1165   1st Qu.:  3206  
    ##  Median :   26027   Median :  10930   Median :   3140   Median :  7113  
    ##  Mean   :  103623   Mean   :  40896   Mean   :  19561   Mean   : 19344  
    ##  3rd Qu.:   67968   3rd Qu.:  28664   3rd Qu.:   9535   3rd Qu.: 17392  
    ##  Max.   :10137915   Max.   :2314275   Max.   :1654626   Max.   :590465  
    ##  NA's   :1                                                              
    ##     oth_2016     
    ##  Min.   :     3  
    ##  1st Qu.:   165  
    ##  Median :   440  
    ##  Mean   :  1992  
    ##  3rd Qu.:  1394  
    ##  Max.   :117058  
    ## 

This dataset contains 3112 observations and 12 variables. These
variables are, 1. fips\_code: Federal Information Processing Standards
codes. 2. REGION: Census Region code 3. DIVISION: Census Division code
4. STATE: State FIPS code 5. COUNTY: County FIPS code 6. STNAME: State
name 7. CTYNAME: County name 8. POPESTIMATE2016:7/1/2016 resident total
population estimate 9. total\_2016:Total voting population in the county
10. dem\_2016: Votes for the democratic party in the county
11.oth\_2016: Votes for other parties in the county 12.gop\_2016: Votes
for the republican party in the county

I would like to analyse the population estimate and its association with
the election results, in order understand the popular vote, and possibly
adding more variables to the dataset as predictors for the election
outcome.

``` r
pres_county_data_1$percent_dem <- paste(pres_county_data_1$dem_2016*100/pres_county_data_1$total_2016)

ggplot(data=na.omit(pres_county_data_1),aes(x=as.factor(POPESTIMATE2016),y=as.numeric(percent_dem))) + geom_point(stat="identity")+labs(title= "Votes for democrats vs the county population",x =  "Population",y = "% of votes for democrats")
```

![](2016-election-results_files/figure-markdown_github/1.3-1.png)

On looking at the scatter plot above, we can see that there is a
positive association between votes for democrats and the county
population, as the plots are clustered on a slight incline.

I would like to create a map of percentage of votes for democrats.

``` r
 county_plot<-function(fips_code,percent_dem){
   library(choroplethr)
   temp  <- as.data.frame(list(region=as.numeric(fips_code),value=as.numeric(percent_dem)))
   county_choropleth(temp)
 }

county_plot(na.omit(pres_county_data_1$fips_code),pres_county_data_1$percent_dem)
```

    ## Warning in self$bind(): The following regions were missing and are being
    ## set to NA: 2050, 2105, 2122, 2150, 2164, 2180, 2188, 2240, 2090, 2198,
    ## 15005, 2100, 2170, 51515, 2016, 2060, 2290, 2282, 2070, 2110, 2130, 2185,
    ## 2195, 2220, 2230, 2020, 2068, 2013, 2261, 2270, 2275

![](2016-election-results_files/figure-markdown_github/1.4-1.png)

The map plots the % of votes for democrates in each county, wherein
darker the color of blue, more is the percentage of votes ofr the
democratic party. I used the library chloroplthr for simplicity, and to
allow for the usage of fips\_code, which would not have been possible if
using ggmaps.

I then created one more visualization regarding the election results.

``` r
#all_states <- map_data("state")
pres_county_data_1$STNAME <- tolower(pres_county_data_1$STNAME)
pres_county_data_1$CTYNAME <- tolower(pres_county_data_1$CTYNAME)  

#pres_county_data_1$region <- pres_county_data_1$STNAME
#pres_county_data_2 <- merge(pres_county_data_1,all_states,by="region")
#pres_county_data_2 <- subset(pres_county_data_2, select = -c(order))
#pres_county_data_2 <- distinct(pres_county_data_2,"region")

pres_county_data_1$m<-ifelse(pres_county_data_1$dem_2016>pres_county_data_1$gop_2016, 0,1)
pres_county_data_1 <- na.omit(pres_county_data_1)

df <- as.data.frame(list(region=as.numeric(pres_county_data_1$fips_code),
                                  value=pres_county_data_1$m))

county_choropleth(na.omit(unique(df),num_colors = 2,
                          legend="Parties",
                          title="Counties that voted GOP and DEM in 2016"))
```

    ## Warning in min(xx[xx > upper]): no non-missing arguments to min; returning
    ## Inf

    ## Warning in self$bind(): The following regions were missing and are being
    ## set to NA: 2050, 2105, 2122, 46113, 2150, 2164, 2180, 2188, 2240, 2090,
    ## 2198, 15005, 2100, 2170, 51515, 2016, 2060, 2290, 2282, 2070, 2110, 2130,
    ## 2185, 2195, 2220, 2230, 2020, 2068, 2013, 2261, 2270, 2275

![](2016-election-results_files/figure-markdown_github/1.5-1.png)

The above map aims to show the results of the 2016 elections, wherein
the lighter blue colour signifies that the democratic party had more
votes, and the darker blue signifies that the repulican party had more
votes. This is at the county level.

I then tried to estimate the probability that a county voted for
democrats in 2016 elections (i.e., the probability that democrats
received more votes than GOP).

The variables that I would consider important are: 1. fips\_code 2.
CTYNAME 3. STNAME 4. dem\_2008 5. gop\_2008 6. oth\_2008 7. dem\_2012 8.
gop\_2012 9. oth\_2012 10. POPESTIMATE2016 11. POPESTIMATE2012 12.
NPOPCHG\_2010 13. NPOPCHG\_2011 14. NPOPCHG\_2012 15. NPOPCHG\_2013 16.
NPOPCHG\_2014 17. NPOPCHG\_2015 18. NPOPCHG\_2016

These are the variables I consider relevant as predictor variables, in
order to understand how the population estimates, the change in resident
population, as well as the results of previous elections would effect
the 2016 elections, especially due to the tendency of the democrats
gaining the popular vote, thus making the variables possibly good
predictors.

I estimated a logistic regression model to explain the probability of
voting democratic as a function of the variables I considered relevant.

``` r
pres_county_model_data <-subset(pres_county_data, select = c(fips_code,REGION, DIVISION,STATE,COUNTY,CTYNAME,STNAME, dem_2008, gop_2008, oth_2008, dem_2012, gop_2012, oth_2012, POPESTIMATE2016, POPESTIMATE2012, NPOPCHG_2010, NPOPCHG_2011, NPOPCHG_2012, NPOPCHG_2013,NPOPCHG_2014, NPOPCHG_2015, NPOPCHG_2016,total_2016,dem_2016,gop_2016,oth_2016))

str(pres_county_model_data)
```

    ## 'data.frame':    3112 obs. of  26 variables:
    ##  $ fips_code      : int  1001 1003 1005 1007 1009 1011 1013 1015 1017 1019 ...
    ##  $ REGION         : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ DIVISION       : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE          : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY         : int  1 3 5 7 9 11 13 15 17 19 ...
    ##  $ CTYNAME        : Factor w/ 1927 levels "Abbeville County",..: 87 94 105 154 169 230 240 253 302 324 ...
    ##  $ STNAME         : Factor w/ 51 levels "Alabama","Alaska",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ dem_2008       : int  6093 19386 5697 2299 3522 4011 4188 16334 6799 2306 ...
    ##  $ gop_2008       : int  17403 61271 5866 6262 20389 1391 5485 32348 8067 7298 ...
    ##  $ oth_2008       : int  145 756 67 83 356 13 36 560 90 141 ...
    ##  $ dem_2012       : int  6354 18329 5873 2200 2961 4058 4367 15500 6853 2126 ...
    ##  $ gop_2012       : int  17366 65772 5539 6131 20741 1250 5081 30272 7596 7494 ...
    ##  $ oth_2012       : int  189 887 47 60 278 10 35 468 113 141 ...
    ##  $ POPESTIMATE2016: int  55416 208563 25965 22643 57704 10362 19998 114611 33843 25725 ...
    ##  $ POPESTIMATE2012: int  55027 190403 27132 22645 57772 10654 20665 117208 34084 26017 ...
    ##  $ NPOPCHG_2010   : int  171 934 -109 -58 52 -19 -8 -118 -69 -9 ...
    ##  $ NPOPCHG_2011   : int  513 3454 -22 -125 331 -170 -90 -732 -95 96 ...
    ##  $ NPOPCHG_2012   : int  -228 3750 -194 -91 65 -68 -183 -528 78 -56 ...
    ##  $ NPOPCHG_2013   : int  -235 4744 -194 -144 -26 -78 -335 -733 39 57 ...
    ##  $ NPOPCHG_2014   : int  185 4598 -175 10 -125 136 -47 -638 -127 -130 ...
    ##  $ NPOPCHG_2015   : int  58 3945 -493 50 55 -257 -157 -552 47 -218 ...
    ##  $ NPOPCHG_2016   : int  381 4873 -305 82 28 -93 -128 -674 -200 -1 ...
    ##  $ total_2016     : int  24661 94090 10390 8748 25384 4701 8685 47376 13778 10503 ...
    ##  $ dem_2016       : int  5908 18409 4848 1874 2150 3530 3716 13197 5763 1524 ...
    ##  $ gop_2016       : int  18110 72780 5431 6733 22808 1139 4891 32803 7803 8809 ...
    ##  $ oth_2016       : int  643 2901 111 141 426 32 78 1376 212 170 ...

``` r
pres_county_model_data$win<-ifelse(pres_county_model_data$dem_2016>pres_county_model_data$gop_2016,1,0)

log_model_1 <- suppressWarnings(glm( formula = win~dem_2008+gop_2008+oth_2008+dem_2012+
                      gop_2012+oth_2012+POPESTIMATE2016+POPESTIMATE2012+
                      NPOPCHG_2010+NPOPCHG_2011+NPOPCHG_2012+
                      NPOPCHG_2013+NPOPCHG_2014+NPOPCHG_2015+NPOPCHG_2016, 
                    data = pres_county_model_data,family = binomial(link = "logit") ))


summary(log_model_1)
```

    ## 
    ## Call:
    ## glm(formula = win ~ dem_2008 + gop_2008 + oth_2008 + dem_2012 + 
    ##     gop_2012 + oth_2012 + POPESTIMATE2016 + POPESTIMATE2012 + 
    ##     NPOPCHG_2010 + NPOPCHG_2011 + NPOPCHG_2012 + NPOPCHG_2013 + 
    ##     NPOPCHG_2014 + NPOPCHG_2015 + NPOPCHG_2016, family = binomial(link = "logit"), 
    ##     data = pres_county_model_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.3244  -0.3513  -0.1822  -0.0138   6.2131  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.962e+00  9.299e-02 -21.095  < 2e-16 ***
    ## dem_2008         1.236e-04  5.241e-05   2.359 0.018345 *  
    ## gop_2008        -2.872e-04  6.589e-05  -4.359 1.31e-05 ***
    ## oth_2008        -9.341e-04  3.663e-04  -2.550 0.010783 *  
    ## dem_2012         4.821e-04  5.501e-05   8.764  < 2e-16 ***
    ## gop_2012        -4.794e-04  6.349e-05  -7.551 4.32e-14 ***
    ## oth_2012         2.358e-03  4.243e-04   5.558 2.73e-08 ***
    ## POPESTIMATE2016 -4.013e-04  2.074e-04  -1.935 0.053009 .  
    ## POPESTIMATE2012  4.148e-04  2.072e-04   2.002 0.045258 *  
    ## NPOPCHG_2010     7.349e-04  7.311e-04   1.005 0.314748    
    ## NPOPCHG_2011     9.631e-04  2.537e-04   3.796 0.000147 ***
    ## NPOPCHG_2012     5.621e-04  2.035e-04   2.763 0.005729 ** 
    ## NPOPCHG_2013     1.304e-03  3.149e-04   4.140 3.47e-05 ***
    ## NPOPCHG_2014     6.255e-05  4.352e-04   0.144 0.885719    
    ## NPOPCHG_2015     3.032e-04  4.215e-04   0.719 0.471955    
    ## NPOPCHG_2016            NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2696.3  on 3110  degrees of freedom
    ## Residual deviance: 1082.3  on 3096  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1112.3
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
pR2_1 <- pR2(log_model_1)
```

I experimented with a few different specifications.

``` r
#Model 2
log_model_2 <- suppressWarnings(glm( formula = win~dem_2008+gop_2008+dem_2012+
                      gop_2012+POPESTIMATE2016+POPESTIMATE2012+
                      NPOPCHG_2010+NPOPCHG_2011+NPOPCHG_2012+
                      NPOPCHG_2013+NPOPCHG_2014+NPOPCHG_2015+NPOPCHG_2016, 
                    data = pres_county_model_data,family = binomial(link = "logit") ))


summary(log_model_2)
```

    ## 
    ## Call:
    ## glm(formula = win ~ dem_2008 + gop_2008 + dem_2012 + gop_2012 + 
    ##     POPESTIMATE2016 + POPESTIMATE2012 + NPOPCHG_2010 + NPOPCHG_2011 + 
    ##     NPOPCHG_2012 + NPOPCHG_2013 + NPOPCHG_2014 + NPOPCHG_2015 + 
    ##     NPOPCHG_2016, family = binomial(link = "logit"), data = pres_county_model_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.3244  -0.3586  -0.1852  -0.0147   6.3462  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.888e+00  9.122e-02 -20.701  < 2e-16 ***
    ## dem_2008         1.378e-04  5.639e-05   2.445 0.014493 *  
    ## gop_2008        -2.610e-04  6.911e-05  -3.777 0.000159 ***
    ## dem_2012         4.693e-04  5.843e-05   8.031 9.65e-16 ***
    ## gop_2012        -4.759e-04  6.696e-05  -7.107 1.18e-12 ***
    ## POPESTIMATE2016 -1.866e-04  2.010e-04  -0.928 0.353299    
    ## POPESTIMATE2012  2.016e-04  2.010e-04   1.003 0.315861    
    ## NPOPCHG_2010     5.163e-04  7.097e-04   0.728 0.466889    
    ## NPOPCHG_2011     1.026e-03  2.420e-04   4.237 2.26e-05 ***
    ## NPOPCHG_2012     6.971e-04  1.939e-04   3.595 0.000324 ***
    ## NPOPCHG_2013     1.104e-03  3.195e-04   3.456 0.000548 ***
    ## NPOPCHG_2014    -3.418e-04  4.273e-04  -0.800 0.423697    
    ## NPOPCHG_2015     1.189e-04  4.136e-04   0.287 0.773816    
    ## NPOPCHG_2016            NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2696.3  on 3110  degrees of freedom
    ## Residual deviance: 1107.6  on 3098  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1133.6
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
pR2_2 <- pR2(log_model_2)

#Model 3
log_model_3 <- suppressWarnings(glm( formula = win~dem_2008+dem_2012
                      +POPESTIMATE2016+POPESTIMATE2012+
                      NPOPCHG_2010+NPOPCHG_2011+NPOPCHG_2012+
                      NPOPCHG_2013+NPOPCHG_2014+NPOPCHG_2015+NPOPCHG_2016, 
                    data = pres_county_model_data,family = binomial(link = "logit") ))


summary(log_model_3)
```

    ## 
    ## Call:
    ## glm(formula = win ~ dem_2008 + dem_2012 + POPESTIMATE2016 + POPESTIMATE2012 + 
    ##     NPOPCHG_2010 + NPOPCHG_2011 + NPOPCHG_2012 + NPOPCHG_2013 + 
    ##     NPOPCHG_2014 + NPOPCHG_2015 + NPOPCHG_2016, family = binomial(link = "logit"), 
    ##     data = pres_county_model_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.6496  -0.4391  -0.4088  -0.3266   4.1316  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -2.294e+00  7.351e-02 -31.209  < 2e-16 ***
    ## dem_2008         9.839e-05  2.460e-05   3.999 6.37e-05 ***
    ## dem_2012         6.356e-05  2.458e-05   2.585 0.009727 ** 
    ## POPESTIMATE2016 -3.159e-04  8.894e-05  -3.551 0.000383 ***
    ## POPESTIMATE2012  2.838e-04  8.797e-05   3.226 0.001257 ** 
    ## NPOPCHG_2010     9.751e-04  3.263e-04   2.988 0.002805 ** 
    ## NPOPCHG_2011     5.921e-04  1.365e-04   4.339 1.43e-05 ***
    ## NPOPCHG_2012     2.000e-04  1.361e-04   1.469 0.141736    
    ## NPOPCHG_2013    -1.042e-04  1.391e-04  -0.749 0.453841    
    ## NPOPCHG_2014     6.219e-04  1.929e-04   3.225 0.001261 ** 
    ## NPOPCHG_2015     2.915e-04  2.120e-04   1.375 0.169049    
    ## NPOPCHG_2016            NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2696.3  on 3110  degrees of freedom
    ## Residual deviance: 1837.8  on 3100  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1859.8
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
pR2_3 <- pR2(log_model_3)

#Model 4
log_model_4 <- suppressWarnings(glm( formula = win~dem_2008+dem_2012
                      +POPESTIMATE2016+POPESTIMATE2012+
                     +NPOPCHG_2011+NPOPCHG_2012
                     +NPOPCHG_2015+NPOPCHG_2016,
                    data = pres_county_model_data,family = binomial(link = "logit") ))


summary(log_model_4)
```

    ## 
    ## Call:
    ## glm(formula = win ~ dem_2008 + dem_2012 + POPESTIMATE2016 + POPESTIMATE2012 + 
    ##     +NPOPCHG_2011 + NPOPCHG_2012 + NPOPCHG_2015 + NPOPCHG_2016, 
    ##     family = binomial(link = "logit"), data = pres_county_model_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.8137  -0.4399  -0.4100  -0.3374   4.0816  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -2.305e+00  7.357e-02 -31.331  < 2e-16 ***
    ## dem_2008         8.860e-05  2.297e-05   3.857 0.000115 ***
    ## dem_2012         6.518e-05  2.290e-05   2.846 0.004424 ** 
    ## POPESTIMATE2016 -1.015e-04  7.346e-05  -1.382 0.166934    
    ## POPESTIMATE2012  7.214e-05  7.329e-05   0.984 0.324937    
    ## NPOPCHG_2011     7.600e-04  1.160e-04   6.551 5.71e-11 ***
    ## NPOPCHG_2012     1.520e-04  1.303e-04   1.167 0.243292    
    ## NPOPCHG_2015     1.323e-04  2.005e-04   0.660 0.509510    
    ## NPOPCHG_2016    -1.039e-04  1.020e-04  -1.019 0.308177    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2696.3  on 3110  degrees of freedom
    ## Residual deviance: 1853.8  on 3102  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1871.8
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
pR2_4 <- pR2(log_model_4)

pR2_1
```

    ##           llh       llhNull            G2      McFadden          r2ML 
    ##  -541.1333091 -1350.0014077  1617.7361970     0.5991609     0.4054825 
    ##          r2CU 
    ##     0.6989113

``` r
pR2_2
```

    ##           llh       llhNull            G2      McFadden          r2ML 
    ##  -553.7756932 -1350.0014077  1592.4514290     0.5897962     0.4006309 
    ##          r2CU 
    ##     0.6905487

``` r
pR2_3
```

    ##           llh       llhNull            G2      McFadden          r2ML 
    ##  -918.8943566 -1350.0014077   862.2141022     0.3193382     0.2420593 
    ##          r2CU 
    ##     0.4172263

``` r
pR2_3
```

    ##           llh       llhNull            G2      McFadden          r2ML 
    ##  -918.8943566 -1350.0014077   862.2141022     0.3193382     0.2420593 
    ##          r2CU 
    ##     0.4172263

The highest *p**s**e**u**d**o* − *R*<sup>2</sup> value is for the first
model that I ran, which included all the relevant predictors I had
considered. The value *p**s**e**u**d**o**R*<sup>2</sup> is 0.5992, which
means that the model can explain about 60% of the variability in the
predictor variables. It is seems to be a good fit.

Taking alpha value as 0.05, thus checking at 95% confidence, the
variables that are statistically significant are:

1.  dem\_2008
2.  gop\_2008
3.  oth\_2008
4.  dem\_2012
5.  gop\_2012
6.  oth\_2012
7.  POPESTIMATE2012
8.  NPOPCHG\_2011
9.  NPOPCHG\_2012 10.NPOPCHG\_2013

``` r
summary(log_model_1)
```

    ## 
    ## Call:
    ## glm(formula = win ~ dem_2008 + gop_2008 + oth_2008 + dem_2012 + 
    ##     gop_2012 + oth_2012 + POPESTIMATE2016 + POPESTIMATE2012 + 
    ##     NPOPCHG_2010 + NPOPCHG_2011 + NPOPCHG_2012 + NPOPCHG_2013 + 
    ##     NPOPCHG_2014 + NPOPCHG_2015 + NPOPCHG_2016, family = binomial(link = "logit"), 
    ##     data = pres_county_model_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.3244  -0.3513  -0.1822  -0.0138   6.2131  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.962e+00  9.299e-02 -21.095  < 2e-16 ***
    ## dem_2008         1.236e-04  5.241e-05   2.359 0.018345 *  
    ## gop_2008        -2.872e-04  6.589e-05  -4.359 1.31e-05 ***
    ## oth_2008        -9.341e-04  3.663e-04  -2.550 0.010783 *  
    ## dem_2012         4.821e-04  5.501e-05   8.764  < 2e-16 ***
    ## gop_2012        -4.794e-04  6.349e-05  -7.551 4.32e-14 ***
    ## oth_2012         2.358e-03  4.243e-04   5.558 2.73e-08 ***
    ## POPESTIMATE2016 -4.013e-04  2.074e-04  -1.935 0.053009 .  
    ## POPESTIMATE2012  4.148e-04  2.072e-04   2.002 0.045258 *  
    ## NPOPCHG_2010     7.349e-04  7.311e-04   1.005 0.314748    
    ## NPOPCHG_2011     9.631e-04  2.537e-04   3.796 0.000147 ***
    ## NPOPCHG_2012     5.621e-04  2.035e-04   2.763 0.005729 ** 
    ## NPOPCHG_2013     1.304e-03  3.149e-04   4.140 3.47e-05 ***
    ## NPOPCHG_2014     6.255e-05  4.352e-04   0.144 0.885719    
    ## NPOPCHG_2015     3.032e-04  4.215e-04   0.719 0.471955    
    ## NPOPCHG_2016            NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2696.3  on 3110  degrees of freedom
    ## Residual deviance: 1082.3  on 3096  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1112.3
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
suppressWarnings(logitmfx(formula = win~dem_2008+gop_2008+oth_2008+dem_2012+
                      gop_2012+oth_2012+POPESTIMATE2016+POPESTIMATE2012+
                      NPOPCHG_2010+NPOPCHG_2011+NPOPCHG_2012+
                      NPOPCHG_2013+NPOPCHG_2014+NPOPCHG_2015+NPOPCHG_2016, 
                    data = pres_county_model_data))
```

    ## Call:
    ## logitmfx(formula = win ~ dem_2008 + gop_2008 + oth_2008 + dem_2012 + 
    ##     gop_2012 + oth_2012 + POPESTIMATE2016 + POPESTIMATE2012 + 
    ##     NPOPCHG_2010 + NPOPCHG_2011 + NPOPCHG_2012 + NPOPCHG_2013 + 
    ##     NPOPCHG_2014 + NPOPCHG_2015 + NPOPCHG_2016, data = pres_county_model_data)
    ## 
    ## Marginal Effects:
    ##                       dF/dx   Std. Err.       z     P>|z|    
    ## dem_2008         2.8601e-05  1.2281e-05  2.3288 0.0198677 *  
    ## gop_2008        -6.6452e-05  1.5874e-05 -4.1863 2.835e-05 ***
    ## oth_2008        -2.1612e-04  8.6434e-05 -2.5004 0.0124051 *  
    ## dem_2012         1.1155e-04  1.3629e-05  8.1846 2.732e-16 ***
    ## gop_2012        -1.1092e-04  1.5436e-05 -7.1860 6.672e-13 ***
    ## oth_2012         5.4556e-04  1.0015e-04  5.4475 5.108e-08 ***
    ## POPESTIMATE2016 -9.2857e-05  4.7807e-05 -1.9423 0.0520970 .  
    ## POPESTIMATE2012  9.5985e-05  4.7777e-05  2.0090 0.0445327 *  
    ## NPOPCHG_2010     1.7005e-04  1.6912e-04  1.0055 0.3146726    
    ## NPOPCHG_2011     2.2285e-04  5.9163e-05  3.7667 0.0001655 ***
    ## NPOPCHG_2012     1.3006e-04  4.7945e-05  2.7127 0.0066747 ** 
    ## NPOPCHG_2013     3.0160e-04  7.3717e-05  4.0913 4.289e-05 ***
    ## NPOPCHG_2014     1.4473e-05  1.0064e-04  0.1438 0.8856582    
    ## NPOPCHG_2015     7.0144e-05  9.7474e-05  0.7196 0.4717573    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The coefficients can be interpretated as, 1. dem\_2008: For every one
unit change in dem\_2008, the log odds of win(by democrat party)
increases by 0.00012362

1.  gop\_2008: For every one unit change in gop\_2008, the log odds of
    win(by democrat party) decreases by 0.00028721

2.  oth\_2008: For every one unit change in gop\_2008, the log odds of
    win(by democrat party) decreases by 0.00093408

3.  dem\_2012: For every one unit change in gop\_2008, the log odds of
    win(by democrat party) increases by 0.00048210

4.  gop\_2012: For every one unit change in gop\_2008, the log odds of
    win(by democrat party) decreases by 0.00047941

5.  oth\_2012: For every one unit change in gop\_2008, the log odds of
    win(by democrat party) increases by 0.00235793

6.  POPESTIMATE2012: For every one unit change in gop\_2008, the log
    odds of win(by democrat party) increases by 0.00041485

7.  NPOPCHG\_2011: For every one unit change in gop\_2008, the log odds
    of win(by democrat party) increases by 0.00096315

8.  NPOPCHG\_2012: For every one unit change in gop\_2008, the log odds
    of win(by democrat party) increases by 0.00056212

10.NPOPCHG\_2013:For every one unit change in gop\_2008, the log odds of
win(by democrat party) increases by 0.00130353

Using Marginal effects: 1. POPESTIMATE2012: For every 1 unit increase in
the population in 2012, there is 0.000095985 increase in democratic
wins.

1.  NPOPCHG\_2011: For every 1 unit increase in change of population in
    2011, there is 0.000222847 increase in democratic wins.

2.  NPOPCHG\_2012: For every 1 unit increase in change of population in
    2012, there is 0.000130060 increase in democratic wins.

3.  NPOPCHG\_2013: For every 1 unit increase in change of population in
    2013, there is - 0.000301600 increase in democratic wins.

4.  dem\_2008: For every 1 unit increase in change of number of
    democratic votes in 2008, there is 0.000028601 increase in
    democratic wins.

5.  gop\_2008: For every 1 unit increase in change of number of gop
    votes in 2008, there is 0.000066452 decrease in democratic wins.

6.  oth\_2008: For every 1 unit increase in change of number of other
    party votes in 2008, there is 0.000216119 decrease in democratic
    wins.

7.  dem\_2012: For every 1 unit increase in change of number of
    democratic votes in 2012, there is 0.000111545 increase in
    democratic wins.

8.  gop\_2012: For every 1 unit increase in change of number of gop
    votes in 2012, there is 0.000110923 decrease in democratic wins.

9.  oth\_2012: For every 1 unit increase in change of number of other
    votes in 2012, there is 0.000545559 increase in democratic wins.

From this analysis, we can say that population of the county, population
changes in 2011,2012, 2013 and democratic votes in 2008, 2012 impacted
the democratic victory in 2016 positviely. Also, other third parties
seem to play a significant role in the democratic victory, although they
had a differential impact in 2008 and 2012.
