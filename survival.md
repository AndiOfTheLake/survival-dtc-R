---
title: "Survival Analysis in R"
author: "Andi"
Last updated: "04 June, 2021"
output: 
  html_document: 
    keep_md: yes
---




```r
# {r, echo = FALSE, results='hide'}
# if we used both 'echo=TRUE' and 'results=hide' the pipe would not work properly
# if we used 'echo = FALSE' and 'results=hide' we would have only messages (i.e. attaching package) If we don't want them we set 'error = FALSE', 'warning = FALSE', and 'message = FALSE'.
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
# package for analysis
library(survival)

# package for visualization
library(survminer)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: ggpubr
```

```r
# time to death of 686 breast cancer patients
data(GBSG2, package = "TH.data")

# time to re-employment of 3343 unemployed patients
data(UnempDur, package = "Ecdat")
```

# Introducing the GBSG2 dataset


```r
# Check out the help page for this dataset
# help(GBSG2, package = "TH.data")

# Look at the summary of the dataset
summary(GBSG2)
```

```
##  horTh          age        menostat       tsize        tgrade   
##  no :440   Min.   :21.00   Pre :290   Min.   :  3.00   I  : 81  
##  yes:246   1st Qu.:46.00   Post:396   1st Qu.: 20.00   II :444  
##            Median :53.00              Median : 25.00   III:161  
##            Mean   :53.05              Mean   : 29.33            
##            3rd Qu.:61.00              3rd Qu.: 35.00            
##            Max.   :80.00              Max.   :120.00            
##      pnodes         progrec           estrec             time       
##  Min.   : 1.00   Min.   :   0.0   Min.   :   0.00   Min.   :   8.0  
##  1st Qu.: 1.00   1st Qu.:   7.0   1st Qu.:   8.00   1st Qu.: 567.8  
##  Median : 3.00   Median :  32.5   Median :  36.00   Median :1084.0  
##  Mean   : 5.01   Mean   : 110.0   Mean   :  96.25   Mean   :1124.5  
##  3rd Qu.: 7.00   3rd Qu.: 131.8   3rd Qu.: 114.00   3rd Qu.:1684.8  
##  Max.   :51.00   Max.   :2380.0   Max.   :1144.00   Max.   :2659.0  
##       cens       
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.4359  
##  3rd Qu.:1.0000  
##  Max.   :1.0000
```

## Digging into the GBSG2 dataset 1



```r
# Count censored and uncensored data
num_cens <- table(GBSG2$cens)
num_cens
```

```
## 
##   0   1 
## 387 299
```

```r
# Create barplot of censored and uncensored data
barplot(num_cens)
```

![](survival_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Using the Surv() function for GBSG2


```r
# Create Surv-Object
sobj <- Surv(GBSG2$time, GBSG2$cens)

# Look at 10 first elements
sobj[1:10]
```

```
##  [1] 1814  2018   712  1807   772   448  2172+ 2161+  471  2014+
```

```r
GBSG2$time[1:10]
```

```
##  [1] 1814 2018  712 1807  772  448 2172 2161  471 2014
```

```r
GBSG2$cens[1:10]
```

```
##  [1] 1 1 1 1 1 1 0 0 1 0
```

```r
# Look at summary
summary(sobj)
```

```
##       time            status      
##  Min.   :   8.0   Min.   :0.0000  
##  1st Qu.: 567.8   1st Qu.:0.0000  
##  Median :1084.0   Median :0.0000  
##  Mean   :1124.5   Mean   :0.4359  
##  3rd Qu.:1684.8   3rd Qu.:1.0000  
##  Max.   :2659.0   Max.   :1.0000
```

```r
# Look at structure
str(sobj)
```

```
##  'Surv' num [1:686, 1:2] 1814  2018   712  1807   772   448  2172+ 2161+  471  2014+ ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : chr [1:2] "time" "status"
##  - attr(*, "type")= chr "right"
```

## The UnempDur dataset


```r
# Load the UnempDur data
data(UnempDur, package = "Ecdat")

# Count censored and uncensored data
cens_employ_ft <- table(UnempDur$censor1)
cens_employ_ft
```

```
## 
##    0    1 
## 2270 1073
```

```r
# Create barplot of censored and uncensored data
barplot(cens_employ_ft)
```

![](survival_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# Create Surv-Object
sobj <- Surv(UnempDur$spell, UnempDur$censor1)

# Look at 10 first elements
sobj[1:10]
```

```
##  [1]  5  13  21   3   9+ 11+  1+  3   7   5+
```

