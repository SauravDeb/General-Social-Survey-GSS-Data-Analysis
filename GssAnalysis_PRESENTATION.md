Setup
-----

### Load packages

``` r
library(ggplot2)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.3

``` r
library(statsr)
```

### Load data

Make sure your data and R Markdown files are in the same directory. When loaded your data file will be called `gss`. Delete this note when before you submit your work.

``` r
load("gss.Rdata")
```

------------------------------------------------------------------------

Part 1: Data
------------

Since 1972, the General Social Survey (GSS) has been monitoring societal change and studying the growing complexity of American society. The GSS aims to gather data on contemporary American society in order to monitor and explain trends and constants in attitudes, behaviors, and attributes; to examine the structure and functioning of society in general as well as the role played by relevant subgroups; to compare the United States to other societies in order to place American society in comparative perspective and develop cross-national models of human society.

GSS questions cover a diverse range of issues including national spending priorities, marijuana use, crime and punishment, race relations, quality of life, confidence in institutions, and sexual behavior.

Since the GSS is a general survey involving random individuals belonging to random communities, it can hence be said that the data is generelizable in nature.

However since the GSS is not an experimental study, the concept of causalty can't be applied here.

------------------------------------------------------------------------

Part 2: Research question
-------------------------

Q. What has been the shift in the negative opinion among the people of the United States towards the concept of homosexual relationships throughout the years from 1989 to 2012? This will also help us discover if we can reach to a concrete evidence that solidifies this opinion shift in the 21st century(i.e 2000 onwards untill 2012).

We are all well acquainted to the fact that homosexuals, especially during most part of the time prior to the 21st century, were wrongly frowned upon and shamed by the society, who treated them as a diseased section of people. Moreover, homosexual relationships were even more disgusted and rebelled by wider sections of the society. In contrast to these situations in the previous century, the United States of America in 2003 granted LGBT(Lesbian, gay, bisexual, and transgender) rights and in 2015 legalised same-sex marriage. In this research topic, it would be interesting to record the shift in the negative opinion of the general public as the United States was preparing to open its arms to homosexuality and same sex relationships.

------------------------------------------------------------------------

Part 3: Exploratory data analysis
---------------------------------

``` r
gss <- gss %>% group_by(year) %>% mutate(homo_sex = as.character(homosex), obs_years = n())

dist_homo <- gss %>% filter(!is.na(homosex), homo_sex=="Always Wrong")

new <- dist_homo %>% group_by(year, obs_years) %>% summarise(cnt = n()) 

new <- new %>% group_by(year) %>% summarise(prop = cnt/obs_years)

#Summary statistics
new
```

    ## # A tibble: 24 × 2
    ##     year      prop
    ##    <int>     <dbl>
    ## 1   1973 0.7001330
    ## 2   1974 0.6704852
    ## 3   1976 0.6664443
    ## 4   1977 0.6823529
    ## 5   1980 0.6975477
    ## 6   1982 0.7118280
    ## 7   1984 0.7026477
    ## 8   1985 0.7288136
    ## 9   1987 0.7520616
    ## 10  1988 0.4861580
    ## # ... with 14 more rows

This summary table(new) stores the proportion of people in the US with negative opinion towards homosexual relationships i.e Always Wrong, since 1973 untill 2012.

``` r
#Plot
ggplot(new, aes(year, prop)) + geom_line() + geom_point() +
        labs(title = "Shift of negative opinion towards homosexual relationships throughout the years",
              x="Year", y="% People against homosexual relationships")
```

![](https://github.com/SauravDeb/General-Social-Survey-GSS-Data-Analysis/blob/master/opinionShift.png)

The plot above gives us a visualization of the shift in the proportion of people with negative opinions towards homosexual relationships i.e Always wrong, since 1973 untill 2012.

It can be clearly seen from the above plot that there was a larger proportion of people back during the period roughly between 1970 and 1985 opposing any relationships among homosexual individuals. This figure went down steeply since then as we move towards the inception of the 21st century. All over it can be said that there has been a decline in the proportion of people in the United States who are against homosexual relationships.

------------------------------------------------------------------------

Part 4: Inference
-----------------

This inference will provide useful insight if there is any concrete evidence that the opinion shift among the general public has changed between 2000 and 2012.

### Hypothesis

The hypothesis are as such:

#### *Null hypothesis*:

The opinion shift has nothing to do with the passing of years from 2000 till 2012 i.e the change in opinion is independent of the passing years.

#### *Alternate hypothesis*:

The opinion shift has everything to do with the passing of years from 2000 till 2012 i.e the change in opinion is dependent on the passing of years.

### Condition checking:

1.*Independence*: Within group independence is satisfied, also independence between groups is satisfied. Each case contributes to any 1 cell in our table.

2.*Sample size/skew*: Each cell must have atleast 5 expected cases.

### Methodology

Here we're using the **Chi-Square** test of independence as we're dealing with categorical variables with more than 2 categories. We'll be considering a significance level of 5% while testing our hypothesis.

### The "TEST"

``` r
new_ht <- gss %>% filter(year == 2000 | year == 2012) %>% select(year, homosex)

new_ht <- new_ht %>% filter(!is.na(homosex))

new_ht$homosex <- factor(new_ht$homosex)

table <- table(new_ht$year,new_ht$homosex)

#Contingency Table
table
```

    ##       
    ##        Always Wrong Almst Always Wrg Sometimes Wrong Not Wrong At All
    ##   2000          997               76             135              489
    ##   2012          567               37              82              553

``` r
#Performing the Chi-Square test of independence
chisq.test(table)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table
    ## X-squared = 79.036, df = 3, p-value < 2.2e-16

### **Result Interpretations**

It can be seen from the Chi-Square test of independence that the obtained **p-value is significantly less than our significance level of 5%**. Hence we can interpret that if in fact the opinion shift is independent of the passing of time from 2000 untill 2012, the probability of the same being different is signifcantly less than 0.0022.

Thus said, we can successfully reject our null hypothesis and say that we have obtained concrete evidence that the shift of opinion towards homosexual relationships in the United States is dependent on the passing of years from 2000 untill 2012.

**END**
