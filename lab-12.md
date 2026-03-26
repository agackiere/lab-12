Lab 12 - Smoking during pregnancy
================
Anaelle Gackiere
03-27-2026

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
library(skimr)

set.seed(51)
```

### Exercise 1

Which variables in this dataset are numeric, and what do their
distributions look like? Are there any outliers or extreme values that
might affect your analysis?

The variables that are numeric in ncbirths are female age, male age,
weeks, visits, gained, weight. Mature, premie, lowbirthweight, gender,
habit, marital, whitemom are categorical. Gained seems to have extreme
values (from 0 pounds to 85 pounds) and it has 27 NAs. fage also has
many NAs (171). There are some outliers for a lot of the variables, and
we can already tell that smoking status changes some of the
distributions.

``` r
data(ncbirths)

summary(ncbirths)
```

    ##       fage            mage            mature        weeks             premie   
    ##  Min.   :14.00   Min.   :13   mature mom :133   Min.   :20.00   full term:846  
    ##  1st Qu.:25.00   1st Qu.:22   younger mom:867   1st Qu.:37.00   premie   :152  
    ##  Median :30.00   Median :27                     Median :39.00   NA's     :  2  
    ##  Mean   :30.26   Mean   :27                     Mean   :38.33                  
    ##  3rd Qu.:35.00   3rd Qu.:32                     3rd Qu.:40.00                  
    ##  Max.   :55.00   Max.   :50                     Max.   :45.00                  
    ##  NA's   :171                                    NA's   :2                      
    ##      visits            marital        gained          weight      
    ##  Min.   : 0.0   not married:386   Min.   : 0.00   Min.   : 1.000  
    ##  1st Qu.:10.0   married    :613   1st Qu.:20.00   1st Qu.: 6.380  
    ##  Median :12.0   NA's       :  1   Median :30.00   Median : 7.310  
    ##  Mean   :12.1                     Mean   :30.33   Mean   : 7.101  
    ##  3rd Qu.:15.0                     3rd Qu.:38.00   3rd Qu.: 8.060  
    ##  Max.   :30.0                     Max.   :85.00   Max.   :11.750  
    ##  NA's   :9                        NA's   :27                      
    ##  lowbirthweight    gender          habit          whitemom  
    ##  low    :111    female:503   nonsmoker:873   not white:284  
    ##  not low:889    male  :497   smoker   :126   white    :714  
    ##                              NA's     :  1   NA's     :  2  
    ##                                                             
    ##                                                             
    ##                                                             
    ## 

``` r
skimr::skim(ncbirths)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | ncbirths |
| Number of rows                                   | 1000     |
| Number of columns                                | 13       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| factor                                           | 7        |
| numeric                                          | 6        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: factor**

| skim_variable  | n_missing | complete_rate | ordered | n_unique | top_counts         |
|:---------------|----------:|--------------:|:--------|---------:|:-------------------|
| mature         |         0 |             1 | FALSE   |        2 | you: 867, mat: 133 |
| premie         |         2 |             1 | FALSE   |        2 | ful: 846, pre: 152 |
| marital        |         1 |             1 | FALSE   |        2 | mar: 613, not: 386 |
| lowbirthweight |         0 |             1 | FALSE   |        2 | not: 889, low: 111 |
| gender         |         0 |             1 | FALSE   |        2 | fem: 503, mal: 497 |
| habit          |         1 |             1 | FALSE   |        2 | non: 873, smo: 126 |
| whitemom       |         2 |             1 | FALSE   |        2 | whi: 714, not: 284 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| fage | 171 | 0.83 | 30.26 | 6.76 | 14 | 25.00 | 30.00 | 35.00 | 55.00 | ▃▇▇▂▁ |
| mage | 0 | 1.00 | 27.00 | 6.21 | 13 | 22.00 | 27.00 | 32.00 | 50.00 | ▃▇▇▂▁ |
| weeks | 2 | 1.00 | 38.33 | 2.93 | 20 | 37.00 | 39.00 | 40.00 | 45.00 | ▁▁▁▇▂ |
| visits | 9 | 0.99 | 12.10 | 3.95 | 0 | 10.00 | 12.00 | 15.00 | 30.00 | ▂▇▇▁▁ |
| gained | 27 | 0.97 | 30.33 | 14.24 | 0 | 20.00 | 30.00 | 38.00 | 85.00 | ▂▇▅▁▁ |
| weight | 0 | 1.00 | 7.10 | 1.51 | 1 | 6.38 | 7.31 | 8.06 | 11.75 | ▁▁▇▇▁ |

``` r
ncbirths %>%
  select(fage, mage, weeks, visits, gained, weight, habit) %>%
  filter(!is.na(habit)) %>%
  pivot_longer(cols = c(fage, mage, weeks, visits, gained, weight),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = habit, y = value, fill = habit)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Numeric Variable Distributions by Smoking Habit",
       x = NULL, y = NULL) +
  theme(legend.position = "none")
```

    ## Warning: Removed 205 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](lab-12_files/figure-gfm/exercise1-code-1.png)<!-- -->

### Exercise 2

The mean of the weights of the white mom’s babies in the dataset is 7.25
pounds.

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom == "white")

mean(ncbirths_white$weight)
```

    ## [1] 7.250462

### Exercise 3

Are the criteria necessary for conducting simulation-based inference
satisfied? Briefly consider whether simulation-based inference (e.g.,
bootstrapping) is appropriate in this context by addressing the
following: Are observations in the sample independent of each other? Is
the sample size reasonably large? Does the shape of the distribution
pose any concerns (e.g., extreme skew or clustering)? Use graphical
summaries (histogram, boxplot) and/or numerical summaries to support
your answer. Refer back to your plots or output from Exercise 1.

The data is a random sample from NC birth records, so observations
should be independent. In terms of sample size, N = 714 is large enough
for bootstrapping since the threshold is typically n ≥ 30. The shape of
the distribution might pose some concerns; there is no extreme skew
(although there is a slight negative skew) but there may be some
clustering, since the tail of the distribution is essentially made up of
premies.

``` r
# graphical summaries
ggplot(ncbirths_white, aes(x = weight)) +
  geom_histogram(binwidth = 0.5)
```

![](lab-12_files/figure-gfm/exercise3-code-1.png)<!-- -->

``` r
ggplot(ncbirths_white, aes(x = weight, fill = premie)) +
  geom_histogram(binwidth = 0.5, alpha = 0.7)
```

![](lab-12_files/figure-gfm/exercise3-code-2.png)<!-- -->

``` r
ggplot(ncbirths_white, aes(y = weight)) +
  geom_boxplot()
```

![](lab-12_files/figure-gfm/exercise3-code-3.png)<!-- -->

``` r
# samplesize
nrow(ncbirths_white)
```

    ## [1] 714

### Exercise 4

Here’s the general procedure: - Take a bootstrap sample (with
replacement) from ncbirths_white. - Calculate the mean of this
bootstrapped resample. - Repeat these two steps many times (e.g., 1,000
or more) to create a bootstrap distribution of sample means. - This
distribution will be centered at the observed sample mean. - To simulate
the null hypothesis, shift the entire distribution so that its mean
equals 7.43. You can do this by subtracting the bootstrap distribution’s
mean from each value, and then adding 7.43 back. - Once shifted, compare
the observed mean to this distribution: calculate the proportion of
simulated means that are at least as extreme as the observed mean (i.e.,
the p-value).

📌 If this logic feels abstract, go back and review the Lecture on
Bootstrapping. The same shifting strategy was used there to simulate a
null centered at a hypothesized rent value.

Run a hypothesis test using simulation-based inference. Your goal is to
simulate a null distribution of sample means assuming a population mean
of 7.43 pounds, and then assess how unusual the observed sample mean is
within that distribution. To structure your analysis, work through the
following steps:

4a. Use bootstrapping to simulate a distribution of sample means from
ncbirths_white.

4b. Shift the distribution so that its center aligns with the null
hypothesis value (7.43 pounds).

4c. Create a histogram of your shifted null distribution. Overlay a
dashed vertical line to show your observed sample mean.

4d. Calculate the two-tailed p-value: what proportion of simulated means
are at least as extreme (in both directions) as your observed value?

4e. Interpret your results. Based on the p-value and your visualization,
what do you conclude about whether birth weight has changed since 1995?
