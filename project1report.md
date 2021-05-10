Project 1 Report
================
Israel Vasquez iv3356

## Data selected

The data selected were two separate datasets exploring determinants of
economic growth data found within the R-package “AER”. The variables
that will be used in this report are “growth” the average percentage
change in gdp from 1960-1995, “rgdp60” the gdp per capita in the year
1960 (in USD), “education” the average number of years of schooling
adult residents received, “tradeshare” the sum of exports and imports
divided by GDP, “revolutions” the average annual number of
revolutions/coups/insurrections from 1960-1995, and “assassinations” the
average annual number of political assassinations from 1960-1995 (in per
million population). I chose this data since I thought it would be
interesting to explore specifically how the effects of radical violence
such as assassinations and revolutions on GDP growth compared to the
effects of typical economic factors. The two datasets were found to be
already tidy and were joined using the following code.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.5     v dplyr   1.0.3
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
GrowthDJ <- read_csv("C:/Users/israe/Documents/RStuff/GrowthDJ.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   X1 = col_double(),
    ##   oil = col_character(),
    ##   inter = col_character(),
    ##   oecd = col_character(),
    ##   gdp60 = col_double(),
    ##   gdp85 = col_double(),
    ##   gdpgrowth = col_double(),
    ##   popgrowth = col_double(),
    ##   invest = col_double(),
    ##   school = col_double(),
    ##   literacy60 = col_double()
    ## )

``` r
growth <- read_csv("C:/Users/israe/Documents/RStuff/GrowthSW.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   X1 = col_double(),
    ##   country = col_character(),
    ##   growth = col_double(),
    ##   rgdp60 = col_double(),
    ##   tradeshare = col_double(),
    ##   education = col_double(),
    ##   revolutions = col_double(),
    ##   assassinations = col_double()
    ## )

``` r
data1 <- growth %>% left_join(GrowthDJ, by = 'X1')
```

This data was joined this way to keep the names of the countries from
the first dataset while adding the other variables from the second
dataset. One issue that was that the two data sets had their own growth
variables with different meanings. Where the first dataset measured
growth in real GDP per capita while the other measured gross GDP growth.
For this report I will be using the first dataset’s variable for growth.

## Further data manipulation and Summary Statistics

``` r
#add categorical variable
data1 <- data1 %>%
  mutate(political_violence= case_when(assassinations > 0 ~ 'yes',
                                       revolutions > 0 ~ 'yes',
                                       TRUE ~ 'no'))
#create separate dataframe of just numeric variables of interest
data1_num <- data1 %>%
  select(-X1) %>%
  column_to_rownames("country") %>%
  select(`growth`,rgdp60,tradeshare,education,revolutions,assassinations)

#economic efficiency with education
data1 %>%
  group_by(country) %>%
  mutate('gdp/education'= rgdp60/education) %>%
  summarise(`gdp/education`) %>%
  arrange(desc(`gdp/education`))
```

    ## # A tibble: 65 x 2
    ##    country             `gdp/education`
    ##    <chr>                         <dbl>
    ##  1 Niger                         2660.
    ##  2 Venezuela                     2505.
    ##  3 Sierra Leone                  1657.
    ##  4 Austria                       1409.
    ##  5 Switzerland                   1370.
    ##  6 Togo                          1359.
    ##  7 Haiti                         1320.
    ##  8 Trinidad and Tobago           1306.
    ##  9 Ghana                         1296.
    ## 10 France                        1252.
    ## # ... with 55 more rows

``` r
#rate of assassinations per revolution if country had both an assassination and a revolution
data1 %>%
  group_by(country) %>%
  filter(assassinations>0, revolutions>0) %>%
  mutate('assassinations/revolution'=assassinations/revolutions) %>%
  summarise(`assassinations/revolution`) %>%
  arrange(desc(`assassinations/revolution`))
```

    ## # A tibble: 38 x 2
    ##    country      `assassinations/revolution`
    ##    <chr>                              <dbl>
    ##  1 Italy                              36.0 
    ##  2 Spain                              21.5 
    ##  3 Colombia                            7.67
    ##  4 India                               6.50
    ##  5 Guatemala                           4.56
    ##  6 Kenya                               4.33
    ##  7 South Africa                        3.67
    ##  8 Germany                             3.50
    ##  9 Israel                              3.00
    ## 10 Chile                               2.80
    ## # ... with 28 more rows

``` r
#Finding country with most economic growth
data1 %>%
  group_by(country) %>%
  summarise(`growth`) %>%
  arrange(desc(`growth`))
```

    ## # A tibble: 65 x 2
    ##    country            growth
    ##    <chr>               <dbl>
    ##  1 Korea, Republic of   7.16
    ##  2 Malta                6.65
    ##  3 Taiwan, China        6.62
    ##  4 Cyprus               5.38
    ##  5 Thailand             4.88
    ##  6 Japan                4.30
    ##  7 Malaysia             4.11
    ##  8 Portugal             3.65
    ##  9 Ireland              3.25
    ## 10 Greece               3.22
    ## # ... with 55 more rows

``` r
#Mean,standard deviation, minimums, and maximums of numeric variables
data1_num %>%
  summarise_if(is.numeric,mean)
```

    ##     growth   rgdp60 tradeshare education revolutions assassinations
    ## 1 1.942715 3103.785   0.564703  3.985077   0.1674501      0.2775641

``` r
data1_num %>%
  summarise_if(is.numeric,sd)
```

    ##    growth   rgdp60 tradeshare education revolutions assassinations
    ## 1 1.89712 2512.657  0.2892703     2.542   0.2246798      0.4915284

``` r
data1_num %>%
  summarise_if(is.numeric,min)
```

    ##      growth   rgdp60 tradeshare education revolutions assassinations
    ## 1 -2.811944 366.9999   0.140502       0.2           0              0

``` r
data1_num %>%
  summarise_if(is.numeric,max)
```

    ##     growth   rgdp60 tradeshare education revolutions assassinations
    ## 1 7.156855 9895.004   1.992616     10.07   0.9703704       2.466667

When further manipulating the data, I first created a new categorical
variable named “political\_violence” where when there was an
assassination or a revolution in a country, the variable would read a
value of “yes”, and if there were neither, it would read a value of
“no”. Next I created a separate data frame containing only the numerical
variables of interest to efficiently calculate summary statistics and to
create the correlation matrix later on.  
Statistics I calculated included economic efficiency with education. I
did this by dividing the rgdp60 and education variables through
mutation. The idea behind this is that I wanted to see which country had
the highest rate of gdp per years of education. So a high value for this
gdp/education variable would indicate that a country was able to
efficiently generate gdp per year of education for a population. The
highest value for this statistic went to the country of Niger.  
Next I calculated rate of assassinations per revolutions within a
country through mutation, given that the country had both an
assassination and a revolution during the time frame of 1960-1995. The
country with highest rate of assassinations per revolution was Italy.  
I then calculated mean, standard deviation, the minimums, and the
maximums for each numeric variable of interest within the dataset. Some
interesting findings for this are that the country with the highest rate
of economic growth from 1960-1995 was South Korea, while the country
with the highest gdp per capita in 1960 was the United States.

## Visualizations

After I created the correlation matrix for the numeric variables of
interest, I found that the biggest contributors (either positively or
negatively) to economic growth were tradeshare, and political violence.
Another discovery was that GDP per capita was heavily correlated to
education. I created two more scatterplots to better see the
relationship between these correlates.

``` r
#cor matrix
cor(data1_num) %>%
  as.data.frame %>%
  rownames_to_column %>%
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  ggplot(aes(rowname, factor(other_var, levels = rev(levels(factor(other_var)))), fill=correlation)) +
  geom_tile() +
  scale_fill_gradient2(low="red",mid="white",high="blue") +
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  labs(title = "Correlation matrix for GDP growth factors", x = "Variable 1", y = "Variable 2")
```

![](project1report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#economic growth by tradeshare and the presence of political violence
ggplot(data = data1, aes(x = tradeshare, y = `growth`)) +
  geom_point(aes(color = political_violence)) +
  geom_smooth(method = 'lm', se = FALSE, aes(color = political_violence)) +
  labs(title = 'Economic Growth by Tradeshare and presence of Political Violence')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project1report_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#GDP per capita by Average years of Education and presence of Political Violence
ggplot(data = data1, aes(x = education, y = rgdp60)) +
  geom_point(aes(color = political_violence)) +
  geom_smooth(method = 'lm', se = FALSE, aes(color = political_violence)) +
  labs(title = 'GDP per capita by Average years of Education and presence of Political Violence')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project1report_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

Based on the first scatterplot, tradeshare has a fairly linear
relationship to economic growth and we can see that the presence of
political violence does affect the rate of economic growth. From the
second plot we can see that the relationship between education and GDP
per capita is very linear and the presence of political violence does
not seem to have much of an effect on that relationship.

\#Principal Component Analysis via PAM

``` r
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.0.4

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(cluster)
sil_width <- vector()
for(i in 2:10){  
  pam_fit <- pam(data1_num, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width}

fviz_nbclust(data1_num, FUNcluster = pam, method = "s")
```

![](project1report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
data1_pam <- data1_num %>%
  pam(k=2)

data1_pam
```

    ## Medoids:
    ##             ID   growth   rgdp60 tradeshare education revolutions
    ## Sri Lanka   53 2.704598 1259.000  0.7429716      3.43         0.2
    ## Netherlands 60 2.200577 6076.997  0.8342042      5.27         0.0
    ##             assassinations
    ## Sri Lanka              0.2
    ## Netherlands            0.0
    ## Clustering vector:
    ##               India           Argentina               Japan              Brazil 
    ##                   1                   2                   1                   1 
    ##       United States          Bangladesh               Spain            Colombia 
    ##                   2                   1                   1                   1 
    ##                Peru               Haiti           Australia               Italy 
    ##                   1                   1                   2                   2 
    ##              Greece              France               Zaire             Uruguay 
    ##                   1                   2                   1                   2 
    ##              Mexico            Pakistan               Niger             Bolivia 
    ##                   1                   1                   1                   1 
    ##             Germany              Canada      United Kingdom         New Zealand 
    ##                   2                   2                   2                   2 
    ##         Philippines             Finland           Venezuela  Korea, Republic of 
    ##                   1                   2                   2                   1 
    ##           Guatemala            Honduras         El Salvador               Chile 
    ##                   1                   1                   1                   1 
    ##            Thailand              Sweden             Senegal Trinidad and Tobago 
    ##                   1                   2                   1                   2 
    ##             Ecuador             Denmark         Switzerland             Austria 
    ##                   1                   2                   2                   2 
    ##            Zimbabwe            Paraguay          Costa Rica            Portugal 
    ##                   1                   1                   1                   1 
    ##                Togo             Iceland              Israel        South Africa 
    ##                   1                   2                   1                   1 
    ##              Norway        Sierra Leone  Dominican Republic               Ghana 
    ##                   2                   1                   1                   1 
    ##           Sri Lanka       Taiwan, China              Panama    Papua New Guinea 
    ##                   1                   1                   1                   1 
    ##               Kenya             Ireland             Jamaica         Netherlands 
    ##                   1                   1                   1                   2 
    ##              Cyprus            Malaysia             Belgium           Mauritius 
    ##                   1                   1                   2                   1 
    ##               Malta 
    ##                   1 
    ## Objective function:
    ##    build     swap 
    ## 939.9133 814.3037 
    ## 
    ## Available components:
    ##  [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
    ##  [6] "clusinfo"   "silinfo"    "diss"       "call"       "data"

``` r
data1_pca <- data1_num %>%
  scale %>%
  prcomp

fviz_pca_var(data1_pca, col.var = "black")
```

![](project1report_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
fviz_cluster(list(data=data1_num, cluster = data1_pam$clustering),  ellipse.type="convex", geom="point", stand=TRUE, palette="Dark2") + labs(title = "PAM") + theme(legend.position="bottom")
```

![](project1report_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->
