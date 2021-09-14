DSU Sulfur Dixoide EDA
================

First to clean the data, we needed to properly format the Date column.

``` r
colnames(to)[1] <- "Month"
wso <- left_join(to, so)
```

    ## Joining, by = "Month"

``` r
ten <- paste(20, wso$Month)
ten <- as.Date(ten, format= "%d %b-%y")

rnd <- round_date(ten, unit = "month") -1 

wso$Month <- rnd

head(wso)
```

    ## # A tibble: 6 x 12
    ##   Month          GEO      NG     NUC     OBG      OT    SUN    WAT    WND  Total
    ##   <date>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 2015-01-31  1.04e6  9.42e6 1566083 484337. 193870. 6.34e5 7.77e5 1.74e5 1.43e7
    ## 2 2015-02-28  9.50e5  7.09e6 1527938 401115. 177275. 8.47e5 8.33e5 6.00e5 1.24e7
    ## 3 2015-03-31  9.65e5  8.17e6 1695814 456489. 175510. 1.17e6 8.20e5 7.99e5 1.43e7
    ## 4 2015-04-30  9.64e5  7.86e6 1647237 406532. 156495. 1.33e6 9.25e5 1.23e6 1.45e7
    ## 5 2015-05-31  1.06e6  7.54e6 1697962 474720. 196879. 1.37e6 1.23e6 1.65e6 1.52e7
    ## 6 2015-06-30  1.04e6  1.06e7 1641225 496822. 181533. 1.45e6 1.49e6 1.59e6 1.85e7
    ## # … with 2 more variables: SO2_Con <dbl>, SO2_AQI <dbl>

ggplot requires the data to be structured in a gathered format to build
visualizations. The following are visuals that explore the relationships
among different pollutants.

``` r
too <- wso %>% gather(key="Source", value = "MWH", GEO:SO2_AQI)

head(too)
```

    ## # A tibble: 6 x 3
    ##   Month      Source      MWH
    ##   <date>     <chr>     <dbl>
    ## 1 2015-01-31 GEO    1043735.
    ## 2 2015-02-28 GEO     950206.
    ## 3 2015-03-31 GEO     965262.
    ## 4 2015-04-30 GEO     964087.
    ## 5 2015-05-31 GEO    1056928.
    ## 6 2015-06-30 GEO    1036208.

``` r
#bar plot

ggplot(data=too[-c(577:792),], aes(x=Source, y=MWH, fill = Source)) +
  geom_bar(stat="identity") +
  ggtitle("Bar Chart of Net Energy (MWh) Produced By Energy Source") + 
  scale_fill_manual(values = c("coral3", "coral2", "darkorange", "darkolivegreen2", "darkolivegreen3", "cyan4", "darkorchid3", "darkorchid4"), name = "Source", labels = c("Geothermal", "Natural Gas", "Nuclear","Other: Biomass Gas", "Other", "Solar Energy", "Hydro Power", "Wind Energy")) +
  theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Natural gas makes up the majority of energy produced in California, the
following pie chart looks at how much this is in relation to other power
plant energy production.

``` r
#piechart
ggplot(too[-c(577:792),], aes(x="", y=MWH, fill =Source ))+
geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  ggtitle("Pie Chart of Net Energy (MWh) Produced By Energy Source") +
  labs(x="", y="") + scale_fill_manual(values = c("coral3", "coral2", "darkorange", "darkolivegreen2", "darkolivegreen3", "cyan4", "darkorchid3", "darkorchid4"), name = "Source", labels = c("Geothermal", "Natural Gas", "Nuclear","Other: Biomass Gas", "Other", "Solar Energy", "Hydro Power", "Wind Energy")) +
  theme_void()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Natural gas makes up about 50% of all energy produced in California, and
is followed by solar and hydro energy that make up a combined 25% of
energy produced in California power plants.

``` r
#line graph

ggplot(data=too[-c(577:792),], aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1)  + 
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  ggtitle("Monthly Average Energy Production") +
  labs(x="year", y="Energy Produced (MWh)") + scale_color_manual(values = c("lightblue2", "lightpink2", "lightsteelblue3", "lightsteelblue4", "mediumorchid1", "mediumpurple1", "lightskyblue", "lightseagreen", "#de7065ff"), labels = c("Geothermal", "Natural Gas", "Nuclear", "Other: Biomass Gas", "Other", "Solar Energy","Hydro Power", "Wind Energy"), name = "Source") + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Natural gas makes up the majority of energy produce, but since 2015 it
seems to be decreasing in net volume produced annually. Solar has had
the largest increase in energy production in this period.

``` r
sew <- too[649:792,]

#So2
ggplot(data=sew, aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  ggtitle("Monthly Average of SO2 Concentration Measures") +
  labs(x="year", y="SO2 Concentration (ppm)") + scale_color_manual(values = c("cadetblue3", "darkgoldenrod1"), name = "Source", labels = c("SO2 AQI", "SO2 Concentration")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The following graphs compare standardized measurements between Sulfur
Dioxide and various different other variables.

``` r
# scaling 
x <- scale(wso[2:12])
x <- as.data.frame(x)
x$Month <- rnd
y <- x %>% gather(key="Source", value = "MWH", GEO:SO2_AQI)


#NG
ggplot(data=y[c(73:144,649:720),], aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  labs(title = "Standardized Changes in Production and Concentration", subtitle = "natural gas produced (Net MWh) and SO2 concentration (ppm)", y = "Standardized Values (Z-Scores)") + scale_color_manual(values = c("lightsalmon","lightseagreen"), name = "Source", labels = c("Natural Gas", "SO2 Concentration")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Water
ggplot(data=y[c(433:504,649:720),], aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  labs(title = "Standardized Changes in Production and Concentration", subtitle = "hyrdo energy (Net MWh) and SO2 concentration (ppm)", y = "Standardized Values (Z-Scores)") + scale_color_manual(values = c("lightseagreen", "violet"), name = "Source", labels = c("SO2 Concentration", "Hydro Power")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
#solar
ggplot(data=y[c(361:429,649:720),], aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1)+
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  labs(title = "Standardized Changes in Production and Concentration", subtitle = "solar energy (Net MWh) and SO2 concentration (ppm)", y = "Standardized Values (Z-Scores)") + scale_color_manual(values = c("lightseagreen", "darkgoldenrod1"), name = "Source", labels = c("SO2 Concentration", "Solar Power")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
#wind
ggplot(data=y[c(505:573,649:720),], aes(x=Month, y=MWH)) +
  geom_line(aes(color = Source), size = 1)+
  scale_x_date(date_breaks = "1 year",date_labels = "%b-%Y") +
  labs(title = "Standardized Changes in Production and Concentration", subtitle = "wind energy (Net MWh) and SO2 concentration (ppm)", y = "Standardized Values (Z-Scores)") + scale_color_manual(values = c("lightseagreen", "violet"), name = "Source", labels = c("SO2 Concentration", "Wind Energy")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

These graphs show the monthly average values for each year to show the
effects of seasonality on different variables.

``` r
too[c(73:144),] %>% 
  mutate(
    year = factor(year(Month)),     # use year to define separate curves
    date = update(Month, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(date, MWH, color = year)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + geom_line() +
  labs(x="Month", y ="Net MWh", title ="Natural Gas Average Monthly Production (MWh)" )+  scale_color_brewer(palette="Set2",name = "Year", labels = c("2015", "2016", "2017", "2018", "2019", "2020")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
too[c(433:504),] %>% 
  mutate(
    year = factor(year(Month)),     # use year to define separate curves
    date = update(Month, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(date, MWH, color = year)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + geom_line() +
  labs(x="Month", y ="Net MWh", title ="Hydro Energy Average Monthly Production (MWh)" )+  scale_color_brewer(palette="Set2",name = "Year", labels = c("2015", "2016", "2017", "2018", "2019", "2020")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
too[c(361:432),] %>% 
  mutate(
    year = factor(year(Month)),     # use year to define separate curves
    date = update(Month, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(date, MWH, color = year)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + geom_line() +
  labs(x="Month", y ="Net MWh", title ="Solar Energy Average Monthly Production (MWh)" )+  scale_color_brewer(palette="Set2",name = "Year", labels = c("2015", "2016", "2017", "2018", "2019", "2020")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
too[c(649:720),] %>% 
  mutate(
    year = factor(year(Month)),     # use year to define separate curves
    date = update(Month, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(date, MWH, color = year)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + geom_line() +
  labs(x="Month", y ="Net MWh", title ="So2 Energy Average Monthly Concentration (ppm)" )+  scale_color_brewer(palette="Set2",name = "Year", labels = c("2015", "2016", "2017", "2018", "2019", "2020")) + theme_minimal()
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

Finally, this is a correlation plot that visualizes the strength of the
relationships between each of the variables.

``` r
x3 <- x[,-c(11,12)]
colnames(x3) <- c("Geothermal", "Natural Gas", "Nuclear", "Biomass Gas", "Other", "Solar", "Hydro", "Wind", "Total", "Sulfur Dioxide")
M2 <- cor(x3)
corrplot(M2, type="upper", order="hclust",
         col = turbo(8)[1:8])
```

![](Compiled_finalEDA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
