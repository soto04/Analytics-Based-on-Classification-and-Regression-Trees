``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   0.8.5
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)
library("patchwork")
head(flights)
```

    ## # A tibble: 6 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
ls("package:nycflights13")
```

    ## [1] "airlines" "airports" "flights"  "planes"   "weather"

``` r
glimpse(airlines)
```

    ## Rows: 16
    ## Columns: 2
    ## $ carrier <chr> "9E", "AA", "AS", "B6", "DL", "EV", "F9", "FL", "HA", "MQ",...
    ## $ name    <chr> "Endeavor Air Inc.", "American Airlines Inc.", "Alaska Airl...

``` r
glimpse(airports)
```

    ## Rows: 1,458
    ## Columns: 8
    ## $ faa   <chr> "04G", "06A", "06C", "06N", "09J", "0A9", "0G6", "0G7", "0P2"...
    ## $ name  <chr> "Lansdowne Airport", "Moton Field Municipal Airport", "Schaum...
    ## $ lat   <dbl> 41.13047, 32.46057, 41.98934, 41.43191, 31.07447, 36.37122, 4...
    ## $ lon   <dbl> -80.61958, -85.68003, -88.10124, -74.39156, -81.42778, -82.17...
    ## $ alt   <dbl> 1044, 264, 801, 523, 11, 1593, 730, 492, 1000, 108, 409, 875,...
    ## $ tz    <dbl> -5, -6, -6, -5, -5, -5, -5, -5, -5, -8, -5, -6, -5, -5, -5, -...
    ## $ dst   <chr> "A", "A", "A", "A", "A", "A", "A", "A", "U", "A", "A", "U", "...
    ## $ tzone <chr> "America/New_York", "America/Chicago", "America/Chicago", "Am...

``` r
glimpse(flights)
```

    ## Rows: 336,776
    ## Columns: 19
    ## $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013...
    ## $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 55...
    ## $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 60...
    ## $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2,...
    ## $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 8...
    ## $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 8...
    ## $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7,...
    ## $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6"...
    ## $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301...
    ## $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N...
    ## $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LG...
    ## $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IA...
    ## $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149...
    ## $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 73...
    ## $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6...
    ## $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59...
    ## $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-0...

``` r
glimpse(planes)
```

    ## Rows: 3,322
    ## Columns: 9
    ## $ tailnum      <chr> "N10156", "N102UW", "N103US", "N104UW", "N10575", "N10...
    ## $ year         <int> 2004, 1998, 1999, 1999, 2002, 1999, 1999, 1999, 1999, ...
    ## $ type         <chr> "Fixed wing multi engine", "Fixed wing multi engine", ...
    ## $ manufacturer <chr> "EMBRAER", "AIRBUS INDUSTRIE", "AIRBUS INDUSTRIE", "AI...
    ## $ model        <chr> "EMB-145XR", "A320-214", "A320-214", "A320-214", "EMB-...
    ## $ engines      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...
    ## $ seats        <int> 55, 182, 182, 182, 55, 182, 182, 182, 182, 182, 55, 55...
    ## $ speed        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ engine       <chr> "Turbo-fan", "Turbo-fan", "Turbo-fan", "Turbo-fan", "T...

``` r
glimpse(weather)
```

    ## Rows: 26,115
    ## Columns: 15
    ## $ origin     <chr> "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", ...
    ## $ year       <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 20...
    ## $ month      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ day        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ hour       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 1...
    ## $ temp       <dbl> 39.02, 39.02, 39.02, 39.92, 39.02, 37.94, 39.02, 39.92, ...
    ## $ dewp       <dbl> 26.06, 26.96, 28.04, 28.04, 28.04, 28.04, 28.04, 28.04, ...
    ## $ humid      <dbl> 59.37, 61.63, 64.43, 62.21, 64.43, 67.21, 64.43, 62.21, ...
    ## $ wind_dir   <dbl> 270, 250, 240, 250, 260, 240, 240, 250, 260, 260, 260, 3...
    ## $ wind_speed <dbl> 10.35702, 8.05546, 11.50780, 12.65858, 12.65858, 11.5078...
    ## $ wind_gust  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ precip     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ pressure   <dbl> 1012.0, 1012.3, 1012.5, 1012.2, 1011.9, 1012.4, 1012.2, ...
    ## $ visib      <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, ...
    ## $ time_hour  <dttm> 2013-01-01 01:00:00, 2013-01-01 02:00:00, 2013-01-01 03...

``` r
airlines_data <- airlines
airports_data <- airports
flights_data <- flights
planes_data <- planes
weather_data <- weather
```

### Histograma Año del avión

``` r
#min(planes$year, na.rm = TRUE) = 1956
#max(planes$year, na.rm = TRUE) = 2013
g <- ggplot(planes_data, aes(year)) #+ scale_fill_brewer(palette = "Spectral")

#tamaño del bin o size para cambiar el tamaño
g + geom_histogram(binwidth = 1, 
                   col="black", 
                   size=.5) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Plane Year Manufactured") +
  scale_x_continuous(breaks = 1960 + 10*(0:5))
```

    ## Warning: Removed 70 rows containing non-finite values (stat_bin).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
#Histograma básico
#Auto Bin poner los bins necesarios en ese tamaño, default es 30
```

``` r
#También podemos poner ggplot(mpg,aes(fill =class), pero es mejor tener los datos por separado de las características

g <- ggplot(planes_data, aes(year)) + scale_fill_brewer(palette = "Set1")

g + geom_histogram(aes(fill=engine), 
                   binwidth = 1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Year Manufactured and Engine") +
  scale_x_continuous(breaks = 1960 + 10*(0:5))
```

    ## Warning: Removed 70 rows containing non-finite values (stat_bin).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#damos el tamaño de bin, binwdith and size
```

``` r
g + geom_histogram(aes(fill=engine), 
                   binwidth = 4, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Fixed Bins", 
       subtitle="Year Manufactured and Engine") +
  scale_x_continuous(breaks = 1960 + 10*(0:5))
```

    ## Warning: Removed 70 rows containing non-finite values (stat_bin).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-7-1.png)
\#\#\# Histograma on a categorical variable

``` r
g <- ggplot(planes_data, aes(y = manufacturer))
g + geom_bar(aes(fill=engine), width = 0.9)  +
  labs(title="Histogram on Categorical Variable",
       subtitle="Manufactured and Engine")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-8-1.png)
\#\#\# Density plot

``` r
g <- ggplot(flights_data, aes(distance))
g + geom_density(aes(fill=factor(origin)), alpha=0.4) +
    labs(title="Density plot",
         subtitle="Distance by Origin (NYC AIRPORTS)",
         caption="Source: nycflights13",
         x="Distance",
         fill="Origin")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-9-1.png)
\#\#\# Box plots

``` r
flights2_data <- flights_data %>% 
  left_join(airlines)
```

    ## Joining, by = "carrier"

``` r
g <- ggplot(flights2_data, aes(name, distance))
g + geom_boxplot(varwidth=T, fill="dodgerblue", alpha = 0.5) +
  theme(axis.text.x = element_text(angle=90, vjust=0.7, size=7.5)) +
    labs(title="Box plot",
         subtitle="Distance grouped by airline",
         caption="Source: nycflights13",
         x="Airline",
         y="Distance")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
#Box plot para todas las categorías
```

``` r
# divide los boxplot de cada una de las cetegorías
g <- ggplot(flights2_data, aes(name, distance))
g + geom_boxplot(aes(fill=factor(origin))) +  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6,size= 7)) +
  labs(title="Box plot",
       subtitle="Distance grouped by airline",
       caption="Source: nycflights13",
       factor = "Origin",
         x="Airline",
         y="Distance")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Violin part

``` r
g <- ggplot(flights2_data, aes(origin, distance))
g + geom_violin() +
  labs(title="Violin plot",
       subtitle="Distance by Origin (NYC AIRPORTS)",
       caption="Source: nycflight13",
       x="Origin",
       y="Distance")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
g + geom_violin(trim =TRUE) + coord_flip()
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-13-1.png)
\#\#\# Add summary statistics on a violin plot

``` r
g <- ggplot(flights2_data, aes(origin, distance))
p <- g + geom_violin() +
  labs(title="Violin plot",
       subtitle="Distance by Origin (NYC AIRPORTS)",
       caption="Source: nycflight13",
       x="Origin",
       y="Distance") +
  coord_flip()
p
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
p + stat_summary(fun=median, geom="point", size=2, color="red")
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
p + geom_boxplot(width=0.05)
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
#Divide por cada uno de las calses por color
#color = class, pero ya estan en el ejex

g <- ggplot(flights2_data, aes(origin, distance, color = origin))
pc <- g + geom_violin() +
  labs(title="Violin plot",
       subtitle="Distance by Origin (NYC AIRPORTS)",
       caption="Source: nycflight13",
       x="Origin",
       y="Distance") +
  coord_flip() +
   scale_color_brewer(palette="Set1")
pc
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-17-1.png)
\#\#\# Multiple Graphs (Air time, amount of time spent in the air in
minutes)

``` r
p_data <- ggplot(flights2_data,aes(x = air_time))

p1 <- p_data + geom_histogram(binwidth = 7, color = "steelblue", fill = "white") + 
  labs(title="Bin width 7")
p2 <-  p_data + geom_histogram(binwidth = 10, color = "dark blue", 
                               fill = "blue", alpha = 0.4) + 
  labs(title="Bin width 10")

p3 <- p_data + geom_histogram(binwidth = 12, color = "firebrick", 
                              fill = "dark red", alpha = 0.2) + 
  labs(title="Bin width 12")

 p1 + (p2 /p3)
```

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
 p1 / (p2 +p3)
```

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-19-1.png)

### Histogram and Density

``` r
ggplot(flights2_data,aes(x = air_time)) + 
  geom_histogram(binwidth = 7,aes(y=..density..), color = "steelblue2", fill = "steelblue4") +
  geom_density(alpha = .2, fill = "lightblue1") + labs(title="Air Time")
```

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_density).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-20-1.png)
\#\#\# Add Value Markers

``` r
ggplot(flights2_data,aes(x = air_time)) + 
  geom_histogram(binwidth = 7,aes(y=..density..), color = "steelblue2", fill = "steelblue4", alpha = 0.8)+
 geom_density(alpha = .2, fill = "lightblue1") + labs(title="Air Time") + 
  geom_vline(xintercept = mean(flights2_data$air_time, na.rm=TRUE), color = "red", linetype = "dashed")
```

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

    ## Warning: Removed 9430 rows containing non-finite values (stat_density).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-21-1.png)

### Air Time (Top 5 airlines)

``` r
compare_mean <- flights2_data %>%
        group_by(name) %>%
        summarise(Mean = mean(air_time, na.rm=TRUE)) %>% 
        top_n(5)
```

    ## Selecting by Mean

``` r
# Separar los Top 5 según su media
flights3_data <- flights2_data %>% 
  filter(name %in%  compare_mean$name)
 

g <- ggplot(flights3_data, aes(x = air_time))
g + geom_density(aes(fill=factor(name)), alpha=0.35) + 
    labs(title="Density plot", 
         subtitle="Amount of time by Airlines(Top 5)",
         caption="Source: nycflights13",
         x="Air Time",
         fill="Name of the Airline") + 
  geom_vline(data = compare_mean, aes(xintercept = Mean, color = factor(name)),
                   linetype = "dashed", size = 0.8)
```

    ## Warning: Removed 938 rows containing non-finite values (stat_density).

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-22-1.png)

### Error bars Top 10 airlines (Air Time)

``` r
compare_mean <- flights2_data %>%
        group_by(name) %>%
        summarise(Mean = mean(air_time, na.rm=TRUE)) %>% 
        top_n(10)
```

    ## Selecting by Mean

``` r
# Separar los Top 5 según su media
flights3_data <- flights2_data %>% 
  filter(name %in%  compare_mean$name)




flights_means_se <- flights3_data %>%
  group_by(name) %>% 
  summarize(mean_air = mean(air_time, na.rm = TRUE), 
            sd_air=sd(air_time, na.rm = TRUE), 
            N_air=n(), 
            se = sd_air/sqrt(N_air), 
            upper_limit=mean_air+se, 
            lower_limit=mean_air-se 
            )
flights_means_se 
```

    ## # A tibble: 10 x 7
    ##    name                      mean_air sd_air N_air    se upper_limit lower_limit
    ##    <chr>                        <dbl>  <dbl> <int> <dbl>       <dbl>       <dbl>
    ##  1 AirTran Airways Corporat~     101.   23.9  3260 0.419        102.        101.
    ##  2 Alaska Airlines Inc.          326.   16.2   714 0.605        326.        325.
    ##  3 American Airlines Inc.        189.   81.7 32729 0.451        189.        188.
    ##  4 Delta Air Lines Inc.          174.   84.8 48110 0.387        174.        173.
    ##  5 Frontier Airlines Inc.        230.   15.2   685 0.579        230.        229.
    ##  6 Hawaiian Airlines Inc.        623.   20.7   342 1.12         624.        622.
    ##  7 JetBlue Airways               151.   89.6 54635 0.384        152.        151.
    ##  8 Southwest Airlines Co.        148.   55.5 12275 0.501        148.        147.
    ##  9 United Air Lines Inc.         212.  101.  58665 0.417        212.        211.
    ## 10 Virgin America                337.   20.9  5162 0.291        337.        337.

``` r
ggplot(flights_means_se, aes(x = name, y=mean_air, fill=name)) +
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width = 0.2) +
   theme(axis.text.x = element_text(angle=70, vjust=0.6,size= 7))
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
ggplot(flights_means_se, aes(y = name, x = mean_air, fill=name)) +
  geom_errorbar(aes(xmin=lower_limit, xmax=upper_limit), width = 0.2)# +
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-24-2.png)

``` r
 #  theme(axis.text.x = element_text(angle=70, vjust=0.6,size= 7))
```

``` r
ggplot(flights_means_se, aes(y = name, x = mean_air, fill=name)) +
  geom_errorbar(aes(xmin=lower_limit, xmax=upper_limit), width = 0.2) +
   geom_bar(stat="identity", alpha= 0.5)
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-25-1.png)

### Reordering

``` r
ggplot(flights_means_se, aes(y = reorder(name, mean_air), x = mean_air, fill=name)) +
  geom_errorbar(aes(xmin=lower_limit, xmax=upper_limit), width = 0.2) +
   geom_bar(stat="identity", alpha= 0.5)
```

![](Lecture-3_2flights_files/figure-markdown_github/unnamed-chunk-26-1.png)
