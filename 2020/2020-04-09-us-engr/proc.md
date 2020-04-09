ABET-Accredited Engineering Programs in the US
================
Zach
2020-04-09

A simple investigation of Wikipedia’s [List of engineering
schools](https://en.wikipedia.org/wiki/List_of_engineering_schools). My
focus is on institutions in the US.

``` r
library(tidyverse)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## Registered S3 method overwritten by 'rvest':
    ##   method            from
    ##   read_xml.response xml2

    ## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.1       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
url_engr <- "https://en.wikipedia.org/wiki/List_of_engineering_schools"
engr_selector <- "table.wikitable:nth-child(42)"
```

## Scrape and Wrangle Data

<!-- -------------------------------------------------- -->

``` r
page <- read_html(url_engr)
```

``` r
## Parse html
df_raw <-
  page %>%
  html_nodes(engr_selector) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  as_tibble()

## Wrangle
df_engr <-
  df_raw %>%
  mutate(
    `State (city)[10]` = if_else(
      str_detect(`State (city)[10]`, "District of Columbia"),
      "District of Columbia (District of Columbia)",
      `State (city)[10]`
    )
  ) %>%
  extract(
    col = `State (city)[10]`,
    into = c("state", "city"),
    regex = "([\\w\\s[:punct:]]+)\\((.+)\\)"
  ) %>%
  rename(
    has_grad = `Master's/doctoralprograms?`
  ) %>%
  rename_all(str_to_lower) %>%
  mutate(
    has_grad = if_else(has_grad == "Yes", TRUE, FALSE),
    state = str_trim(state)
  )

df_engr %>%
  filter(is.na(state))
```

    ## # A tibble: 0 x 5
    ## # … with 5 variables: state <chr>, city <chr>, university <chr>,
    ## #   school <chr>, has_grad <lgl>

## EDA

<!-- -------------------------------------------------- -->

``` r
df_engr %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   370

There are 370 universities in the United States with ABET-accredited
engineering programs. This is fewer than I expected.

``` r
df_engr %>%
  count(state) %>%
  arrange(desc(n)) %>%
  knitr::kable()
```

| state                |  n |
| :------------------- | -: |
| California           | 32 |
| Texas                | 26 |
| New York             | 25 |
| Pennsylvania         | 22 |
| Michigan             | 17 |
| Massachusetts        | 16 |
| Ohio                 | 15 |
| Florida              | 11 |
| Tennessee            | 11 |
| Colorado             | 10 |
| Indiana              | 10 |
| Connecticut          |  9 |
| Illinois             |  9 |
| New Jersey           |  8 |
| Virginia             |  8 |
| Washington           |  8 |
| Alabama              |  7 |
| Louisiana            |  7 |
| Maryland             |  7 |
| Missouri             |  7 |
| Minnesota            |  6 |
| North Carolina       |  6 |
| Wisconsin            |  6 |
| Arkansas             |  5 |
| Georgia              |  5 |
| Oklahoma             |  5 |
| Oregon               |  5 |
| Arizona              |  4 |
| District of Columbia |  4 |
| Idaho                |  4 |
| Iowa                 |  4 |
| Kentucky             |  4 |
| Utah                 |  4 |
| Kansas               |  3 |
| Maine                |  3 |
| Montana              |  3 |
| New Hampshire        |  3 |
| New Mexico           |  3 |
| Puerto Rico          |  3 |
| Rhode Island         |  3 |
| South Carolina       |  3 |
| West Virginia        |  3 |
| Alaska               |  2 |
| Mississippi          |  2 |
| Nevada               |  2 |
| North Dakota         |  2 |
| South Dakota         |  2 |
| Vermont              |  2 |
| Delaware             |  1 |
| Hawaii               |  1 |
| Nebraska             |  1 |
| Wyoming              |  1 |

Observations:

  - California, Texas, and New York have the most institutions with
    engineering programs
  - Delaware, Hawaii, Nebraska, and Wyoming have the fewest (one each)

<!-- end list -->

``` r
df_engr %>%
  count(city) %>%
  arrange(desc(n)) %>%
  head(10)
```

    ## # A tibble: 10 x 2
    ##    city                     n
    ##    <chr>                <int>
    ##  1 Boston                   4
    ##  2 District of Columbia     4
    ##  3 Los Angeles              4
    ##  4 New York                 4
    ##  5 Philadelphia             4
    ##  6 Baltimore                3
    ##  7 Colorado Springs         3
    ##  8 Detroit                  3
    ##  9 Nashville                3
    ## 10 Portland                 3

Observations:

  - The cities with the most engineering institutions are the ones I’d
    expect
  - However, I’d think Boston would have more institutions?
      - Note that MIT and Harvard are in Cambridge
