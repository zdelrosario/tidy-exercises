Daily Data Scrape
================
Zach
2020-03-16

**Purpose**: Our understanding of the covid-19 outbreak is rapidly
developing. This means our picture of the situation—the data
available—are going to evolve quickly. To build a retrospective
picture of how our understanding evolved, I’m going to track one source
of data as it develops, and keep a daily history.

This document scrapes the Santa Clara County government site for
covid-19 updates, and saves each day’s
    version.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.3.0     ✔ purrr   0.3.3
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.5
    ## ✔ tidyr   1.0.2     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
filename_stub <- "./data/covid19_santa_clara"
date_today <- today()
```

## \#\# Load and Wrangle

Load the Santa Clara update
page

``` r
url_santa_clara <- "https://www.sccgov.org/sites/phd/DiseaseInformation/novel-coronavirus/Pages/home.aspx"
css_santa_clara <- "#MSOZoneCell_WebPartctl00_m_g_6ee5292c_2758_48a4_a561_9decdb5a0be9 > table:nth-child(1)"
css_santa_header <- "h2.ms-rteElement-H2:nth-child(4)"
page <- read_html(url_santa_clara)
```

Scrape the update table

``` r
str_santa_clara <-
  page %>%
  html_nodes(css_santa_clara) %>%
  html_table() %>%
  .[[1]] %>%
  .[[1]]

## Doesn't extract cleanly; grab the string
str_santa_clara
```

    ## [1] "var sccgovJsonObjectctl00_m_g_6ee5292c_2758_48a4_a561_9decdb5a0be9 = [{\"Title\":\"03/19/2020\",\"Total_Confirmed_Cases\":\"196\",\"Hospitalized\":\"65\",\"Deaths\":\"8\",\"International_Travel_Associated\":\"21\",\"Close_Contacts_of_Known_Cases\":\"54\",\"Presumed_Community_Transmission\":\"93\",\"ID\":24,\"Modified\":\"\\/Date(1584737077000)\\/\",\"Created\":\"\\/Date(1584736774000)\\/\"}];$(document).ready(function () { sccgovGetTableHtml(sccgovJsonObjectctl00_m_g_6ee5292c_2758_48a4_a561_9decdb5a0be9, \"sccgov_generic_list_container_ctl00_m_g_6ee5292c_2758_48a4_a561_9decdb5a0be9\", \"/sites/phd/DiseaseInformation/novel-coronavirus/COVID19CASESV03\", \"COVID19CASESV03 - USE THIS ONE\"); });"

Wrangle the table

``` r
## Split the string and look for colon separators
df_today <-
  str_split(
    str_santa_clara,
    ","
  ) %>%
  as_tibble(.name_repair = "unique") %>%
  rename(s = `...1`) %>%
  mutate(s = str_remove_all(s, "\"")) %>%
  extract(s, into = c("var", "val"), regex = "(\\w+):(\\d+)", convert = TRUE) %>%
  filter(!is.na(val))
```

    ## New names:
    ## * `` -> ...1

Parse the header for the update time.

``` r
date_update <-
  page %>%
  html_nodes(css_santa_header) %>%
  html_text() %>%
  str_extract(., "as of .*") %>%
  str_extract(., "[:upper:].*$") %>%
  mdy(.)
```

## Write

<!-- -------------------------------------------------- -->

``` r
filename <- str_c(
  filename_stub,
  "_t",
  date_today,
  "_u",
  date_update,
  ".csv"
)

write_csv(df_today, filename)
```
