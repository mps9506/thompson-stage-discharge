---
title: "Exploring Thompsons Creek Stage Discharge Data"
date: "2021-01-21"
github-repo: https://github.com/mps9506/thompson-stage-discharge
bibliography: bibliography.bib
biblio-style: "apalike"
link-citations: true
---



## About

This document is an exploratory analysis of the pressure data and IQ data at ...


```r
library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)
library(here)
library(units)
library(ggforce)
library(hrbrthemes)

update_geom_font_defaults(font_rc)

theme_ms <- function(...) {
  theme_ipsum_pub(plot_margin = margin(10,10,10,10),
              axis_title_just = "c") +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white", 
            colour = NA), 
          panel.border = element_rect(fill = NA, 
            colour = "grey20"),
          ...)
}
```



## Data Import

### Pressure Transducers

Use `readr::read_csv()` to import data. The HOBO pressure data is pre-computed but has extra lines we need to clean up.

- each location has two files, March through August and August through December.

- ideally, I can make one dataframe that compiles all the data

- we can read all the files and row bind but we need to add a column indicating the site number

- for this deployment it looks like the daylight savings time adjustment is not applied to data. So everything between March 6 and November 1 is off 1 hour from DST. Assuming all data will be reported in observed time, it is probably easiest to convert time back to GMT, then apply R's built in time zones to convert the times with appropriate -5 or -6 hour offsets.


```r
## make a list of files to import
file_paths <- paste0(here("Data/Hobo"),
                     "/",
                     list.files(path = here("Data/Hobo"),
                                pattern = ".csv"))

## create a blank tibble to fill
hobo_df <- tibble()


## loop through file paths to read each file
for (i in file_paths) {
  x <- read_csv(
    i,
    skip = 2,
    col_names = c(
      "Row",
      "Date",
      "Time",
      "Abs_Pres",
      "Temp",
      "Bar_Pressure",
      "Water_Level",
      "Coupler_Detached",
      "Coupler_Attached",
      "Stopped",
      "EOF"
    ),
    col_types = "nccnnnncccc"
  )
  x$file <- i
  hobo_df <- bind_rows(hobo_df, x)
  rm(x)
}

## clean up the dataframe
hobo_df <- hobo_df %>%
  mutate(
    ## regex extracts site number from file path
    Site = str_extract(file, "\\d{1,6}"),
    
    ## convert date and time columns to date/time format
    dt = paste(Date, Time),
    Date_Time = as.POSIXct(paste(Date, Time),
                           tz = "Etc/GMT-6",
                           format = "%m/%d/%y %I:%M:%S %p")) %>%
  mutate(Site = as.factor(Site)) %>%
  ## select the columns we need to keep
  dplyr::select(Abs_Pres, Temp, Water_Level, Site, Date_Time) %>%
  ## filter rows without water_level
  dplyr::filter(!is.na(Water_Level))
  
## attach units to our columns
units(hobo_df$Water_Level) <- as_units("ft")
units(hobo_df$Temp) <- as_units("°F")
units(hobo_df$Abs_Pres) <- as_units("psi")

  
hobo_df
```

```
## # A tibble: 71,447 x 5
##    Abs_Pres   Temp Water_Level Site  Date_Time          
##       [psi]   [°F]        [ft] <fct> <dttm>             
##  1  14.4270 76.006       0.392 16396 2020-03-02 15:17:19
##  2  14.4237 75.659       0.388 16396 2020-03-02 15:32:19
##  3  14.4272 75.312       0.399 16396 2020-03-02 15:47:19
##  4  14.4192 74.964       0.391 16396 2020-03-02 16:02:19
##  5  14.4165 74.791       0.379 16396 2020-03-02 16:17:19
##  6  14.4153 74.446       0.361 16396 2020-03-02 16:32:19
##  7  14.4172 74.271       0.364 16396 2020-03-02 16:47:19
##  8  14.4166 74.098       0.363 16396 2020-03-02 17:02:19
##  9  14.4207 73.926       0.372 16396 2020-03-02 17:17:19
## 10  14.4270 73.753       0.376 16396 2020-03-02 17:32:19
## # … with 71,437 more rows
```

### IQ Plus

```r
#read_csv(here::here("Data/IQPlus/16397-2020_12_31.csv"))

## make a list of files to import
file_paths <- paste0(here("Data/IQPlus"),
                     "/",
                     list.files(path = here("Data/IQPlus"),
                                pattern = ".csv"))

## create a blank tibble to fill
iqplus_df <- tibble()

## loop through file paths to read each file
for (i in file_paths) {
  x <- read_csv(
    i,
    col_types = "nc______n__n________________________________________"
  )
  x$file <- i
  iqplus_df <- bind_rows(iqplus_df, x)
  rm(x)
}


iqplus_df <- iqplus_df %>%
    mutate(
    ## regex extracts site number from file path
    Site = str_extract(file, "\\d{1,6}")) %>%
  ## use `dplyr::` to specify which rename function to use, just in case
  dplyr::rename(Sample_Number =`Sample number`,
                Date_Time = `Sample time`,
                Depth = `Depth (ft)`,
                Flow = `Flow (ft³/s)`) %>%
  dplyr::select(-c(Sample_Number, file)) %>%
  mutate(Date_Time = as.POSIXct(Date_Time,
                                tz = "Etc/GMT-6",
                                format = "%Y-%m-%d %H:%M:%S"))


## attach units to our columns
units(iqplus_df$Depth) <- as_units("ft")
units(iqplus_df$Flow) <- as_units("ft^3/s")

iqplus_df
```

```
## # A tibble: 26,174 x 4
##    Date_Time              Depth     Flow Site 
##    <dttm>                  [ft] [ft^3/s] <chr>
##  1 2020-05-12 16:58:00 1.758559 17.25626 16396
##  2 2020-05-12 17:13:00 1.867973 20.86921 16396
##  3 2020-05-12 17:28:00 2.006131 27.27231 16396
##  4 2020-05-12 17:43:00 2.119382 31.04828 16396
##  5 2020-05-12 17:58:00 2.172015 33.71041 16396
##  6 2020-05-12 18:13:00 2.228058 36.78339 16396
##  7 2020-05-12 18:28:00 2.350440 44.18084 16396
##  8 2020-05-12 18:43:00 2.508431 51.30917 16396
##  9 2020-05-12 18:58:00 2.674151 60.84247 16396
## 10 2020-05-12 19:13:00 2.809936 69.92496 16396
## # … with 26,164 more rows
```



## Data Explore

### Pressure Transducers

Plot the depth date over time for all three sites


```r
## note, that if the scale name has a space, there is a bug in ggforce
## need to set units_options(parse = FALSE) per:
## https://github.com/thomasp85/ggforce/issues/197

ggplot(hobo_df) +
  geom_line(aes(Date_Time, Water_Level, color = Site)) +
  scale_y_unit(name = "Level", unit = "ft") +
  scale_x_datetime(name = "Date/Time") +
  labs(title = "15-Minute Measured Height") +
  theme_ms()
```

<img src="document_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## IQPlus

Plot the depth data drom the IQPlus from all three sites.


```r
ggplot(iqplus_df) +
  geom_point(aes(Date_Time, Depth, color = Site)) +
  facet_wrap(~Site, scales = "free") +
  labs(title = "Stage Discharge") +
  theme_ms()
```

```
## Warning: Removed 1483 rows containing missing values (geom_point).
```

<img src="document_files/figure-html/unnamed-chunk-2-1.png" width="672" />


Plot the depth-discharge from all three sites as measured by the IQPlus.


```r
ggplot(iqplus_df) +
  geom_point(aes(Depth, Flow, color = Site)) +
  scale_y_unit(name = "Flow", unit = "ft^3/s") +
  facet_wrap(~Site, scales = "free") +
  labs(title = "Stage Discharge") +
  theme_ms()
```

```
## Warning: Removed 1483 rows containing missing values (geom_point).
```

<img src="document_files/figure-html/unnamed-chunk-3-1.png" width="672" />

To do:

[] visually compare measured depths with the IQ and the Hobos

[] need to better understand the remaining columns in the IQPlus dataset to see if we can systematically clean the data

[] do we need to group measurement events? ie. should the rating curve be built from certain portions of the hydrograph (need to see what the literature says)
