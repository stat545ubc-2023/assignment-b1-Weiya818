STAT545B Assignment B1 (Weiya Zhu)
================

# Exercise 1 Make a Function

In this exercise, I will make a function that is not complicated and
fortifying it.

``` r
# load the following packages
library(datateachr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

The function I made is a density_plot function since I used to plot many
density plot in Mini Data Analysis Project.

``` r
density_plot <- function(data, x, fill) {
  #make sure there is no NA value
  x_val<- x[!is.na(x)]
    if (any(x_val <= 0)) {
      stop("X column contains values smaller than or equal to 0. Please filter the data to remove zero or negative values before using this function.")
    }
    if(!is.numeric(x_val)) {
      stop('Please make sure x is a numeric vlaue')
    }
    if (!is.factor(fill) && !is.character(fill)) {
      stop('Make sure fill is a categorical variable!')
    }

  ggplot(data = data, aes(x = x)) +
    geom_density(aes(fill = fill), alpha = 0.5) +
    labs(title = "Density Plot", x = x, y = "Density") +
    scale_x_log10() +
    theme_minimal()
}
```

# Exercise 2: Document your Function

In this Exercise 2, I will document the function using roxygen2 tags

``` r
density_plot <- function(data, x, fill) {
  x_val<- x[!is.na(x)]
    if (any(x_val <= 0)) {
      stop("X column contains values smaller than or equal to 0. Please filter the data to remove zero or negative values before using this function.")
    }
    if(!is.numeric(x_val)) {
      stop('Please make sure x is a numeric vlaue')
    }
    if (!is.factor(fill) && !is.character(fill)) {
      stop('Make sure fill is a categorical variable!')
    }

  ggplot(data = data, aes(x = x)) +
    geom_density(aes(fill = fill), alpha = 0.5) +
    labs(title = "Density Plot", x = x, y = "Density") +
    scale_x_log10() +
    theme_minimal()
}
#' 1. density_plot
#' 2. @description The density_plot function creates a density plot wiht alpha=0.5 based on the given inputs, dataset, x, and fill values, using ggplot2.
#'  This function can be used as a visualization tool to represent the distribution of a continuous numeric variable (x) across multiple fills or categories(fill). 
#' 3.@param data The input dataset
#' @param x A continuous numerical variable in the dataset, it is one of the varibale being investigated
#' @param fill A categorical variable in the dataset, it is one of the varibale being investigated
#' 4.@return A density plot with value of x in x-axis across different fills or categories and denisity as the y-axis
```

# Exercise 3: Include examples

I will demonstrate the usage of the density_plot function with a few
examples.

**Example 1**

``` r
#filter the vancouver_trees datatset by filtering out the diameter column with diameter>0 to avoid the wanring message
filtered_dataset <- vancouver_trees %>%
  filter(diameter > 0)

#call the density_plot function to plot a density plot, which represents the distribution of the tree diameter across different root_barrier. 
density_plot( data=filtered_dataset, x=filtered_dataset$diameter, fill=filtered_dataset$root_barrier)
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Example 2**

``` r
#Use the filter_dataset above to filter out the diameter column with diameter>0
#Now, set the height_range_id column as a categorical column in order to use this function
filtered_dataset$height_range_id <- factor(filtered_dataset$height_range_id)

#call the density_plot function to plot a density plot for diameter versus height_range_id, where the different height_range_id are shown as fills
density_plot( data=filtered_dataset, x=filtered_dataset$diameter, fill=filtered_dataset$height_range_id)
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Example 3**

``` r
#Preprocess the dataframe to get the desired colun value
vancouver_trees <-vancouver_trees %>%
  #create a new dbl col year_planted through the data variable
   mutate(year_planted = year(date_planted))

# filter the dataset for the downtown neighborhood and the top 5 popular species
downtown_species <- vancouver_trees %>%
  filter(neighbourhood_name == "DOWNTOWN" & 
         year_planted >= 2013 & year_planted <= 2019 & 
         species_name %in% c("RUBRUM", "PLATANOIDES", "FREEMANI   X", "SYLVATICA", "BETULUS"))

#call the density_plot function to plot a density plot for the top 5 species in DOWNTOWN from 2013 to 2019
density_plot( data=downtown_species, x=downtown_species$year_planted, fill=downtown_species$species_name)
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Exercise 4: Test the Function

Running examples to check whether the function is working as expected.

**Test 1**

``` r
test_that("Test 1:Function creates a ggplot",{
  epected_val <- density_plot(filtered_dataset, filtered_dataset$diameter, filtered_dataset$root_barrier)
  expect_is(epected_val,"ggplot")
})
```

    ## Test passed 🥇

**Test 2**

``` r
test_that("Test 2: Function can only have continuous numerical x values", {
  expect_error(density_plot(filtered_dataset, filtered_dataset$root_barrier, filtered_dataset$diameter))
})
```

    ## Test passed 🥇

**Test 3**

``` r
test_that("Test 3: Function only takes three inputs", {
  expect_error(density_plot(filtered_dataset, filtered_dataset$root_barrier, filtered_dataset$diameter, filtered_dataset$height_range_id))
})
```

    ## Test passed 🥇

**Test 4**

``` r
test_that("Test 4: Function can only have categorical values as fill inputs", {
  expect_error(density_plot(filtered_dataset, filtered_dataset$diameter, filtered_dataset$tree_id))
})
```

    ## Test passed 🎊

**Test 5**

``` r
test_that("Test 5: Function can only accept x-value larger than 0", {
  expect_error(density_plot(vancouver_trees, vancouver_trees$diameter, vancouver_trees$root_barrier))
})
```

    ## Test passed 🥇
