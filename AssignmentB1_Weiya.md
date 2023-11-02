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
density plot in Mini Data Analysis Project. I filtered out all possible
NA value in x first. Then I set two pre-condition for my function.
Firstly, x should be a continuous numeric value and fill should be a
categorical value in order to make the density plot informative.

``` r
density_plot <- function(data, x, fill,x_axis_name) {
  #make sure there is no NA value
  x_val<- x[!is.na(x)]
    if(!is.numeric(x_val)) {
      stop('Please make sure x is a numeric vlaue')
    }
    if (!is.factor(fill) && !is.character(fill)) {
      stop('Make sure fill is a categorical variable!')
    }

  ggplot(data = data, aes(x = x_val)) +
    geom_density(aes(fill = fill), alpha = 0.5) +
    labs(title = "Density Plot", x = x_axis_name, y = "Density") +
    scale_x_log10() +
    theme_minimal()
}
```

# Exercise 2: Document your Function

In this Exercise 2, I will document the function using roxygen2 tags

``` r
density_plot <- function(data, x, fill, x_axis_name) {
  x_val<- x[!is.na(x)]
    if(!is.numeric(x_val)) {
      stop('Please make sure x is a numeric vlaue')
    }
    if (!is.factor(fill) && !is.character(fill)) {
      stop('Make sure fill is a categorical variable!')
    }

  ggplot(data = data, aes(x = x_val)) +
    geom_density(aes(fill = fill), alpha = 0.5) +
    labs(title = "Density Plot", x = x_axis_name, y = "Density") +
    scale_x_log10() +
    theme_minimal()
}
#' 1. density_plot
#' 2. @description The density_plot function creates a density plot wiht alpha=0.5 based on the given inputs, dataset, x, and fill values, using ggplot2.
#'  This function can be used as a visualization tool to represent the distribution of a continuous numeric variable (x) across multiple fills(categorical values).
#' 3.@param data The input dataset 
#' @param x A continuous numerical variable in the dataset, it is one of the varibale being investigated
#' @param fill A variable in the dataset, it is one of the varibale being investigated
#' @param x_axis_name This is the name of the x-axis given by the user who uses this function
#' 4.@return A density plot with value of x in x-axis across different fills and denisity as the y-axis
```

# Exercise 3: Include examples

I will demonstrate the usage of the density_plot function with a few
examples.

**Example 1**

``` r
#filter the vancouver_trees datatset by filtering out the diameter column with diameter>0 to avoid the warning message
filtered_dataset <- vancouver_trees %>%
  filter(diameter > 0)

#call the density_plot function to plot a density plot, which represents the distribution of the tree diameter across different root_barrier. 
density_plot( data=filtered_dataset, x=filtered_dataset$diameter, fill=filtered_dataset$root_barrier, x_axis_name="Diameter")
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Example 2**

``` r
#Use the filter_dataset above to filter out the diameter column with diameter>0
#Now, set the height_range_id column as a categorical column 
filtered_dataset$height_range_id <- factor(filtered_dataset$height_range_id)

#call the density_plot function to plot a density plot for diameter versus height_range_id, where the different height_range_id are shown as fills
density_plot( data=filtered_dataset, x=filtered_dataset$diameter, fill=filtered_dataset$height_range_id, x_axis_name="Diameter")
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Example 3**

``` r
#call the density_plot function to plot a density plot for radius_mean versus the results of diagnosis. 
#It is clear from the graph that when radius_mean ranges around 15, the density of the diagnosis B is extremely high,
#whereas when the radius_mean ranges from 17-20, the density of the diagnosis M is very high.
density_plot( data=cancer_sample, x=cancer_sample$radius_mean, fill=cancer_sample$diagnosis, x_axis_name="radius_mean")
```

![](AssignmentB1_Weiya_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Exercise 4: Test the Function

Running examples to check whether the function is working as expected.

**Test 1**

``` r
test_that("Test 1: Function creates a ggplot with a custom x-axis name", {
  expected_val <- density_plot(filtered_dataset, filtered_dataset$diameter, filtered_dataset$root_barrier, x_axis_name = "Diameter")
  expect_is(expected_val, "ggplot")
})
```

    ## Test passed 🎉

**Test 2**

``` r
test_that("Test 2: Function can only have continuous numerical x values", {
  expect_error(density_plot(filtered_dataset, filtered_dataset$root_barrier, filtered_dataset$diameter, x_axis_name="Diameter"))
})
```

    ## Test passed 😀

**Test 3**

``` r
test_that("Test 3: Function only takes four input variable", {
  expect_error(density_plot(filtered_dataset, filtered_dataset$root_barrier, filtered_dataset$diameter, filtered_dataset$height_range_id, x_axis_name="Diameter"))
})
```

    ## Test passed 🥳

**Test 4**

``` r
test_that("Test 4: Function can only have categorical fill values", {
  expect_error(density_plot( data=cancer_sample, x=cancer_sample$radius_mean, fill=cancer_sample$area_mean, x_axis_name="radius_mean"))
})
```

    ## Test passed 🥳
