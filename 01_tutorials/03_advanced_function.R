#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Shows that you can create the functions as complex as you want
# - Shows a small note in tidy eval
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

## Packages
library(tidyverse)

## Functions
source("...")

# 2. Load data ------------------------------------------------------------

## Load data
iris_tbl <- as_tibble(iris)

# 3. Analyze data ---------------------------------------------------------

## Calculate mean of numeric variables in Iris dataset, and sort them
iris_tbl |>  
    summarise(
        #across selects columns and then applies a transformation to them 
        across( 
            #where function simply selects the variables for which the following conditions are true
            where(is.numeric), mean
            #so basically, this takes the tbl and selects all the numeric columns, and then takes the mean
        ), 
        #by command shortcuts having to use group_by(Species)
        #and it gets arround potential weird knock-on effects of having groups lingering in the data
        .by = Species
    ) |> 
    #creates a long table from a wide table 
        #wide tables have multiple columns each rerpesenting different values of a single variable 
        #long tables have one column for values, and another indicating the variable type for each corresponding value
    pivot_longer( 
        cols      = where(is.numeric), 
        names_to  = "measure", 
        values_to = "mean"
    ) |> 
    group_by(measure) |> 
    arrange( 
        desc(mean), 
        .by_group = TRUE
    ) |> 
    ungroup()
    #now we can add this as a function to the utilities page

# 4. General function -----------------------------------------------------
calc_iris_mean(iris_tbl)

## https://ggplot2.tidyverse.org/reference/tidyeval.html

## Apply to other datasets









