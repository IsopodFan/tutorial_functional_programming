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
source("R/utilities.R")

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

calc_iris_mean(iris_tbl) 

# 4. General function -----------------------------------------------------
    #make a new version of the calc_iris_mean function that could apply to other data sets 

## https://ggplot2.tidyverse.org/reference/tidyeval.html

## Function 
calc_numeric_mean <- function(data, group) {
    data |>  
        summarise(
            across( 
                where(is.numeric), mean
            ), 
            .by = group
        ) |> 
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
}


## Apply to other datasets
airquality |>  
    summarise(
        across( 
            where(is.numeric), mean
        ), 
        .by = Month
    ) |> 
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

calc_numeric_mean( 
    data  = airquality, 
    group = Month
)
#the above throws an error because of tidyverse evals 
    #basically, within the first portion of the function 
    #the .by  = group is not looking for the group specified in the function input
    #instead its looking for a group within the data table 
    #you can fix this with curly braces, which tells R to look in the function inputs first

    #also in this iteration we clean NAs out with an anonymous function 
        #anonymous function is a function with no name used once 
        #it is introducts with "\(variable name)"

calc_numeric_mean <- function(data, group) {
    data |>  
        summarise(
            across( 
                where(is.numeric), \(x) mean(x, na.rm = TRUE)
            ), 
            .by = {{ group }}
        ) |> 
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
}

calc_numeric_mean( 
    data  = airquality, 
    group = Month
) |> print(n = 50)


