#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Introduces functions
# - Teaches to create basic R functions
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

library(tidyverse)

# 2. Load data ------------------------------------------------------------

## Load data
trees_tbl <- as_tibble(trees) |> 
    select(-Volume)

# 3. First function -------------------------------------------------------

## -> Convert girth from inches to centimeters

## 3.1. Calculate normally ---------------------

## Convert Girth from inches to cm
trees_tbl$Girth_cm <- trees_tbl$Girth * 2.54

## 3.2. Calculate with a function --------------

## Create function
convert_into_cm <- function(x) {
    x * 2.54
}

## Convert Girth from inches to cm
convert_into_cm(trees_tbl$Girth)

## 3.3. Calculate with mutate ------------------

## Convert Girth from inches to cm
trees_tbl |> 
    mutate(
        Girth_cm = trees_tbl$Girth * 2.54
    )

## 3.4. Function with mutate -------------------

## Convert Girth from inches to cm
trees_tbl |> 
    mutate(
        Girth_cm = convert_into_cm(trees_tbl$Girth)
    )
    
    #works the same as this: 
    trees_tbl |> 
        mutate(
            Girth_cm = convert_into_cm(Girth)
        )

# 4. Second function ------------------------------------------------------

## -> Convert height from feet to meters

## Convert feet to meters
convert_into_m <- function(x) {
    x * 0.3048
}

## Calculate height in meters
trees_tbl <- trees_tbl |> 
    mutate( 
        Height_m = convert_into_m(Height)
    )

# 5. Third function -------------------------------------------------------

## Calculate volume function
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter / 100)^2 * height
}

## Apply function
trees_tbl |> 
    mutate( 
        volume_m3 = calculate_volume(
            diameter = Girth_cm, 
            height = Height_m
        )    
    )

