#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Teaches to organize the functions in a different file
# - Source functions into other scripts
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

## Load packages
library(tidyverse) 

#it is often best practice to keep your functions and related code in a separate Rscript 
    #you can then recall those functions with the source command 
    #this keeps things organized

## Load functions 
source("R/utilities.R")

# 2. Load data ------------------------------------------------------------

## Load data
trees_tbl <- as_tibble(trees) |> 
    select(-Volume)

# 3. Calculate volume -----------------------------------------------------

## -> Convert girth from inches to centimeters
## -> Convert height from feet to meters
## -> Calculate volume in m3

trees_tbl |> 
    mutate( 
        Girth_cm  = convert_into_cm(Girth), 
        Height_m  = convert_into_m(Height), 
        Volume_m3 = calculate_volume(Girth_cm, Height_m)
    )












