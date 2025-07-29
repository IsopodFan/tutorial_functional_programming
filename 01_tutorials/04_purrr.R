#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Teaches you how to use purrr to iterate over lists
# - Compares for loop vs purrr::map()
# - Use of anonymous functions
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

pak::pak("ranger")

library(tidyverse)
library(ranger)

# 2. Load data ------------------------------------------------------------

iris_tbl <- as_tibble(iris)

# 3. Simple example -------------------------------------------------------

## Iterate over the list to create the message "This fruit is a <fruit_name>"
objects_list <- list("peach", "pear", "cherry", "strawberry", "blackberry")

## 3.1. Using for loop ---------------------------

## Create an empty list

messages_list <- list()
for (i in 1:length(objects_list)) { 
    
    messages_list[[i]] <- str_glue("This is a {objects_list[[i]]}")
    
}

## 3.2. Using functional programming -------------
messages_list <- map( 
    objects_list, 
    \(fruit) str_glue("This fruit is a {fruit}")
)

# 4. Model by species -----------------------------------------------------

## Create a different linear model for each Iris species

## 4.1. Using for loop ----------------------------

## Unique Species
species_vec <- unique(iris_tbl$Species)

## Create empty list
filtered_lst <- list()

## Iterate to filter each species
for (species in 1:length(species_vec)){ 
   
    ##filter observations 
    iris_filtered <- iris_tbl |> 
        filter( 
           Species == species_vec[species] 
        )
    ## calculate linear model 
    iris_filtered_lm <- lm(Petal.Length ~ Sepal.Length, data = iris_filtered)
    ## get summary 
    filtered_lst[[species]] <- summary(iris_filtered_lm)
}

## 4.2. Using functional programming --------------

## Create a function
calculate_iris_lm <- function(data) { 

    lm(Petal.Length ~ Sepal.Length, data = data) |>  
        summary()
    
}
        #issue is, the above function calculates it for all iris species 
        #we wanna specify species 

## Create list
iris_species_list <- iris_tbl |> 
    split(iris_tbl$Species)
    #split makes new tibbles for each unique input of the specified column 

## Iterate over the list
map( 
    iris_species_list, 
    calculate_iris_lm
)

## 4.3. In one step ---------------------------------
iris_tbl |> 
    split(iris_tbl$Species) |> 
    map(calculate_iris_lm)

## 4.4. Using anonymous function --------------------
iris_tbl |> 
    split(iris_tbl$Species) |> 
    map(
        \(data) lm(Petal.Length ~ Sepal.Length, data = data) |> 
            summary()
    )

# 5. Iterating 2 inputs --------------------------------------------------

## 5.1. Simple example ----------------------------

## Sum over the lists
fruits <- list("peach", "pear", "cherry", "strawberry", "blackberry")
colors <- list("orange", "green", "red", "red", "black")
rounded <- list("rounded", "not rounded", "rounded", "not rounded", "rounded")

## Iterate to create the sentence
map2( 
    .x = fruits, 
    .y = colors, 
    \(fruit, color) str_glue("the color of {fruit} is {color}")
)

## iterate over 3 vectors 
pmap(
    #first argument = list of input lists
    .l = list(
        fruits, 
        colors,
        rounded
    ),
    \(fruit, color, rounded) str_glue("the color of {fruit} is {color} and it is {rounded}")
)

## 5.2. A bigger example --------------------------

## Create a grid of parameters
    #expand grid creates a tibble with every combination of inputs
params_tbl <- expand_grid(
        ntree = c(100, 200, 500, 1000), 
        mtry  = 1:4
    )

## Create a Random Forest model for each parameter

    #random forest is an algorithm that goes through a bunch of different attributes in a data set 
    #and figures out which has best predictive power? 
    #or something like that idk I don't think it's the most important
ranger( 
    #specify the attribute to be predicted based on variables
    formula   = Sepal.Length ~ ., 
    #specify the data to draw variables from
    data      = iris_tbl, 
    #give the number of different trees to make
    num.trees = 500, 
    #then tell it how many attributes to use at a time
    mtry      = 1
)

iris_rf_list <- map2(
    .x = params_tbl$ntree,
    .y = params_tbl$mtry,
    \(ntree, mtry) ranger(
        formula   = Sepal.Length ~ ., 
        data      = iris_tbl, 
        num.trees = ntree, 
        mtry      = mtry
    )
)

## Extract r.squared
map( 
    iris_rf_list, 
    \(rf_model) rf_model$r.squared
)

## Add as a new column
params_tbl |> 
    mutate(
        rsq = map_dbl(
            iris_rf_list, 
            \(rf_model) rf_model$r.squared
        )
    ) |> 
    arrange(desc(rsq))

iris_tbl
