


## FUNCTION: convert girth from inches to centimeters
convert_into_cm <- function(x) {
    x * 2.54
}

## FUNCTION: convert height from feet to meters 
convert_into_m <- function(x) {
    x * 0.3048
}

## FUNCTION: calculate volume from girth and height in m^3
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter / 100)^2 * height
}

## FUNCTION: summarise iris dataset
calc_iris_mean <- function(data) {
    data |>  
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
}

## FUNCTION: summarise mean of numeric variables in a dataset 
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