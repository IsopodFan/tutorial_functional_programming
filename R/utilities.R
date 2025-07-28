


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