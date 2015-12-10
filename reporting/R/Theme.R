#' A Reference class to represent the theme on the plots.
#'
#' @author Zsolt Kovari
#'
#' @field font Character representing the font style. The default value is \code{"Helvetica"}.
#' @field x.text.size Numeric representing the size of the labels on the x-axis. Default value is 12.
#' @field y.text.size Numeric representing the size of the labels on the y-axis. Default value is 12.
#'
#'
#' @examples
#' theme <- Theme$new()
#' theme$x.text.size <- 14
#'
#' @export Theme
Theme <- setRefClass(Class = "Theme",
                     fields = list(
                       font = "character",
                       x.text.size = "numeric",
                       y.text.size = "numeric"
                     ),
                     methods = list(
                       initialize = function() {
                         font <<- "Helvetica"
                         x.text.size <<- 12
                         y.text.size <<- 12
                       }
                     ))
