#' Combining mean, standard deviation and coefficient of variation
#'
#' This function combines mean, standard deviation and coefficient of variation to create reporting tables.
#'
#' @param x numeric vector
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' tabNCA(Theoph, dose=500, concUnit="mg/L") %>%
#'   as.data.frame() %>%
#'   summarise_all(meansdcv)

meansdcv <- function(x){
    paste0(mean(x), "|", sd(x))
}

