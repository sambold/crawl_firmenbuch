#' to_numeric
#' 
#' Wandelt String in numerischen Wert um
#' 
#' @param x String
#' @return numerischer Wert
#' @export
#' 
to_numeric <- function(x){
    x <- gsub("\\.","",x)
    x <- gsub(",","\\.",x)
    x <- as.numeric(x)
}