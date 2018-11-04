#' Calculate 2-d kernal smoother given input bandwidth
#' @param x X location from expand.grid
#' @param y Y location from expand.grid
#' @param data Path to expand.grid dataframe
#' @param h Bandwidth
#' @keywords creation
#' @export
#' @examples
#' calc.smooth()

calc.smooth <- function(x, y, data, h) {
  xi <- data$longitude
  yi <- data$latitude
  num.div.den <- (((x - xi)^2) + ((y - yi)^2))/(2*h^2)
  kh <- exp(-(num.div.den))
  calculated <- sum(data$value*kh)/sum(kh)
  return(calculated)
}
