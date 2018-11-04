#' Returns the cross-validation error for an input bandwidth
#' @param h Bandwidth
#' @param data Dataframe
#' @keywords creation
#' @export
#' @examples
#' smooth.cv()

smooth.cv <- function(h, data) {
  single.date <- function(date, h, data) {
    final.results <- list()
    temp <- data[data$time == date,]
    results=list()
    for (i in c(1:nrow(temp))) {
      try({
        train <- temp[-i,]
        test <- temp[i,]
        estimate <- calc.smooth(test$longitude,
                                test$latitude, train, h)
        results[[i]] <- (estimate-test$value)^2
      }, silent=TRUE)
    }
    mean.results <- mean(unlist(results))
    return(mean.results)
  }
  dates.mse <- list()
  dates <- unique(data$time)
  for (i in c(1:length(dates))) {
    dates.mse[i] <- single.date(dates[i], h, data)
  }
  mean <- mean(unlist(dates.mse), na.rm=T)
  return(mean)
}
