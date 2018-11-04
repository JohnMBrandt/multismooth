#' A function to export individual plots of each time-series step
#' @param date Input dataframe
#' @param imputed.data Path to geojson
#' @param known.data Input dataframe
#' @param column Path to geojson
#' @param lim Input dataframe
#' @param path Path to geojson
#' @keywords creation
#' @export
#' @examples
#' create.plots()


create.plots <- function(date, imputed.data, known.data, column, lim, path) {
  library(ggplot2)
  subset <- imputed.data[imputed.data[[column]] == date,]
  known.data[[column]] <- as_datetime(known.data[[column]])
  subs.known <- known.data[known.data[[column]] == date,]
  plot1 <- ggplot(data=subset, aes(y=lat, x=long))+
    geom_tile(aes(fill=value), size=3)+
    geom_point(data = subs.known, aes(x=longitude, y=latitude, color=value))+
    theme_void()+
    scale_color_distiller(palette="Spectral", limits=lim, guide = F)+
    scale_fill_distiller(palette = "Spectral", limits=lim,
                         guide = guide_legend( keyheight = unit(2.5, units = "mm"),
                                               keywidth=unit(10, units = "mm"), label.position = "bottom",
                                               title.position = 'top', nrow=1))+
    ggtitle(as.character(as_datetime(date)))+
    theme(legend.position=c(0.77,0.2),
          plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47",
                                    margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
  name <- paste0(path, as.character(as_datetime(date)), ".png")
  ggsave(name, plot1, dpi=120, width=8, height=5)
}
