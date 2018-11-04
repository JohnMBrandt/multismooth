#' A function to join point data to geojson
#' @param date Input dataframe
#' @param data Path to geojson
#' @param column Input dataframe
#' @param map Path to geojson
#' @param title Path to geojson
#' @keywords creation
#' @export
#' @examples
#' map.plot()


map.plot <- function(date, data, column, map, title) {
  map <- rgeos::gBuffer(map, byid=TRUE, width=0)
  map.df <- invisible(broom::tidy(map, region="SUBZONE_C"))
  subs <- data[data[[column]] == date,]
  map.df <- merge(map.df, subs, by.x="id", by.y="zone")
  library(ggplot2)
  mapplot <- ggplot()+
    geom_polygon(data=map.df, aes(x=long, y=lat, group=group, fill=value))+
    theme_void()+
    scale_fill_distiller(palette = "Spectral", limits=c(0,100),
                         guide = guide_legend( keyheight = unit(2.5, units = "mm"),
                                               keywidth=unit(10, units = "mm"), label.position = "bottom",
                                               title.position = 'top', nrow=1), name = title)+
    theme(legend.position=c(0.75,0.2), plot.title = element_text(size= 22, hjust=0.01,
                                                                 color = "#4e4d47", margin = margin(
                                                                   b = -0.1, t = 0.4, l = 2, unit = "cm")))+
    ggtitle(as.character(as_datetime(date)))+
    coord_map()
  name <- paste0("./gif/", as.character(as_datetime(date)), ".png")
  ggsave(name, mapplot, dpi=120, width=7, height=4.4)
}
