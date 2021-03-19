#' Theme function for plots
#'
#' @param font_size (integer) Base size for font
#'
#' @return ggplot theme object
#' @import ggplot2

plot_theme_adc <- function(font_size = 12){

    theme_bw(base_size=font_size,base_family="Helvetica") +
        theme(
            plot.title=element_text(face="bold",margin=margin(10,0,10,0),color="#1D244F"),
            plot.subtitle = element_text(margin=margin(0,0,10,0),color="#1D244F"),
            axis.text.x = element_text(angle=50, vjust=0.5, color="#1D244F"),
            axis.text.y = element_text(color="#1D244F"),
            axis.title.x = element_text(color="#1D244F",vjust=-.5),
            axis.title.y = element_text(color="#1D244F",angle=90,vjust=.5),
            panel.background=element_rect(fill="white"),
            axis.line = element_line(color="#1D244F"),
            panel.grid.major = element_line(colour = "gray", size = 0.01),
            panel.grid.minor = element_line(colour = "gray", size = 0.04),
        )
}
