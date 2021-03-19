#' Plot cumulative metric
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param metric (char) one of "files" or "datasets"
#' @param from Start date of plot (chatacter or POSIXct)
#' @param to End date of plot (character of POSIXct)
#' @param ... additional arguments to `plot_theme_adc`
#'
#' @return Plot of total datasets
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data

plot_cumulative_metric <- function(objects,
                                     metric,
                                     from = as.Date("2009-01-01"),
                                     to = as.Date(Sys.Date()),
                                     ...) {

    if (!(metric %in% c("files", "datasets"))){
        stop("Set plottting metric to one of: files, datasets")
    }

    if (class(from)[1] == "character"){
        from <- as.Date(from)
    }
    if (class(to)[1] == "character"){
        to <- as.Date(to)
    }

    # Plotting variables
    adc_launch <- as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles"))
    plot_start <- as.Date(lubridate::ymd("20160401", tz = "America/Los_Angeles"))

    if (metric == "files"){
        filter_metric <- "DATA"
    } else if (metric == "datasets"){
        filter_metric <- "METADATA"
    }


    datasets <- objects %>%
        dplyr::filter(.data$formatType == filter_metric) %>%
        dplyr::filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", .data$formatId, )) %>%
        dplyr::filter(is.na(.data$obsoletedBy)) %>%
        dplyr::filter(.data$dateUploaded >= as.Date(from) & .data$dateUploaded <= to) %>%
        dplyr::mutate(dateUploaded = as.Date(.data$dateUploaded)) %>%
        dplyr::arrange(as.Date(.data$dateUploaded)) %>%
        dplyr::mutate(count = 1) %>%
        dplyr::mutate(cumsum = cumsum(.data$count)) %>%
        dplyr::filter(.data$dateUploaded > plot_start + 1)


    datasets <- dplyr::bind_rows(data.frame(identifier = NA,
                                     dateUploaded = seq(plot_start, min(datasets$dateUploaded), by = "day"),
                                     count = 0,
                                     cumsum = min(datasets$cumsum)),
                          datasets)

    # Plotting
    g <- ggplot2::ggplot(datasets, ggplot2::aes(.data$dateUploaded, .data$cumsum)) +
        ggplot2::geom_line(size = 1, color="#1D244F") +
        ggplot2::labs(x = "Date Uploaded",
                     y = paste("Cumulative", stringr::str_to_title(metric))) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        plot_theme_adc(...)

    if (adc_launch > from & adc_launch < to){
        g <- g +
            ggplot2::geom_vline(xintercept = as.numeric(as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles"))), color = "#146660") +
            ggplot2::annotate(geom = "text",
                             x =  as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles")),
                             y = min(datasets$cumsum) + 5,
                             angle = 90,
                             hjust = -0.15,#-0.075,
                             vjust = 1.9,
                             label = "ADC Launch (April 5, 2016)",
                             color = "#146660",
                             size = 3)
    }

    return(g)
}
