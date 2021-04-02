#' Plot cumulative metric
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param type (char) Object type to aggregate, one of "data" or "metadata"
#' @param metric (char) Type of aggregation, one of "count" or "size"
#' @param from Start date of plot (chatacter or POSIXct)
#' @param to End date of plot (character of POSIXct)
#' @param ... additional arguments to `plot_theme_adc`
#'
#' @return Plot of total datasets
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data

plot_cumulative_metric <- function(objects,
                                     type,
                                     metric,
                                     from = as.Date("2009-01-01"),
                                     to = as.Date(Sys.Date()),
                                     ...) {

    if (!(type %in% c("data", "metadata"))){
        stop("Set file type to one of: data, metadata")
    }
    if (!(metric %in% c("count", "size"))){
        stop("Set file type to one of: count, size")
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



    datasets <- objects %>%
        dplyr::filter(.data$formatType == toupper(type)) %>%
        dplyr::filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", .data$formatId, )) %>%
        dplyr::filter(is.na(.data$obsoletedBy)) %>%
        dplyr::filter(.data$dateUploaded >= as.Date(from) & .data$dateUploaded <= to) %>%
        dplyr::mutate(dateUploaded = as.Date(.data$dateUploaded)) %>%
        dplyr::mutate(size_kb = as.numeric(.data$size)/1024) %>%
        dplyr::arrange(as.Date(.data$dateUploaded)) %>%
        dplyr::mutate(count = 1) %>%
        dplyr::mutate(cumcount = cumsum(.data$count)) %>%
        dplyr::mutate(cumsize = cumsum(.data$size_kb)/1e9) %>%
        dplyr::filter(.data$dateUploaded > plot_start + 1)


    # datasets <- dplyr::bind_rows(data.frame(identifier = NA,
    #                                  dateUploaded = seq(plot_start, min(datasets$dateUploaded), by = "day"),
    #                                  count = 0,
    #                                  cumsum = min(datasets$cumcount),
    #                                  cumsize = 0),
    #                       datasets)

    # Plotting
    if (metric == "count"){
        g <- ggplot2::ggplot(datasets, ggplot2::aes(.data$dateUploaded, .data$cumcount))
        min_y <- min(datasets$cumcount)
    } else if (metric == "size"){
        g <- ggplot2::ggplot(datasets, ggplot2::aes(.data$dateUploaded, .data$cumsize))
        min_y <- min(datasets$cumsize)
    }

    g <- g + ggplot2::geom_line(size = 1, color="#1D244F") +
        ggplot2::labs(x = "Date Uploaded",
                      y = paste("Cumulative", stringr::str_to_title(metric))) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        plot_theme_adc(...)

    if (adc_launch > from & adc_launch < to){
        g <- g +
            ggplot2::geom_vline(xintercept = as.numeric(as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles"))), color = "#146660") +
            ggplot2::annotate(geom = "text",
                             x =  as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles")),
                             y = min_y,
                             angle = 90,
                             hjust = -0.15,#-0.075,
                             vjust = 1.9,
                             label = "ADC Launch (April 5, 2016)",
                             color = "#146660",
                             size = 3)
    }

    return(g)
}
