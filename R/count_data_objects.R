#' Count number of new data objects
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of new data objects
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
count_data_objects <- function(objects, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())) {
    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    object_count <- objects %>%
        dplyr::filter(.data$formatType == "DATA") %>%
        dplyr::filter(is.na(.data$obsoletedBy)) %>%
        dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to)

    return(nrow(object_count))
}
