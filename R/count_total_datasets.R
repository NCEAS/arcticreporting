#' Count number of non-obsoleted metadata objects
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of datasets
count_total_datasets <- function(objects, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())) {

    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    dataset_count <- objects %>%
        dplyr::filter(formatType == "METADATA") %>%
        dplyr::filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", .data$formatId)) %>%
        dplyr::filter(is.na(.data$obsoletes)) %>%
        dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to)

    return(nrow(dataset_count))

}
