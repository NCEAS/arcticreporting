#' Count number of new and updated datasets
#'
#' @param objects (data.frame) Table obtained from `query_version_chains`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of new and changed datasets
count_new_and_changed_datasets<- function(objects, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())) {

    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    changed_dataset_count <- objects %>%
        dplyr::filter(.data$formatType == "METADATA") %>%
        dplyr::filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", .data$formatId)) %>%
        dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to)
    # return number of unique series worked on that quarter
    return(length(unique(changed_dataset_count$seriesId)))

}
