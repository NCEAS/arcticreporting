#' Count total volume
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Volume in TB of data center
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
count_volume <- function(objects, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())) {
    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    object_vol <- objects %>%
        dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to) %>%
        dplyr::mutate(size_kb = as.numeric(.data$size)/1024)

    return(sum(object_vol$size_kb)/1e9)
}
