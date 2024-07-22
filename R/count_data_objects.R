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
        if (class(from)[1] == "character") {
            from <- as.POSIXct(from)
        }
        if (class(to)[1] == "character") {
            to <- as.POSIXct(to)
        }
        
        if (is.null(objects$rentries)) {
            warning(
                "No file system counts found. If you want to include file system counts run `query_filesys_objects()."
            )
            objects$rentries <- NA
        }
        
        object_count_metacat <- objects %>%
            dplyr::filter(.data$formatType == "DATA") %>%
            dplyr::filter(is.na(.data$obsoletedBy)) %>%
            dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to) %>%
            dplyr::filter(is.na(.data$rentries))
        
        object_count_filesys <- objects %>%
            dplyr::filter(.data$formatType == "DATA") %>%
            dplyr::filter(is.na(.data$obsoletedBy)) %>%
            dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded <= to) %>%
            dplyr::filter(!is.na(.data$rentries))
        
        filesys_count <-
            sum(object_count_filesys$rentries, na.rm = TRUE)
        
        
        return(nrow(object_count_metacat) + filesys_count)
    }
