#' Count number of unique creators
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of creators
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom data.table as.data.table
count_creators <- function(objects, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())) {

    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    origins_df <- objects %>%
        dplyr::filter(.data$formatType == "METADATA")

    # calculate number of wide columns to create
    max_len <- origins_df$origin  %>%
        lapply(length) %>%
        unlist() %>%
        max()
    # set env vars to NULL for R CMD check
    origin <- id <- formatId <- formatType <- size <- obsoletes <- dateUploaded <- obsoletedBy <- seriesId <- NULL
    # separate origins into columns and move to long format
    origins_df <- data.table::as.data.table(origins_df)
    origins_long <- origins_df[,
                               list(origin = as.character(unlist(origin))),
                               by = list(id,
                                    formatId,
                                    formatType,
                                    size,
                                    obsoletes,
                                    dateUploaded,
                                    obsoletedBy,
                                    seriesId)]



    # get initial dates for dataset uploads
    initial_dates <- origins_long %>%
        dplyr::arrange(.data$dateUploaded) %>%
        dplyr::group_by(.data$seriesId) %>%
        dplyr::summarise(initial_date = min(.data$dateUploaded))
    # create data frame of datasets with initial dates and filter out ANY dataset by a repeat creator across all of the datasets
    origins_new <- origins_long %>%
        dplyr::left_join(initial_dates, by = "seriesId") %>%
        dplyr::mutate(initial_date = as.POSIXct(.data$initial_date)) %>%
        dplyr::mutate(origin = tolower(.data$origin)) %>%
        dplyr::filter(!(duplicated(.data$origin)))


    # Grep-based filters
    # non-persons of some sort or another, found by scanning the list of unique creators
    omit <- c(".",
              "marine biologist",
              "Unknown",
              "Department of Biology",
              "hostdominic mullen")



    # filter out the omit list and grab the datasets by new creators for the date of interest
    origins_final <- origins_new %>%
        dplyr::filter(!(.data$origin %in% tolower(omit))) %>%
        dplyr::filter(.data$initial_date >= from & .data$initial_date <= to)

    # count the number of datasets with new creators
    origin_count <- nrow(origins_final)
    return(origin_count)
}
