#' Count number of unique creators
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of creators
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
        str_split("\\|") %>%
        lapply(length) %>%
        unlist() %>%
        max()
    # separate origins into columns
    suppressWarnings(origins_df <- origins_df %>%
                         tidyr::separate(.data$origin, sep = "\\|", into = paste0("V", seq(1:max_len))))
    # move from wide to long
    origins_long <- origins_df %>%
        tidyr::pivot_longer(cols = starts_with("V"), names_to = "key", values_to = "origin") %>%
        dplyr::filter(!is.na(.data$origin))
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
    # Bryce created these (and we can expand these) based upon what I saw in the results
    # that looked like organizations or non-persons of some sort or another
    omit <- c("UCAR/NCAR",
              "University",
              "Institute",
              "Ynknown",
              "Transfer Unit",
              "Dept.",
              "NSF Arctic Data Center",
              "Coast Guard",
              "ACADIS",
              "Chesapeake Biological Laboratory",
              "Technologies",
              "Department",
              "Research Center",
              "Institution",
              "0214",
              "UCSD/SIO",
              "Technology",
              "LLC",
              "NCAR/EOL",
              "Data and Software Facility (CDS)",
              "Anthropology",
              "LGL",
              "National Resources Constulatants Inc",
              "Inc",
              "USGS Alaska",
              "Hydrology/Ecology",
              "Evolution & Marine Biology",
              "Ecology",
              "Department of Biology",
              "PAOS/CIRES",
              "Hydrology",
              "National Weather Service (NWS)",
              "Laboratory",
              "Hydrology",
              "Service",
              "Science",
              "Research",
              "Mathematics",
              "Center",
              "Administration",
              "Berkeley",
              "Museum",
              "U. S. Fish & Wildlife Service")
    # filter out the omit list and grab the datasets by new creators for the date of interest
    origins_final <- origins_new %>%
        dplyr::filter(!(.data$origin %in% tolower(omit))) %>%
        dplyr::filter(.data$initial_date >= from & .data$initial_date <= to)

    # count the number of datasets with new creators
    origin_count <- nrow(origins_final)
    return(origin_count)
}
