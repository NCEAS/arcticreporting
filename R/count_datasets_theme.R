#' Count number of datasets by theme
#'
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#' @param theme_data Data on themes read from googlesheet (data.frame)
#'
#' @return data.frame of number of datasets by theme
#'
#' @importFrom rlang .data
#' @export
#'
count_datasets_theme <- function(from = as.POSIXct("2000-01-01"), to = as.POSIXct(Sys.Date()), theme_data){

    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    dat_long <- theme_data %>%
        dplyr::filter(.data$dateUploaded >= from & .data$dateUploaded < to) %>%
        tidyr::pivot_longer(cols = starts_with("theme"), names_to = "tier", values_to = "theme") %>%
        dplyr::filter(!is.na(theme))

    if (nrow(dat_long) == 0){
        dat_long <- data.frame("theme" = "oceanography", n = 0)
    }


    dat_summary <- dat_long %>%
        dplyr::group_by(.data$theme) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::arrange(.data$n) %>%
        dplyr::filter(.data$theme != "invalid") %>%
        tidyr::pivot_wider(names_from = .data$theme, values_from = .data$n, values_fill = 0)



    return(dat_summary)




}
