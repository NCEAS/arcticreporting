#' Count number data citations
#'
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of citations in given time period
#' @export
#'

count_citations <- function(from = as.POSIXct("2010-01-01"), to = as.POSIXct(Sys.Date())){

    from <- as.Date(from); to <- as.Date(to)
    from_q <- paste(stringr::str_pad(lubridate::month(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::day(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::year(from), 2, side = "left", pad = "0"),
                    sep = "/")

    to_q <- paste(stringr::str_pad(lubridate::month(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(lubridate::day(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(lubridate::year(to), 2, side = "left", pad = "0"),
                  sep = "/")

    d <- jsonlite::fromJSON(paste0('https://logproc-stage-ucsb-1.test.dataone.org/metrics?q={%22metricsPage%22:{%22total%22:0,%22start%22:0,%22count%22:0},%22metrics%22:[%22citations%22],%22filterBy%22:[{%22filterType%22:%22repository%22,%22values%22:[%22urn:node:ARCTIC%22],%22interpretAs%22:%22list%22},{%22filterType%22:%22month%22,%22values%22:[%22', from_q,'%22,%22', to_q, '%22],%22interpretAs%22:%22range%22}],%22groupBy%22:[%22month%22]}'))

    "https://logproc-stage-ucsb-1.test.dataone.org/metrics?q={%22metricsPage%22:{%22total%22:0,%22start%22:0,%22count%22:0},%22metrics%22:[%22citations%22],%22filterBy%22:[{%22filterType%22:%22dataset%22,%22values%22:[%22https://pasta.lternet.edu/package/metadata/eml/knb-lter-mcr/1035/13%22],%22interpretAs%22:%22list%22},{%22filterType%22:%22month%22,%22values%22:[%2201/01/2010%22,%2206/09/2021%22],%22interpretAs%22:%22range%22}],%22groupBy%22:[%22month%22]}"





    return(sum(d$results$citations))

}
