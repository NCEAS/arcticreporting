#' Return key information on all objects on the Arctic Data Center
#'
#' This function returns the identifier, origin, formatType, size,
#' dateUploaded, and obsoletedBy fields for every object on the
#' Arctic Data Center as a data.frame. This can then be used to extract
#' various metrics and plots.
#'
#' @param n (integer) Number of rows to return
#' @param cache_tolerance (integer) Maximum number of days tolerable for age of
#' cached results. If the cached results are older than the cache tolerance, a
#' new dataset will be cached and the old cache deleted. To force a cache
#' refresh, set tolerance to zero.
#'
#' @return (data.frame) Result of the SOLR query
#' @export
#'
#' @importFrom rlang .data
query_objects <- function(n = 10000000, cache_tolerance = 14){

    options(scipen = 100)

    # set up cache
    if (!(dir.exists(rappdirs::user_cache_dir("arcticreport")))){
        dir.create(rappdirs::user_cache_dir("arcticreport"))
    }

    cfiles <- grep("objects", dir(rappdirs::user_cache_dir("arcticreport"), full.names = TRUE), value = TRUE)

    if (length(cfiles) == 0){
        cache_age <- cache_tolerance # set cache age equal to cache tolerance if no cached files are found
    } else {
        cache_age <- Sys.Date() - as.Date(stringr::str_extract(cfiles, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    }

    if (cache_age < cache_tolerance){
        cd <- readRDS(file = cfiles)
    } else if (cache_age >= cache_tolerance) {

        # check for token
        D1TOKEN <- getOption("dataone_token")

        if (is.null(D1TOKEN)) {
            stop('No token set')
        }

        cn <- dataone::CNode("PROD")
        mn <- dataone::getMNode(cn, "urn:node:ARCTIC")


        cd <- data.frame()
        for (i in seq(0, n-1000, 1000)){
            res <- dataone::query(mn, list(q = '*:*',
                                          fl = 'id,formatType,dateUploaded,obsoletes,obsoletedBy,formatId,origin,size',
                                          sort = 'dateUploaded+desc',
                                          rows = 1000,
                                          start = i),
                                 as = "data.frame")
            cd <- dplyr::bind_rows(cd, res)
        }

        cd <- dplyr::filter(cd, !is.na(.data$id))


        meta <- cd[grep("informatics|gmd|xml", cd$formatId), ]

        unobsoleted <- meta[which(is.na(meta$obsoletedBy)), ]

        meta_end <- unobsoleted$id

        meta$seriesId <- NA
        for (i in 1:length(meta_end)){
            all_vers <- arcticdatautils::get_all_versions(mn, meta_end[i])
            z <- which(meta$id %in% all_vers)
            meta$seriesId[z] <- uuid::UUIDgenerate(n = 1)
            rm(z)
        }

        meta <- dplyr::select(meta, .data$id, .data$seriesId)

        cd <- dplyr::left_join(cd, meta, by = "id")


        # remove old cached file
        if (length(cfiles) > 0){
            file.remove(cfiles)
        }
        # write new cache
        fpath <- file.path(rappdirs::user_cache_dir("arcticreport"), paste0("objects-", Sys.Date(), ".rds"))
        saveRDS(cd, file = fpath)
    }



    return(cd)
}
