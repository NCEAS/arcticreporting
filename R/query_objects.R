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
query_objects <- function(n = 1000000, cache_tolerance = 14){

    # check for token
    D1TOKEN <- getOption("dataone_token")

    if (is.null(D1TOKEN)) {
        stop('No token set')
    }

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
        cn <- dataone::CNode("PROD")
        mn <- dataone::getMNode(cn, "urn:node:ARCTIC")

        cd <- dataone::query(mn, list(q = '*:*',
                                      fl = 'id,formatType,dateUploaded,obsoletes,obsoletedBy,formatId,origin,size',
                                      sort = 'dateUploaded+desc',
                                      rows = as.integer(n)),
                             as = "data.frame")

        if (nrow(cd) >= 1000000) {
            stop("Solr query for this plot returned its maximum number of rows. Resutls may be truncated. You'll need to adjust the Solr query or this code to make sure results aren't truncated.")
        }

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
