#' Query for version chains of all metadata objects on the Arctic Data Center
#'
#' This function takes a data frame of results from `query_objects`, filters for
#' metadata objects only, then assigns an arbitary series identifier to each
#' object.
#'
#' @param objects (data.frame) Result from `query_objects`
#' @param cache_tolerance (integer) Maximum number of days tolerable for age of
#' cached results. If the cached results are older than the cache tolerance, a
#' new dataset will be cached and the old cache deleted. To force a cache
#' refresh, set tolerance to zero.
#'
#' @return (data.frame) Metadata objects data.frame with series identifier
#'
query_version_chains <- function(objects, cache_tolerance = 14){

    if (is.null(getOption("dataone_token"))) {
        stop('No token set')
    }

    # set up cache
    if (!(dir.exists(rappdirs::user_cache_dir("arcticreport")))){
        dir.create(rappdirs::user_cache_dir("arcticreport"))
    }

    cfiles <- grep("versions", dir(rappdirs::user_cache_dir("arcticreport"), full.names = TRUE), value = TRUE)

    if (length(cfiles) == 0){
        cache_age <- cache_tolerance # set cache age equal to cache tolerance if no cached files are found
    } else {
        cache_age <- Sys.Date() - as.Date(stringr::str_extract(cfiles, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    }

    if (cache_age < cache_tolerance){
        meta <- readRDS(file = cfiles)
    } else if (cache_age >= cache_tolerance) {

        cn <- dataone::CNode("PROD")
        mn <- dataone::getMNode(cn, "urn:node:ARCTIC")

        meta <- objects[grep("informatics|gmd|xml", objects$formatId), ]

        unobsoleted <- meta[which(is.na(meta$obsoletedBy)), ]

        meta_end <- unobsoleted$id

        meta$seriesId <- NA
        for (i in 1:length(meta_end)){
            all_vers <- arcticdatautils::get_all_versions(mn, meta_end[i])
            z <- which(meta$id %in% all_vers)
            meta$seriesId[z] <- uuid::UUIDgenerate(n = 1)
            rm(z)
        }

        # remove old cached file
        file.remove(cfiles)
        # write new cache
        fpath <- file.path(rappdirs::user_cache_dir("arcticreport"), paste0("meta-versions-", Sys.Date(), ".rds"))
        saveRDS(meta, file = fpath)

    }
    return(meta)
}
