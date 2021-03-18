#' Get version chains of all metadata objects on the Arctic Data Center
#'
#' This function takes a data frame of results from `query_objects`, filters for
#' metadata objects only, then assigns an arbitary series identifier to each
#' object.
#'
#' @param objects (data.frame) Result from `query_objects`
#'
#' @return (data.frame) Metadata objects data.frame with series identifier
#'
get_version_chains <- function(objects){

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
    return(meta)
}
