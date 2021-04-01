#' Return key information on all objects on the Arctic Data Center
#'
#' This function returns the identifier, origin, formatType, size,
#' dateUploaded, and obsoletedBy fields for every object on the
#' Arctic Data Center as a data.frame. This can then be used to extract
#' various metrics and plots.
#'
#' @param n (integer) Number of rows to return
#'
#' @return (data.frame) Result of the SOLR query
#'
query_objects <- function(n = 1000000){
    D1TOKEN <- getOption("dataone_token")

    cn <- dataone::CNode("PROD")
    mn <- dataone::getMNode(cn, "urn:node:ARCTIC")

    if (is.null(D1TOKEN)) {
        stop('No token set')
    }

    cd <- dataone::query(mn, list(q = '*:*',
                         fl = 'id,formatType,dateUploaded,obsoletes,obsoletedBy,formatId,origin,size',
                         sort = 'dateUploaded+desc',
                         rows = as.integer(n)),
                as = "data.frame")

    if (nrow(cd) >= 1000000) {
        stop("Solr query for this plot returned its maximum number of rows. Resutls may be truncated. You'll need to adjust the Solr query or this code to make sure results aren't truncated.")
    }

    return(cd)
}
