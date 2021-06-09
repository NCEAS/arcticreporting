#' Show list of cached arcticreport files
#'
#' This function returns a list of files cached for the package.
#'
#'
#' @return (list) A list of files
#' @export
#'
arcticreport_cache <- function() dir(rappdirs::user_cache_dir("arcticreport"), full.names = T)


#' Get the PIDs of all versions of an object
#'
#' Get the PIDs of all versions of an object.
#'
#' @param node (MNode) The Member Node to query.
#' @param pid (character) Any object in the chain.
#'
#' @return (character) A vector of PIDs in the chain, in order.
#' @importFrom dataone getSystemMetadata
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- "urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1"
#'
#' ids <- get_all_versions(mn, pid)
#' }
get_all_versions <- function(node, pid) {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid),
              nchar(pid) > 0)

    pids <- c(pid)

    # Walk backward
    sm <- getSystemMetadata(node, pid)

    while (!is.na(sm@obsoletes)) {
        oldsm <- sm # Save a copy for better warning messages
        tryCatch(sm <- getSystemMetadata(node, sm@obsoletes),
                 error = function(x){sm <- NULL})

        if (is.null(sm)) {
            warning(call. = FALSE,
                    paste0("An incomplete version chain has been returned. ", oldsm@identifier, " obsoletes ", oldsm@obsoletes, " but ", oldsm@obsoletes, " could not be found. This can be due to the object not existing or not having correct permission to view it."))
            break
        }

        pids <- c(sm@identifier, pids)
    }

    # Then forward from the start pid
    sm <- getSystemMetadata(node, pid)

    while (!is.na(sm@obsoletedBy)) {
        oldsm <- sm # Save a copy for better warning messages
        sm <- getSystemMetadata(node, sm@obsoletedBy)

        if (is.null(sm)) {
            warning(call. = FALSE,
                    paste0("An incomplete version chain has been returned. ", oldsm@identifier, " is obsoleted by ", oldsm@obsoletedBy, " but ", oldsm@obsoletedBy, " could not be found. This can be due to the object not existing or not having correct permission to view it."))
            break
        }

        pids <- c(pids, sm@identifier)
    }

    pids
}
