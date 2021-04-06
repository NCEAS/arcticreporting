#' Show list of cached arcticreport files
#'
#' This function returns a list of files cached for the package.
#'
#'
#' @return (list) A list of files
#' @export
#'
arcticreport_cache <- function() dir(rappdirs::user_cache_dir("arcticreport"), full.names = T)
