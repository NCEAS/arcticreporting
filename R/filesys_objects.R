#' Return key information on objects stored on the filesystem outside of Metacat
#'
#' This function returns the identifier, formatType, size,
#' dateUploaded, and rentries fields for every data package on the
#' Arctic Data Center large data storage filesystem as a data.frame. 
#' This can then be combined with object counts from Metacat and used 
#' to extract various metrics and plots.
#' 
#' The function works by running local bash commands on the datateam host, and
#' will not run on other hosts
#'
#' @return (data.frame) Result of filesystem stat
#' @export
#'
#' @importFrom readr read_csv
#' @import dplyr
filesys_objects <- function(){
    
    # Get current large data sizes, must be run on datateam
    large_file <- paste0(getwd(), "/adc-large-data.csv")
    large_file_cmd <- paste0("cd /var/data/10.18739 && find . -maxdepth 1 -type d | grep A2 | xargs -n 1 basename | xargs -I @ -n 1 sh -c \"stat --printf='%n,%Y,' @ && getfattr --only-values -n ceph.dir.rbytes @ && echo -n ',' && getfattr --only-values -n ceph.dir.rentries @ && echo ''\" > ", large_file)
    processx::run("bash", c("-c", large_file_cmd))
    large_data <- readr::read_csv("adc-large-data.csv", col_names = c("id","dateUploaded","size","rentries")) %>%
        mutate(dateUploaded = as.POSIXct(dateUploaded),
               formatType = "DATA",
               size = size*1) %>% 
        arrange(dateUploaded)
    
    return(large_data)
}
