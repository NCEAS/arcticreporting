#' Count number of people accessing data
#'
#' @param path Path to file with downloads information from Metacat
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of unique people accessing data
#' @export
#'
#' @note In the postgres database, use query: COPY (
#'     SELECT * FROM access_log WHERE
#'     date_logged > '2021-02-01 00:00' AND
#'     date_logged < '2021-04-30 23:59' AND
#'     lower(event) = 'read'
#'     ORDER BY date_logged ASC
#' )
#' TO '/tmp/access_log.csv' WITH CSV HEADER;
#```

count_unique_accessors <- function(path, from = as.POSIXct("1899-01-01"), to = as.POSIXct(Sys.Date())){
    if (class(from)[1] == "character"){
        from <- as.POSIXct(from)
    }
    if (class(to)[1] == "character"){
        to <- as.POSIXct(to)
    }

    access <- data.table(read.csv(path, header=T, stringsAsFactors = F))



    # Grab the COUNTER-compliant UA filters
    robots <- httr::GET("https://raw.githubusercontent.com/atmire/COUNTER-Robots/master/generated/COUNTER_Robots_list.txt")
    robots <- httr::content(robots, encoding = "utf-8")
    robots <- readr::read_lines(robots)

    robots <- c(robots, "globalbioticinteractions/0.1.8 (https://globalbioticinteractions.org; mailto:info@globalbioticinteractions.org)")

    # Filter it now
    filtered_ip_patterns <- c("127.0.0.1",
                              "128.111.54.100",
                              "24.237.16.147",
                              "128.111.84.81",
                              "23.240.131.55",
                              "65.74.54.217",
                              "24.237.16.147",
                              "199.102.125.230")

    filtered_dn_patterns <- c("CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org",
                              "CN=Lauren Walker A10489,O=Google,C=US,DC=cilogon,DC=org",
                              "CN=Christopher Jones A2108,O=Google,C=US,DC=cilogon,DC=org",
                              "CN=Lauren Walker A10489,O=Google,C=US,DC=cilogon,DC=org",
                              "CN=urn:node:ARCTIC,DC=dataone,DC=org",
                              "uid=mecum,ou=Account,dc=ecoinformatics,dc=org",
                              "uid=cjones,ou=Account,dc=ecoinformatics,dc=org",
                              "CN=Matt Jones A729,O=Google,C=US,DC=cilogon,DC=org",
                              "http://orcid.org/0000-0001-8199-864X", # couture
                              "http://orcid.org/0000-0002-8121-2341", # c jones
                              "http://orcid.org/0000-0003-0077-4738", # m jones
                              "http://orcid.org/0000-0002-0381-3766", # mecum
                              "http://orcid.org/0000-0002-1006-9496", # goldstein
                              "http://orcid.org/0000-0002-9766-8044", # emery
                              "http://orcid.org/0000-0003-1180-9959", # john clark
                              "http://orcid.org/0000-0002-1586-0121", # heller
                              "http://orcid.org/0000-0002-8941-9259", # ng
                              "http://orcid.org/0000-0003-1758-9950", # leinfelder
                              "http://orcid.org/0000-0002-0480-2663", # tan
                              "http://orcid.org/0000-0003-2192-431X", # walker
                              "http://orcid.org/0000-0001-5966-0466", # prescott
                              "http://orcid.org/0000-0001-8775-4996", # raquel
                              "http://orcid.org/0000-0001-7489-5423", # su
                              "http://orcid.org/0000-0001-8778-5696", # meade
                              "http://orcid.org/0000-0003-4703-1974", # jeanette clark
                              "http://orcid.org/0000-0003-2077-855X", # oliver
                              "http://orcid.org/0000-0002-2192-403X", # slaughter
                              "http://orcid.org/0000-0002-4244-2865", # cornejo
                              "http://orcid.org/0000-0003-0047-8808", # louchouarn
                              "http://orcid.org/0000-0001-7847-0043", # mcgill
                              "http://orcid.org/0000-0003-0740-3649", # beck
                              "http://orcid.org/0000-0003-0003-2515", # bachtel
                              "http://orcid.org/0000-0002-1076-8342", # boyle
                              "http://orcid.org/0000-0001-9205-2948", # gordee
                              "http://orcid.org/0000-0001-7553-6692", # graybiel
                              "http://orcid.org/0000-0003-2245-3804", # halperin
                              "http://orcid.org/0000-0002-0979-560X", # kim
                              "http://orcid.org/0000-0001-6695-8383", # lee
                              "http://orcid.org/0000-0002-2561-5840", # mullen
                              "http://orcid.org/0000-0003-3283-554X", # ochs
                              "http://orcid.org/0000-0002-1681-9032", # sum
                              "http://orcid.org/0000-0002-3681-1337", # ou
                              "http://orcid.org/0000-0002-1209-5268", # jing tao
                              "http://orcid.org/0000-0003-3697-394X", # tran
                              "http://orcid.org/0000-0001-8594-506X", # sophia tao
                              "http://orcid.org/0000-0002-7822-2303", # sun
                              "http://orcid.org/0000-0002-4830-5378", # reeder
                              "http://orcid.org/0000-0002-6316-0058", # randazzo
                              "http://orcid.org/0000-0003-2057-2578", # nguyen
                              "http://orcid.org/0000-0002-7811-0504", # mohanti
                              "http://orcid.org/0000-0003-2878-3943", # foran
                              "http://orcid.org/0000-0002-2820-8714", # carlson
                              "http://orcid.org/0000-0001-7379-185X", # freund
                              "http://orcid.org/0000-0001-6955-0535", # maier
                              "http://orcid.org/0000-0002-6220-0134", # o'dean
                              "http://orcid.org/0000-0002-5511-9717", # steves
                              "http://orcid.org/0000-0003-0200-0787", # meyer
                              "http://orcid.org/0000-0002-3118-8697", # raymond
                              "http://orcid.org/0000-0002-7705-5670", # johnson
                              "http://orcid.org/0000-0003-1264-1166", # chong
                              "http://orcid.org/0000-0001-9663-2923", # mortensen
                              "http://orcid.org/0000-0003-0632-7576", # schildhauer
                              "http://orcid.org/0000-0002-0347-8574", # strong
                              "http://orcid.org/0000-0002-1615-3963", # thiessen-bock
                              "http://orcid.org/0000-0001-8874-7595", # kibele
                              "http://orcid.org/0000-0001-6885-9821", # reevesman
                              "http://orcid.org/0000-0003-1241-8351", # monper
                              "http://orcid.org/0000-0001-6602-1558", # pruett
                              "https://orcid.org/0000-0002-7822-2303", # sun
                              "https://orcid.org/0000-0003-3515-6710", # chen
                              "https://orcid.org/0000-0003-1070-9585", # clarin
                              "https://orcid.org/0000-0001-6618-3928", # erickson
                              "https://orcid.org/0000-0001-8888-547X", # lai
                              "https://orcid.org/0000-0001-8613-8956", # mclean
                              "https://orcid.org/0000-0002-6388-0901", # peach
                              "https://orcid.org/0000-0002-5248-9712") # samet

    # add more ORCID iDs above as needed. This should includ team members who submit, and/or download data from ADC.

    # Filter
    access_filtered <- access
    access_filtered <- access_filtered[!(access_filtered$ip_address %in% filtered_ip_patterns),]
    access_filtered <- access_filtered[!(access_filtered$principal %in% filtered_dn_patterns),]
    access_filtered <- access_filtered[!grepl(paste(robots,collapse = "|"), access_filtered$user_agent),]
    access_filtered <- access_filtered[access_filtered$user_agent != "globalbioticinteractions/0.1.8 (https://globalbioticinteractions.org; mailto:info@globalbioticinteractions.org)",]


    access_filtered$date_logged <- as.Date(access_filtered$date_logged)



    access_filtered_dt <- data.table(access_filtered)

    # change to dates for period of interest
    subset <- access_filtered_dt[date_logged >= as.Date(from) & date_logged <= as.Date(to)]

    # The metrics:

    # Unique people accessing metadata and data
    return(length(unique(access_filtered_dt$ip_address)))



}

