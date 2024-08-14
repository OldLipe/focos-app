library(httr2)
library(sf)

mun_brasil <- read.csv("./mun.csv")
mun_brasil <- tolower(mun_brasil$x)

get_request <- function(url, path, query) {
    req <- httr2::request(url)
    req <- httr2::req_url_path_append(req, path)

    req <- httr2::req_url_query(
        req, !!!query, .multi = "comma"
    )

    httr2::req_perform(req, verbosity = 1)
}


## Limite do municipio
get_city <- function(name) {
    name <- toupper(name)
    cql_filter <- sprintf("name_0='Brasil' AND name_2='%s'", name)
    url <- "https://terrabrasilis.dpi.inpe.br/queimadas/geoserver/"
    path <- "wfs"
    query <- list(
        "SERVICE" = "WFS",
        "REQUEST" = "GetFeature",
        "TYPENAMES" = "bdqueimadas:municipios",
        "CQL_FILTER" = cql_filter,
        "COUNT" = 1,
        "outputFormat" = "json"
    )
    resp <- get_request(url, path, query)
    resp <- httr2::resp_body_string(resp)
    sf::st_read(resp, quiet = TRUE)
}

get_focos <- function(city, start_date, end_date, sat_ref = 'AQUA_M-T') {
    city <- toupper(city)
    cql_filter <- sprintf(
        "municipio='%s' AND satelite in ('%s') AND data_hora_gmt between %sT00:00:00 and %sT23:59:59",
        city, sat_ref, start_date, end_date
    )
    year <- format(as.Date(start_date),"%Y")
    url <- "https://terrabrasilis.dpi.inpe.br/queimadas/geoserver/"
    path <- "wfs"
    query <- list(
        "SERVICE" = "WFS",
        "REQUEST" = "GetFeature",
        "TYPENAMES" = sprintf("dados_abertos:focos_%s_br_todosats", year),
        "CQL_FILTER" = cql_filter,
        "COUNT" = 100,
        "outputFormat" = "json"
    )
    resp <- get_request(url, path, query)
    resp <- httr2::resp_body_string(resp)
    sf::st_read(resp, quiet = TRUE)
}




