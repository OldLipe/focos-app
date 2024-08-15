source("R/request-fns.R")
source("R/source-fns.R")

get_limite_municipio <- function(mun_nome) {
    # Format city name to upper case
    mun_nome <- toupper(mun_nome)
    # Create CQL filter
    filter <- sprintf("name_0='Brasil' AND name_2='%s'", mun_nome)
    # Request wfs service
    resp <- request_wfs(
        source = "queimadas",
        path = "wfs",
        typename = "bdqueimadas:municipios",
        filter = filter,
        count = 1
    )
    # Parse response as string
    resp <- httr2::resp_body_string(resp)
    # Transform to sf object
    sf::st_read(resp, quiet = TRUE)
}

get_focos <- function(mun_nome, start_date, end_date, sat_ref = 'AQUA_M-T') {
    # Format city name to upper case
    mun_nome <- toupper(mun_nome)
    # Create CQL filter
    filter <- sprintf(
        paste0("municipio='%s' AND satelite in ('%s') AND data_hora_gmt ",
        "between %sT00:00:00 and %sT23:59:59"),
        mun_nome, sat_ref, start_date, end_date
    )
    # Update typename
    start_year <- format(as.Date(start_date),"%Y")
    typename <- sprintf("dados_abertos:focos_%s_br_todosats", start_year)
    # Request wfs service
    resp <- request_wfs(
        source = "queimadas",
        path = "wfs",
        typename = typename,
        filter = filter
    )
    # Parse response as string
    resp <- httr2::resp_body_string(resp)
    # Transform to sf object
    sf::st_read(resp, quiet = TRUE)
}

get_stac_items <- function(collection, datetime, bbox, cloud_percent) {
    stac_url <- .source_url("bdc", "STAC")
    rstac::stac(stac_url) |>
        rstac::stac_search(
            collections = collection,
            datetime = datetime,
            bbox = bbox,
            limit = 100,
        ) |>
        rstac::ext_query("eo:cloud_cover" <= as.numeric(cloud_percent)) |>
        rstac::post_request() |>
        rstac:::items_fetch()
}

load_municipios <- function(path) {
    read.csv("data/mun.csv")
}
