get_request <- function(url, path, query) {
    req <- httr2::request(url)
    req <- httr2::req_url_path_append(req, path)

    req <- httr2::req_url_query(
        req, !!!query, .multi = "comma"
    )

    httr2::req_perform(req, verbosity = 1)
}

request_wfs <- function(source, path, typename, filter = NULL, count = NULL) {
    # Default query
    query <- list(
        "SERVICE"      = "WFS",
        "REQUEST"      = "GetFeature",
        "outputFormat" = "json"
    )
    # user query
    user_query <- list(
        "TYPENAMES"  = typename,
        "CQL_FILTER" = filter,
        "COUNT"      = count
    )
    query <- modifyList(
        query, user_query
    )
    get_request(
        url = .source_url(source), path = path, query = query
    )
}

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
    cql_filter <- sprintf(
        paste0("municipio='%s' AND satelite in ('%s') AND data_hora_gmt ",
        "between %sT00:00:00 and %sT23:59:59"),
        mun_nome, sat_ref, start_date, end_date
    )
    # Create CQL filter
    start_year <- format(as.Date(start_date),"%Y")
    # Query list
    query <- list(
        "SERVICE"      = "WFS",
        "REQUEST"      = "GetFeature",
        "TYPENAMES"    = sprintf("dados_abertos:focos_%s_br_todosats", start_year),
        "CQL_FILTER"   = cql_filter,
        "outputFormat" = "json"
    )
    resp <- request_service(
        source = "queimadas", path = "wfs", query = query

    )
    # Parse response as string
    resp <- httr2::resp_body_string(resp)
    # Trasnform to sf object
    sf::st_read(resp, quiet = TRUE)
}
