source("R/source-fns.R")

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
        url = .source_url(source, "WFS"), path = path, query = query
    )
}
