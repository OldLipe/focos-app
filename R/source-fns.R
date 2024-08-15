.config <- function(source) {
    config[["sources"]][[source]]
}

.source_url <- function(source, service) {
    .config(source)[[service]]
}
