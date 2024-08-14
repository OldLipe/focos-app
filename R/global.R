library(httr2)
library(sf)
library(yaml)

config <- yaml::yaml.load_file(
    input = "data/config_endpoints.yaml",
    merge.precedence = "override"
)

config_env <- new.env()
config_env[["services"]] <- config
