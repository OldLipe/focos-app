library(yaml)

config <- yaml::yaml.load_file(
    input = "data/config_endpoints.yaml",
    merge.precedence = "override"
)
