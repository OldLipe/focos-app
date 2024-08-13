library(httr2)
library(sf)
library(png)
library(rstac)
library(leaflet)



## Focos de queimadas ativos
get_focos <- function(city, start_date, end_date, sat_ref = 'AQUA_M-T') {
    city <- toupper(city)
    cql_filter <- sprintf(
        "municipio='%s' AND satelite in ('%s') AND data_hora_gmt between %sT00:00:00 and %sT23:59:59",
        city, sat_ref, start_date, end_date
    )

    url <- "https://terrabrasilis.dpi.inpe.br/queimadas/geoserver/"
    path <- "wfs"
    query <- list(
        "SERVICE" = "WFS",
        "REQUEST" = "GetFeature",
        "TYPENAMES" = "dados_abertos:focos_2024_br_todosats",
        "CQL_FILTER" = cql_filter,
        "COUNT" = 100,
        "outputFormat" = "json"
    )
    resp <- get_request(url, path, query)
    resp <- httr2::resp_body_string(resp)
    sf::st_read(resp)
}

## Mapa prodes
url <- "https://terrabrasilis.dpi.inpe.br/geoserver/prodes-brasil-nb/"
path <- "ows"
query <- list(
    "SERVICE" = "WMS",
    "REQUEST" = "GetMap",
    "VERSION" = "1.1.1",
    "LAYERS" = "prodes-brasil-nb:prodes_brasil",
    "STYLES" = "",
    "WIDTH" = 256,
    "HEIGHT" = 256,
    "BBOX" = c(-63.19861955,-10.03014000,-62.81290067,-9.64511475),
    "FORMAT" = "image/png"
)
resp <- get_request(url, path, query)
httr2::resp_content_type(resp)
resp <- httr2::resp_body_raw(resp)
img <- png::readPNG(source = resp)
grid::grid.raster(img)
png::writePNG(img, "~/test.png")

## Imagem SENTINEL ou CBERS
# S2-16D-2
# CB4A-WPM-PCA-FUSED-1
rstac::stac("https://data.inpe.br/bdc/stac/v1/") |>
    rstac::stac_search(collections = "CB4A-WPM-PCA-FUSED-1", limit = 10,
                       datetime = "2024-08-01/2024-08-31") |>
    rstac::get_request()


leaflet::addWMSTiles(
    baseUrl = "https://terrabrasilis.dpi.inpe.br/geoserver/prodes-brasil-nb/ows?SERVICE=WMS&REQUEST=GetMap&VERSION=1.1.1&LAYERS=prodes-brasil-nb%3Aprodes_brasil&STYLES=&WIDTH=256&HEIGHT=256&BBOX=-63.198620,-10.030140,-62.812901,-9.645115&FORMAT=image%2Fpng",
    layers = "prodes-brasil-nb:prodes_brasil",
    group = "PRODES"
) |>
leaflet::addPolygons(
    data = mun,
    color = "black",
    weight = 3,
    dashArray = "3",
    fillOpacity = 0,
    highlightOptions = leaflet::highlightOptions(
        color = "black", weight = 5, bringToFront = TRUE),
    group = "Limite município"
) |>
leaflet::addMarkers(data = focos, group = "Focos Ativos") |>
leaflet::addLayersControl(
    baseGroups = c("OSM"),
    overlayGroups = c("PRODES", "Limite município", "Focos Ativos"),
    options = leaflet::layersControlOptions(collapsed = TRUE)
) |>

### Leaflet
leaflet::leaflet() |>
    leaflet::addWMSTiles(
        baseUrl = "https://terrabrasilis.dpi.inpe.br/geoserver/prodes-brasil-nb/ows?SERVICE=WMS&REQUEST=GetMap&VERSION=1.1.1&LAYERS=prodes-brasil-nb%3Aprodes_brasil&STYLES=&WIDTH=256&HEIGHT=256&BBOX=-63.198620,-10.030140,-62.812901,-9.645115&FORMAT=image%2Fpng",
        layers = "prodes-brasil-nb:prodes_brasil",
        group = "PRODES"
    ) |>
    leaflet::addPolygons(
        data = mun,
        color = "black",
        weight = 3,
        dashArray = "3",
        fillOpacity = 0,
        highlightOptions = leaflet::highlightOptions(
            color = "black", weight = 5, bringToFront = TRUE),
        group = "Limite município"
    ) |>
    leaflet::addMarkers(data = focos, group = "Focos Ativos") |>
    leaflet::addTiles(
        urlTemplate = "https://brazildatacube.dpi.inpe.br/tiler/tms/S2-16D_V2_012015_20170101/{z}/{x}/{y}.png?bands=B12,B8A,B04&color_formula=&collection=S2-16D-2&access_token=vtclkRc5c874XBd0Y7NSUWuzLihnU1bHC5bbLSXfQ5",
        options = leaflet::tileOptions(tms = FALSE, tileSize = 256),
        group = "BDC"
    ) |>
    leaflet::addTiles(
        urlTemplate = "https://brazildatacube.dpi.inpe.br/tiler/tms/S2-16D_V2_012016_20170101/{z}/{x}/{y}.png?bands=B12,B8A,B04&color_formula=&collection=S2-16D-2&access_token=vtclkRc5c874XBd0Y7NSUWuzLihnU1bHC5bbLSXfQ5",
        options = leaflet::tileOptions(tms = FALSE, tileSize = 256),
        group = "BDC"
    ) |>
    leaflet::addTiles(
        urlTemplate = "https://brazildatacube.dpi.inpe.br/tiler/tms/S2-16D_V2_013015_20170101/{z}/{x}/{y}.png?bands=B12,B8A,B04&color_formula=&collection=S2-16D-2&access_token=vtclkRc5c874XBd0Y7NSUWuzLihnU1bHC5bbLSXfQ5",
        options = leaflet::tileOptions(tms = FALSE, tileSize = 256),
        group = "BDC"
    ) |>
    leaflet::addTiles(
        urlTemplate = "https://brazildatacube.dpi.inpe.br/tiler/tms/S2-16D_V2_013016_20170101/{z}/{x}/{y}.png?bands=B12,B8A,B04&color_formula=&collection=S2-16D-2&access_token=vtclkRc5c874XBd0Y7NSUWuzLihnU1bHC5bbLSXfQ5",
        options = leaflet::tileOptions(tms = FALSE, tileSize = 256),
        group = "BDC"
    ) |>
    leaflet::addLayersControl(
        baseGroups = c("OSM"),
        overlayGroups = c("PRODES", "Limite município", "Focos Ativos", "BDC"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::flyToBounds(
        lng1 = -63.19861955,
        lat1 = -10.03014000,
        lng2 = -62.81290067,
        lat2 = -9.64511475
    )


# https://mt3.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}?
#
