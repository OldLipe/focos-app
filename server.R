library(leaflet)
library(shiny)
library(ggplot2)
source("R/data-access.R")
source("R/source-fns.R")

# Load municipios data
municipios_br <- load_municipios()

function(input, output, session) {
    # Update options
    shiny::updateSelectizeInput(
        session = session, inputId = "city", choices = c("", municipios_br)
    )

    # Create the map
    output$map <- leaflet::renderLeaflet({
        leaflet() |>
            leaflet::setView(lng = -56.0949, lat = -15.5989, zoom = 4) |>
            leaflet::addTiles(
                urlTemplate = .source_url("google", "TMS"),
                options = leaflet::tileOptions(tms = FALSE),
                group = "Google Satellite"
            ) |>
            leaflet::addWMSTiles(
                baseUrl = .source_url("prodes", "WMS"),
                layers = "prodes-brasil-nb:prodes_brasil",
                group = "PRODES 2023"
            ) |>
            leaflet::addLayersControl(
                baseGroups = "google-satellite",
                overlayGroups = c("Focos Ativos", "Limite município", "PRODES 2023", "Sentinel-2-16D"),
                options = leaflet::layersControlOptions(collapsed = TRUE),
                position = "topleft"
            ) |>
            leaflet:::hideGroup(group = c("PRODES 2023", "Sentinel-2-16D", "Limite município")) |>
            leaflet::addEasyButtonBar(
                leaflet::easyButton(
                    icon = "fa-globe", title = "Zoom to Level 1",
                    onClick = JS("function(btn, map){ map.setZoom(1);}")
                ),
                leaflet::easyButton(
                    icon = "fa-crosshairs", title = "Locate Me",
                    onClick = JS("function(btn, map) {
                            var groupLayer = map.layerManager.getLayerGroup('Limite município');
                            map.fitBounds(groupLayer.getBounds());
                            }"
                    )
                )
            )
    })
    # Get municipios extent
    city <- shiny::reactive({
        city <- toupper(input$city)
        if (nzchar(city)) {
            city <- get_limite_municipio(city)
        }
        city
    })
    # Get municipios foco
    focos <- shiny::reactive({
        start_date <- input$daterange[[1]]
        end_date <- input$daterange[[2]]
        city_name <- input$city
        focos <- NULL
        if (nzchar(city_name)) {
            focos <- get_focos(
                mun_nome = city_name,
                start_date = start_date,
                end_date = end_date
            )
        }
        focos
    })
    # Get collection items
    stac_items <- shiny::reactive({
        cloud_percent <- input$slider
        start_date <- input$daterange[[1]]
        end_date <- input$daterange[[2]]
        city_name <- input$city
        stac_items <- NULL
        if (any(nzchar(city()))) {
            bbox <- as.numeric(sf::st_bbox(city()))

            stac_items <- get_stac_items(
                collection = "S2-16D-2",
                datetime = paste0(start_date, "/", end_date),
                bbox = bbox,
                cloud_percent = cloud_percent
            )
        }
        stac_items
    })

    shiny::observeEvent(input$daterange, {
        if (!nzchar(isolate(input$city))) {
            return(NULL)
        }
        leaflet::leafletProxy("map") |>
            leaflet::clearGroup("Focos Ativos") |>
            leaflet::addCircles(
                lng = ~longitude,
                lat = ~latitude,
                layerId = ~id,
                opacity = 1,
                radius = 50,
                color = "red",
                stroke = TRUE,
                highlightOptions = leaflet::highlightOptions(
                    color = "black",
                    weight = 5,
                    bringToFront = TRUE
                ),
                weight = 3,
                group = "Focos Ativos",
                data = focos()
            )
    })

    show_stac_items <- function(map, stac_items) {
        tiles <- unique(
            rstac::items_reap(stac_items, field = c("properties", "bdc:tiles"))
        )

        for (tile in tiles) {
            stac_item <- rstac::items_filter(
                stac_items, filter_fn = function(x) {
                    x$properties$`bdc:tiles` == tile
                })

            red <- rstac::assets_url(stac_item, asset_names = "B04")[[1]]
            green <- rstac::assets_url(stac_item, asset_names = "B03")[[1]]
            blue <- rstac::assets_url(stac_item, asset_names = "B02")[[1]]
            bdc_tms <- paste0(
                .source_url("bdc", "TMS"),
                "?url=%s&url=%s&url=%s&bands=1&bands=2&bands=3&rescale=0,2000&rescale=0,2000&rescale=0,2000"
            )
            asset_url <- sprintf(
                bdc_tms, red, green, blue
            )
             map |>
                leaflet::addTiles(
                    urlTemplate = asset_url,
                    options = leaflet::tileOptions(tms = FALSE),
                    group = "Sentinel-2-16D"
                )
        }
    }

    shiny::observeEvent(input$slider, {
        if (!nzchar(isolate(input$city))) {
            return(NULL)
        }
        show_stac_items(leaflet::leafletProxy("map"), stac_items())

    })


    shiny::observeEvent(input$city, {
        if (!nzchar(input$city)) {
            return(NULL)
        }
        mun_bbox <- sf::st_coordinates(sf::st_centroid(city()))
        leaflet::leafletProxy("map", data = focos()) |>
            leaflet::setView(
                lng = mun_bbox[[1]],
                lat = mun_bbox[[2]],
                zoom = 10
            ) |>
            leaflet::clearShapes() |>
            leaflet::addPolygons(
                data = city(),
                color = "black",
                weight = 3,
                dashArray = "3",
                group = "Limite município"
            ) |>
            leaflet::addCircles(
                lng = ~longitude,
                lat = ~latitude,
                layerId = ~id,
                opacity = 1,
                radius = 50,
                color = "red",
                stroke = TRUE,
                highlightOptions = leaflet::highlightOptions(
                    color = "black",
                    weight = 5,
                    bringToFront = TRUE
                ),
                weight = 3,
                group = "Focos Ativos"
            ) |>
            show_stac_items(stac_items())
    })

    # Show a popup at the given location
    show_metadata <- function(id, lat, lng) {
        foco <- focos()[focos()$id == id,]
        foco$precipitacao <- ifelse(
            !is.na(foco$precipitacao), as.integer(foco$precipitacao), 0
        )
        content <- as.character(tagList(
            tags$h4("Informações do foco"),
            tags$br(),
            sprintf("Data/Hora: %s", foco$data_hora_gmt), tags$br(),
            sprintf("Fire Radiative Power (FRP): %2.f (MW)", foco$frp), tags$br(),
            sprintf("LULC: %s", foco$vegetacao), tags$br(),
            sprintf("Precipitacao: %d (mm)", foco$precipitacao), tags$br(),
            sprintf("Satélite: %s", foco$satelite), tags$br(),
            sprintf("Dias sem chuva: %d", foco$numero_dias_sem_chuva)
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content)
    }

    # When map is clicked, show a popup with city info
    observeEvent(input$map_shape_click, {
        leafletProxy("map") |> clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
            return()

        isolate({
            show_metadata(event$id, event$lat, event$lng)
        })
    })

    output$hist <- renderPlot({
        if (!nzchar(input$city)) {
            return(NULL)
        }
        focos_filtered <- focos()
        focos_filtered$data_pas <- as.character(focos_filtered$data_pas)
        ggplot2::ggplot(focos_filtered, ggplot2::aes(x = data_pas)) +
            ggplot2::geom_bar( width = 0.5, fill = "red",
                               color = "#e9ecef", alpha = 0.9) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(size = 15, hjust = 0.5),
                axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
            ) +
            ggplot2::labs(x = "Data", y = "N. de Focos", title = "Frequência de Focos")
    })
}
