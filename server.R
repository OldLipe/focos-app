library(leaflet)

function(input, output, session) {

    ## Interactive Map ###########################################

    # Create the map
    output$map <- renderLeaflet({
        leaflet() |>
            leaflet::addTiles(
                urlTemplate = "https://mt3.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}",
                options = leaflet::tileOptions(tms = FALSE),
                group = "Google Satellite"
            ) |>
            leaflet::addWMSTiles(
                baseUrl = "https://terrabrasilis.dpi.inpe.br/geoserver/prodes-brasil-nb/ows?SERVICE=WMS&REQUEST=GetMap&VERSION=1.1.1&LAYERS=prodes-brasil-nb%3Aprodes_brasil&STYLES=&WIDTH=256&HEIGHT=256&BBOX=-63.198620,-10.030140,-62.812901,-9.645115&FORMAT=image%2Fpng",
                layers = "prodes-brasil-nb:prodes_brasil",
                group = "PRODES"
            ) |>
            leaflet::addLayersControl(
                baseGroups = c("google-satellite"),
                overlayGroups = c("PRODES", "Limite município", "Focos Ativos", "BDC"),
                options = leaflet::layersControlOptions(collapsed = TRUE),
                position = "topleft"
            ) |>
            leaflet:::hideGroup(group = c("PRODES", "BDC")) |>
            addEasyButtonBar(
                easyButton(
                    icon = "fa-globe", title = "Zoom to Level 1",
                    onClick = JS("function(btn, map){ map.setZoom(1);}"))
            )
    })


    city <- reactive({
        city <- toupper(input$city)
        if (nzchar(city)) {
            city <- get_city(city)
        }
        city
    })

    observeEvent(input$city, {
        if (!nzchar(input$city)) {
            return(NULL)
        }
        mun_bbox <- sf::st_coordinates(sf::st_centroid(city()))
        leafletProxy("map", data = focos()) |>
            leaflet::setView(
                lng = mun_bbox[[1]],
                lat = mun_bbox[[2]],
                zoom = 10
            ) |>
            clearGroup("Limite município") |>
            leaflet::addPolygons(
                data = city(),
                color = "black",
                weight = 3,
                dashArray = "3",
                highlightOptions = leaflet::highlightOptions(
                    color = "black", weight = 5, bringToFront = TRUE),
                group = "Limite município"
            ) |>
            clearGroup("Focos Ativos") |>
            addCircles(~longitude, ~latitude, layerId = focos()$id, opacity = 1,
                       radius = 50, color = "red", stroke = TRUE,
                       highlightOptions = leaflet::highlightOptions(
                           color = "black", weight = 5, bringToFront = TRUE),
                       weight = 3
                       ) |>
            addEasyButtonBar(
                easyButton(
                    icon = "fa-crosshairs", title = "Locate Me",
                    onClick = JS(
                        sprintf(
                            "function(btn, map){ map.setView([%f,%f], 10);}",
                            mun_bbox[[2]], mun_bbox[[1]]
                        )
                    )
                )
            )
    })

    focos <- reactive({
        start_date <- input$daterange[[1]]
        end_date <- input$daterange[[2]]
        city_name <- isolate(input$city)
        focos <- NULL
        if (nzchar(city_name)) {
            focos <- get_focos(
                city = city_name,
                start_date = start_date,
                end_date = end_date
            )
        }
        focos
    })

    observeEvent(input$daterange, {
        if (!nzchar(isolate(input$city))) {
            return(NULL)
        }
        leafletProxy("map", data = focos()) |>
            clearGroup("Focos Ativos") |>
            addCircles(layerId = ~id, radius = 10, fillColor = "red")
    })


    # Show a popup at the given location
    show_metadata <- function(id, lat, lng) {
        print(focos())
        #foco <- focos()[focos()$id == id,]
        foco <- sf::st_drop_geometry(focos())

        print(foco)
        content <- as.character(tagList(
            tags$h4("Metadado"),
            tags$br(),
            sprintf("Data/Hora: %s", foco$data_hora_gmt), tags$br(),
            sprintf("Fire Radiative Power (FRP): %f", foco$frp), tags$br(),
            sprintf("Vegetação: %s", foco$vegetacao), tags$br(),
            sprintf("Precipitacao: %f mm", foco$precipitacao), tags$br(),
            sprintf("Dias sem chuva: %d", foco$numero_dias_sem_chuva)
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content)
    }

    # When map is clicked, show a popup with city info
    observeEvent(input$map_shape_click, {
        print("entroip aqui")
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        print(event)
        if (is.null(event))
            return()

        isolate({
            show_metadata(event$id, event$lat, event$lng)
        })
    })


    # # A reactive expression that returns the set of zips that are
    # # in bounds right now
    # zipsInBounds <- reactive({
    #     if (is.null(input$map_bounds))
    #         return(zipdata[FALSE,])
    #     bounds <- input$map_bounds
    #     latRng <- range(bounds$north, bounds$south)
    #     lngRng <- range(bounds$east, bounds$west)
    #
    #     subset(zipdata,
    #            latitude >= latRng[1] & latitude <= latRng[2] &
    #                longitude >= lngRng[1] & longitude <= lngRng[2])
    # })
    #
    # # Precalculate the breaks we'll need for the two histograms
    # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
    #
    # output$histCentile <- renderPlot({
    #     # If no zipcodes are in view, don't plot
    #     if (nrow(zipsInBounds()) == 0)
    #         return(NULL)
    #
    #     hist(zipsInBounds()$centile,
    #          breaks = centileBreaks,
    #          main = "SuperZIP score (visible zips)",
    #          xlab = "Percentile",
    #          xlim = range(allzips$centile),
    #          col = '#00DD00',
    #          border = 'white')
    # })
    #
    # output$scatterCollegeIncome <- renderPlot({
    #     # If no zipcodes are in view, don't plot
    #     if (nrow(zipsInBounds()) == 0)
    #         return(NULL)
    #
    #     print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
    # })
    #
    # # This observer is responsible for maintaining the circles and legend,
    # # according to the variables the user has chosen to map to color and size.
    # observe({
    #     colorBy <- input$color
    #     sizeBy <- input$size
    #
    #     if (colorBy == "superzip") {
    #         # Color and palette are treated specially in the "superzip" case, because
    #         # the values are categorical instead of continuous.
    #         colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #         pal <- colorFactor("viridis", colorData)
    #     } else {
    #         colorData <- zipdata[[colorBy]]
    #         pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #     }
    #
    #     if (sizeBy == "superzip") {
    #         # Radius is treated specially in the "superzip" case.
    #         radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #     } else {
    #         radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #     }
    #
    #     leafletProxy("map", data = zipdata) %>%
    #         clearShapes() %>%
    #         addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #                    stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #         addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #                   layerId="colorLegend")
    # })
    #
    # # Show a popup at the given location
    # showZipcodePopup <- function(zipcode, lat, lng) {
    #     selectedZip <- allzips[allzips$zipcode == zipcode,]
    #     content <- as.character(tagList(
    #         tags$h4("Score:", as.integer(selectedZip$centile)),
    #         tags$strong(HTML(sprintf("%s, %s %s",
    #                                  selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #         ))), tags$br(),
    #         sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    #         sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    #         sprintf("Adult population: %s", selectedZip$adultpop)
    #     ))
    #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    # }
    #
    # # When map is clicked, show a popup with city info
    # observe({
    #     leafletProxy("map") %>% clearPopups()
    #     event <- input$map_shape_click
    #     if (is.null(event))
    #         return()
    #
    #     isolate({
    #         showZipcodePopup(event$id, event$lat, event$lng)
    #     })
    # })
}
