function(input, output, session) {
  
  ###########################################################################
  #                                                                         #   
  # Params                                                                  #
  #                                                                         #
  ###########################################################################
  
  baseColor <- 'purple'
  selectColor <- 'yellow'
  bboxColor <- 'lightblue'
  baseRadius <- 5
  
  ###########################################################################
  #                                                                         #   
  # UI                                                                      #
  #                                                                         #
  ###########################################################################
  
  output$lastUpdate <- shiny::renderUI({
    lastUpdate <- DBI::dbGetQuery(con, 
      'select max(date) as last_update, count(1) as n_rows 
      from ava_canada_env as env
      inner join ava_canada_admin as adm on env.plot_number = adm.plot')
    shiny::tags$p(sprintf('The AVA-Canada dataset contains %s records. Last updated on %s.', 
      prettyNum(as.integer(lastUpdate[['n_rows']]), big.mark = ','), 
      format(as.Date(lastUpdate[['last_update']]), '%Y-%m-%d')))
  })
  shiny::observeEvent(input$selectZone, {
    shiny::updateSelectizeInput(session = session, inputId = 'selectSubzone',
      choices = subzones[input$selectZone])
  })
  
  ###########################################################################
  #                                                                         #   
  # Map                                                                     #
  #                                                                         #
  ###########################################################################
  
  envPlots <- shiny::reactive({
    DBI::dbGetQuery(con, 
      'select plot_number, latitude, longitude from ava_canada_env
      where latitude is not null and longitude is not null')
  })
  output$map <- leaflet::renderLeaflet({
    pal <- leaflet::colorFactor(palette = observable6(), domain = fg$fgcode)
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |> 
      leaflet::addMapPane('fg', zIndex = 410) |> 
      leaflet::addProviderTiles(provider = leaflet::providers$Esri.NatGeoWorldMap) |>
      leaflet::addCircleMarkers(data = envPlots(),
                                popup = ~plot_number,
                                lat = ~latitude, lng = ~-longitude, layerId = ~plot_number,
                                color = baseColor, radius = baseRadius, stroke = FALSE,
                                fillOpacity = .5) |>

       leaflet::addPolygons(data = fg, fillOpacity = .4, weight = 1,
                           options = leaflet::pathOptions(pane = 'fg'),
                           opacity = 1, color = ~pal(fgcode), popup = ~fieldguidename) |>
      
      leaflet::addPolylines(data = provinces, weight = 1, color = 'grey20') |>
      leaflet::addLabelOnlyMarkers(data = sf::st_centroid(fg, of_largest_polygon = TRUE),
                                   label = ~lapply(sprintf("<span style='color: %s'>%s</span>", pal(fgcode), fgcode), htmltools::HTML),
                                   labelOptions = leaflet::labelOptions(noHide = TRUE,
                                                                        textOnly = TRUE, direction = 'center', textsize = '12px',
                                                                        style = list('font-weight' = 'bold', color = 'white',
                                                                                     'background-color' = paste0('#ffffff', 'ff'),
                                                                                     'background-opacity' = .2, 'border-radius' = '5px'))) |>
      leaflet.extras::addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
            fillOpacity = .75,
            color = bboxColor,
            fillColor = bboxColor,
            clickable = TRUE)
        ),
        markerOptions = FALSE,
        circleMarkerOptions = FALSE, 
        editOptions = FALSE, 
        singleFeature = TRUE
      )
  })
  
 
  ###########################################################################
  #                                                                         #   
  # Bounding Box                                                            #
  #                                                                         #
  ###########################################################################
  
  bbox <- shiny::reactive({
    shiny::req(input$bboxNWLat, input$bboxNWLon, input$bboxSELat, 
      input$bboxSELon)
    sf::st_bbox(c(xmin = input$bboxNWLon, xmax = input$bboxSELon,
      ymin = input$bboxSELat, ymax = input$bboxNWLat), crs = 4326) |> 
      sf::st_as_sfc()
  })
  shiny::observeEvent(bbox(), {
    leaflet::leafletProxy('map') |> 
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |> 
      leaflet::clearGroup('bbox') |> 
      leaflet.extras::addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
            fillOpacity = .75,
            color = bboxColor,
            fillColor = bboxColor,
            clickable = TRUE)
        ),
        markerOptions = FALSE,
        circleMarkerOptions = FALSE, 
        editOptions = FALSE, 
        singleFeature = TRUE
      ) |> 
      leaflet::addPolygons(data = bbox(), color = bboxColor,
        fillColor = bboxColor, weight = 1, group = 'bbox',
        fillOpacity = .75, opacity = 1)
  })
  shiny::observeEvent(input$map_draw_new_feature, {
    shiny::updateNumericInput(session = session, inputId = 'bboxNWLat', 
      value = input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[2]])
    shiny::updateNumericInput(session = session, inputId = 'bboxNWLon', 
      value = input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[1]])
    shiny::updateNumericInput(session = session, inputId = 'bboxSELat', 
      value = input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[2]])
    shiny::updateNumericInput(session = session, inputId = 'bboxSELon', 
      value = input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[1]])
  })
  
  ###########################################################################
  #                                                                         #   
  #Select Areas                                                          #
  #                                                                         #
  ###########################################################################
  
  selectedPlots <- shiny::eventReactive(input$filterTop, {
    baseQuery <- 'select
      env.plot_number,
      env.project_id,
      env.fsregion_district,
      env.zone,
      env.sub_zone,
      env.latitude,
      env.longitude,
      env.date,
      env.successional_status,
      env.structural_stage,
      env.realm_class
      adm.site_plot_quality,
      adm.veg_plot_quality,
      adm.soil_plot_quality
      adm.province_state_territory

    from ava_canada_env as env
    inner join ava_canada_admin as adm on env.plot_number = adm.plot'
    # filterQuery <- vector(mode = 'character', length = 0)
    # if (!is.null(input$selectRegion)) {
    #   filterQuery <- append(filterQuery,
    #     sprintf('UPPER(%s(fsregion_district, 1, 3)) in (%s)', 
    #       ifelse(isShinyApps, 'substr', 'substring'),
    #       paste(DBI::dbQuoteString(con, input$selectRegion), collapse = ', '))
    #   )
    # }
    filterQuery <- vector(mode = 'character', length = 0)
    if (!is.null(input$selectGuide)) {
      filterQuery <- append(filterQuery,
                           sprintf('UPPER(fsregion_district) in (%s)', 
                                    #ifelse(isShinyApps, 'substr', 'substring'),
                                    paste(DBI::dbQuoteString(con, input$selectGuide), collapse = ', '))
      )
    }
    if (!is.null(input$selectZone)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(zone) in (%s)',
          paste(DBI::dbQuoteString(con, toupper(input$selectZone)), 
            collapse = ','))
      )
    }
    if (!is.null(input$selectSubzone)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(CONCAT(zone, sub_zone)) in (%s)',
          paste(DBI::dbQuoteString(con, toupper(input$selectSubzone)), 
            collapse = ','))
      )
    }
    if (!is.null(input$selectProjectId)) {
      filterQuery <- append(filterQuery,
        sprintf('project_id in (%s)',
        paste(DBI::dbQuoteString(con, input$selectProjectId), 
        collapse = ','))
      )
    }
    if (!is.na(input$bboxNWLat) && !is.na(input$bboxNWLon) &&
        !is.na(input$bboxSELat) && !is.na(input$bboxSELon)) {
      if (isTRUE(isShinyApps)) {
        bboxQuery <- sprintf(
          "ST_Intersects(MakePoint(-longitude, latitude, 4326), ST_GeomFromText('%s', 4326))",
          sf::st_as_text(bbox()))
      } else {
        bboxQuery <- sprintf(
          "ST_Intersects(ST_SetSRID(ST_MakePoint(-longitude, latitude), 4326)::geography, 'SRID=4326;%s'::geography)", 
          sf::st_as_text(bbox()))
      }
      filterQuery <- append(filterQuery, bboxQuery)
    }
    if (!is.null(input$selectPublicationId)) {
      pubProjectIds <- DBI::dbGetQuery(con,
        sprintf("select distinct plot_number 
        from ava_canada_link_refs_plots
        where ref_short_name in (%s)", 
          paste(DBI::dbQuoteString(con, input$selectPublicationId), 
            collapse = ','))
      )[['plot_number']]
      filterQuery <- append(filterQuery,
        sprintf('plot_number in (%s)',
        paste(DBI::dbQuoteString(con, pubProjectIds), 
          collapse = ','))
      )
    }

    ###########################################################################
    #                                                                         #   
    # Apply Filters                                                           #
    #                                                                         #
    ###########################################################################
    
    
        if (!is.na(input$minYear)) {
      filterQuery <- append(filterQuery,
        sprintf("date_part('year', date) >= '%s'", input$minYear)
      )
    }
    if (!is.na(input$maxYear)) {
      filterQuery <- append(filterQuery,
        sprintf("date_part('year', date) <= '%s'", input$maxYear)
      )
    }
    if (!is.null(input$successionalStatus)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(successional_status) in (%s)', 
        paste(DBI::dbQuoteString(con, toupper(input$successionalStatus)), 
          collapse = ', '))
      )
    }
    if (!is.null(input$structuralStage)) {
      filterQuery <- append(filterQuery,
        sprintf('LOWER(LEFT(structural_stage, 2)) in (%s)', 
          paste(DBI::dbQuoteString(con, input$structuralStage), collapse = ', '))
      )
    }
    if (!is.null(input$sitePlotQuality)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(site_plot_quality) in (%s)', 
          paste(DBI::dbQuoteString(con, toupper(input$sitePlotQuality)), 
            collapse = ', '))
      )
    }
    if (!is.null(input$vegPlotQuality)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(veg_plot_quality) in (%s)', 
          paste(DBI::dbQuoteString(con, toupper(input$vegPlotQuality)), 
            collapse = ', '))
      )
    }
    if (!is.null(input$soilPlotQuality)) {
      filterQuery <- append(filterQuery,
        sprintf('UPPER(soil_plot_quality) in (%s)', 
          paste(DBI::dbQuoteString(con, toupper(input$soilPlotQuality)), 
            collapse = ', '))
      )
    }
    if (input$locationAccuracy != '') {
      filterQuery <- append(filterQuery,
        sprintf('location_accuracy %s', input$locationAccuracy)
      )
    }
    if (!is.null(input$Realm_Class)) {
      filterQuery <- append(filterQuery,
                            sprintf('UPPER(realm_class) in (%s)', 
                                    paste(DBI::dbQuoteString(con, toupper(input$realm_class)), 
                                          collapse = ', '))
      )
    }
    if (isTRUE(input$inPublications)) {
      pubProjectIds <- DBI::dbGetQuery(con,
        "select distinct plot_number 
        from ava_canada_link_refs_plots
        where ref_short_name is not null")[['plot_number']]
      filterQuery <- append(filterQuery,
        sprintf('plot_number in (%s)',
          paste(DBI::dbQuoteString(con, pubProjectIds), 
            collapse = ','))
      )
    }
    # compile queries
    if (length(filterQuery) > 0) {
      query <- sprintf('%s\nWHERE %s', baseQuery, paste(filterQuery, collapse = '\n AND '))
    } else {
      query <- baseQuery
    }
    if (isTRUE(isShinyApps) && exists('bboxQuery')) {
      sf::st_read(dsn = 'ava_canada.gpkg', query = query)
    } else {
      DBI::dbGetQuery(con, query)
    }
  })
  shiny::observeEvent(selectedPlots(), {
    leaflet::leafletProxy('map') |> 
      leaflet::clearGroup('bbox') |> 
      leaflet::addCircleMarkers(data = envPlots(),
        lat = ~latitude, lng = ~-longitude, layerId = ~plot_number,
        color = baseColor, radius = baseRadius, stroke = FALSE, 
        fillOpacity = .5, 
        options = leaflet::markerOptions(zIndexOffset = -1000)) |> 
      leaflet::addCircleMarkers(data = selectedPlots(),
        lat = ~latitude, lng = ~-longitude, layerId = ~plot_number,
        color = selectColor, radius = baseRadius, stroke = FALSE, 
        fillOpacity = .5, 
        options = leaflet::markerOptions(zIndexOffset = -1000))
  })
  
  ###########################################################################
  #                                                                         #   
  # Download                                                                #
  #                                                                         #
  ###########################################################################
  
  output$noPlots <- shiny::renderUI({
    shiny::tags$h3(prettyNum(nrow(selectedPlots()), big.mark = ','))
  })
  tableQueries <- c(
    env = 'select * from ava_canada_env where plot_number in (%s)',
    admin = 'select * from ava_canada_admin where plot in (%s)',
    metadata = 'select distinct m.* from ava_canada_metadata as m 
    inner join ava_canada_env as e on m.project_id = e.project_id where plot_number in (%s)',
    humus = 'select * from ava_canada_humus where plot_number in (%s)',
    mineral = 'select * from ava_canada_mineral where plot_number in (%s)',
    veg = 'select * from ava_canada_veg where plot_number in (%s)')
  output$download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      shiny::req(selectedPlots()[['plot_number']])
      tmp <- tempdir()
      progress <- shiny::Progress$new()
      progress$set(message = 'Preparing data...', value = 0)
      inc <- 1 / length(tableQueries)
      if (input$outputFormat == 'CSVs') {
        mapply(function(query, fileName) {
          dataTable <- DBI::dbGetQuery(con,
            sprintf(query, paste(DBI::dbQuoteString(con, selectedPlots()[['plot_number']]), 
              collapse = ', '))
          )
          data.table::fwrite(dataTable, file = sprintf('%s/%s.csv', tmp, fileName))
          progress$inc(amount = inc)
        }, query = tableQueries, fileName = names(tableQueries))
        zip(file, files =  sprintf('%s/%s.csv', tmp, names(tableQueries)),
          extras = '-j')
      } else if (input$outputFormat == 'Access') {
        vPro <- paste(tmp, uuid::UUIDgenerate(), sep = '/')
        dir.create(vPro)
        file.copy('www/VPro64', vPro, recursive = TRUE)
        accCon <- DBI::dbConnect(odbc::odbc(), .connection_string = 
            sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s/%s;",
              vPro, 'VPro64/VPro64.accdb'))
        mapply(function(query, sampleTable) {
          dataTable <- DBI::dbGetQuery(con,
            sprintf(query, paste(DBI::dbQuoteString(con, selectedPlots()[['plot_number']]), 
              collapse = ', '))
          )
          names(dataTable) <- names(dataTable) |> convert_title_case()
          data.table::setDT(dataTable)
          dataTable <- dataTable[ , lapply(.SD, convert_insert, con = accCon)]
          dataTable <- dataTable[ , insert_id := 1:.N]
          colNames <- setdiff(names(dataTable), 'insert_id')
          sql <- dataTable[, .(sql = sprintf('INSERT INTO %s (%s) VALUES(%s);\n', 
            sprintf('Sample_%s', tools::toTitleCase(sampleTable)),
            paste(DBI::dbQuoteIdentifier(accCon, colNames), collapse = ', '),
            paste(.SD, collapse = ', '))), by = .(insert_id)][['sql']]
          lapply(sql, DBI::dbExecute, conn = accCon)
          progress$inc(amount = inc)
        }, query = tableQueries, sampleTable = names(tableQueries))
        setwd(vPro)
        zip(file, files = list.files(vPro, recursive = TRUE))
      } else {
        showNotification('Invalid output type', type = 'error')
      }
    },
    contentType = "application/zip"
  )
}
