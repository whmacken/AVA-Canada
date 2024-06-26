
fluidPage(
  windowTitle = 'Arctic Ecosystem Plots',
  shiny::tags$head(
    shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    ),
  theme = bcgov_theme,
  inverse = TRUE,
  shiny::fluidRow(
    shiny::tags$div(
      style=sprintf("display: flex; justify-content: space-between; flex-wrap: wrap; align-content: center; padding: .5rem; background-color: %s", bcgov_primary()[1]),
      shiny::tags$h3('Arctic Vegetation Archive - Canada', style="color: white; margin-top: auto; margin-bottom: auto"),
      #shiny::tags$img(src='gov_bc_logo.svg', height = 35)
      
    ),
    shiny::column(width = 3,
      card(title = NULL,
        style = 'margin-top: .5rem;',
        shiny::tags$p(
          'Use this page to extract plot data from the AVA-Canada. Please begin by selecting plots by area, CAVM subzone or project. This selection can then be filtered to return only plots that match selected criteria. After entering your selection click “Apply Filter” to see the number and location of plots in the right hand pane.
          Choose the format of data and click download.'),
        shiny::uiOutput('lastUpdate')
      ),
      shiny::uiOutput('instructions'),
      card(title = 'Select Plots by Area', style = 'margin-top: .5rem;',
          shiny::fluidRow(
            shiny::column(width = 3,
              shiny::selectizeInput(inputId = 'selectGuide',
                label = 'Select Fieldguide', multiple = TRUE,
                #selectize = FALSE, size = 2,
                choices = guides) |> small_selectize()
            ),
            #shiny::column(width = 3,
              # shiny::selectizeInput(inputId = 'selectZone',
              #   label = 'Select BGC Zone', multiple = TRUE,
              #   choices = zones) |> small_selectize()
              #   ),
            shiny::column(width = 3,
              shiny::selectizeInput(inputId = 'selectProvince',
              multiple = TRUE,
              label = 'Select Province', choices = provinces$prov)
            ),
            shiny::column(width = 3,
              shiny::selectInput(inputId = 'selectCAVM',
                label = 'Select CAVM', multiple = TRUE,
                choices = cavm$cavm_subzone) |>
                small_selectize()
              )
            ),
          # shiny::fluidRow(
          #   # shiny::column(width = 4,
          #   #   shiny::selectizeInput(inputId = 'selectProjectId',
          #   #     multiple = TRUE,
          #   #     label = 'Select Projects', choices = projectIds$project_id) |>
          #   #     small_selectize()
          #   #   ),
          #     shiny::column(width = 4,
          #       shiny::selectizeInput(inputId = 'selectProvince',
          #         multiple = TRUE,
          #         label = 'Select Province', choices = provinces$prov)
          #     )
          #   ),
          shiny::fluidRow(
            shiny::column(width = 6,
              shiny::tags$h6('Northwest Corner'),
              shiny::tags$hr(style='height:2px; opacity: 1;'),
              shiny::fluidRow(
                shiny::column(width = 6,
                  shiny::numericInput('bboxNWLat', 'Latitude', value = NA,
                    min = -90, max = 90)
                  ),
                shiny::column(width = 6,
                  shiny::numericInput('bboxNWLon', 'Longitude', value = NA,
                    min = -180, max = 180)
                )
              )
            ),
            shiny::column(width = 6,
              shiny::tags$h6('Southeast Corner'),
              shiny::tags$hr(style='height:2px; opacity: 1;'),
              shiny::fluidRow(
                shiny::column(width = 6,
                  shiny::numericInput('bboxSELat', 'Latitude', value = NA,
                    min = -90, max = 90)
                ),
                shiny::column(width = 6,
                  shiny::numericInput('bboxSELon', 'Longitude', value = NA,
                    min = -180, max = 180)
                )
              )
            )
          )
      ),
     card(title = 'Filter Selected Plots', style = 'margin-top: .5rem;',
    shiny::fluidRow(
    #   shiny::column(width = 3,
    #     shiny::numericInput(inputId = 'minYear', 'Start year',
    #       value = NA, step = 1L)
    #   ),
    #   shiny::column(width = 3,
    #     shiny::numericInput(inputId = 'maxYear', 'End year',
    #       value = NA, step = 1L)
    #   ),
    #   shiny::column(width = 3,
    #     shiny::selectizeInput('successionalStatus', 'Successional Status',
    #       choices = successionalStatus, multiple = TRUE
    #     )
    #   ),
    #   shiny::column(width = 3,
    #     shiny::selectInput('structuralStage', 'Structural Stage',
    #       multiple = TRUE, choices = structuralStage)
    #   )
    # ),
    #   shiny::fluidRow(
    # shiny::column(width = 3,
    #   shiny::selectizeInput('sitePlotQuality', 'Site Plot Quality',
    #     choices = sitePlotQuality,
    #     multiple = TRUE)
    # ),
    # shiny::column(width = 3,
    #               shiny::selectizeInput('Realm_Class', 'Realm_Class',
    #                                     choices = Realm_Class,
    #                                     multiple = TRUE)
    # ),
      # shiny::column(width = 3,
      #   shiny::selectizeInput('vegPlotQuality', 'Veg Plot Quality',
      #     choices = vegPlotQuality,
      #     multiple = TRUE)
      # ),
      # shiny::column(width = 3,
      #   shiny::selectizeInput('soilPlotQuality', 'Soil Plot Quality',
      #     choices = soilPlotQuality,
      #     multiple = TRUE)
      # ),
      shiny::column(width = 3,
        shiny::selectizeInput(inputId = 'locationAccuracy',
          label = 'Location Accuracy', multiple = FALSE,
          choices = c('', locationAccuracy))
      )
    ),
    # shiny::fluidRow(
    #   shiny::column(width = 12,
    #     shiny::checkboxInput(inputId = 'inPublications',
    #       'Plots used in published classification only?', value = FALSE)
    #   )
    # )
      ),
      shiny::actionButton('filterTop', 'Apply', style = 'margin-top: .5rem;')
    ),
    shiny::column(width = 7,
      style = 'padding-left: 0px;',
      card(title = 'Arctic Plot Locations', style = 'margin-top: .5rem;',
        leaflet::leafletOutput('map', height = 500)
      ),
      card(title = 'Download Selected Data', style = 'margin-top: .5rem;',
        shiny::fluidRow(
          shiny::column(width = 4, 
            shiny::tags$h5('Number of plots selected'),
            shiny::uiOutput('noPlots')
            ),
          shiny::column(width = 4,
            shiny::selectInput('outputFormat', 'Download', 'Format', choices = 
                c('Access', 'CSVs', 'TurboVeg [not yet available]',
                  'R long format [not yet available]'))
          ),
          shiny::column(width = 4,
            shiny::tags$div(
              style = 'display: flex; flex-wrap: wrap; align-content: center; height: 100%;',
            shiny::downloadButton('download', label = 'Download')
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 12,
            shiny::tags$p('This data is available under the ', 
              shiny::tags$a(
                href = 'https://creativecommons.org/licenses/by/4.0/',
                'Creative Commons by Attribution license'),
              '.'),
            shiny::tags$p('Please include the following statement in the publication, presentation, or dissemination of any analysis conducted with the AVA-Canada data:
"The plot data used in this analysis was provided by the Arctic Vegetation Archive - Canada"'),
            shiny::tags$p('Formal citation: W.H. MacKenzie, W.H. 2024. Arctic Vegetation Archive - Canada: Ecosystem plot database, https:// .Retrieved [accessed date]')
          )
        )
      )
    )
  )
)

