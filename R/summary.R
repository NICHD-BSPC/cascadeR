#' Summary tab module UI
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
summaryUI <- function(id, panel){
  ns <- NS(id)

  if(panel == 'sidebar'){
    tagList(
      br(),
      br(),
      span('Here we show a brief summary of the dataset',
           style='font-size: 15px;'),
      br(),
      br()
    ) # tagList
  } else if(panel == 'main'){
    tagList(
      withSpinner(
        uiOutput(ns('summary_tbl'))
      )
    ) # tagList
  }
} # summaryUI


#' Summary tab module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param args reactive with global arguments, 'project' & 'analysis'
#'
#' @export
#'
summaryServer <- function(id, obj, args){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        list(rds=obj$rds,
             obj_type=obj$obj_type,
             metadata=obj$metadata,
             qc=obj$qc)
      })

      output$summary_tbl <- renderUI({
        validate(
          need(!is.null(app_object()$rds), 'Waiting for selection')
        )

        obj_type <- app_object()$obj_type

        # get samples
        if('orig.ident' %in% colnames(app_object()$metadata)){
          samps <- unique(app_object()$metadata$orig.ident)
        } else {
          samps <- 'NA'
          showNotification(
            'Sample names not detected! "orig.ident" column absent in metadata',
            type='warning'
          )
        }

        if(obj_type == 'seurat'){
          # get summary of assays
          summ <- do.call('rbind', lapply(app_object()$rds@assays, function(x){
                            paste0(nrow(x), ' features')
                         }))
          summ2 <- paste0(rownames(summ), ': ', summ)

          # get number of cells
          ncells <- nrow(app_object()$metadata)

          # get dimension reductions
          dimred <- names(app_object()$rds@reductions)
        } else {
          ncells <- nrow(app_object()$metadata)

          # TODO: support for multiple assays?
          summ2 <- paste0(nrow(app_object()$rds$var), ' features')

          # TODO: move to function
          dimred <- names(app_object()$rds$obsm)
          names(dimred) <- sub('X_', '', dimred)

        }

        # NOTE: only supported for Seurat objects
        if(!is.null(app_object()$qc)){
          ncells_raw <- sum(unlist(
                              lapply(app_object()$qc$metadata, nrow)
                            ))
        }

        all_stats <- list(samples=samps,
                          summary=summ2,
                          dimred=dimred)

        # get obj summary
        rem_tags <- tagList(
          fluidRow(
            column(3,
              tags$div(
                class='div-stats-card',
                h3('Samples'),
                lapply(all_stats[[ 'samples' ]], tags$p),
              ) # div
            ), # column
            column(4,
              tags$div(
                class='div-stats-card',
                h3('Assays'),
                lapply(all_stats[[ 'summary' ]], tags$p)
              ) # div
            ), # column
            column(5,
              tags$div(
                class='div-stats-card',
                h3('Dimensional reductions'),
                lapply(all_stats[[ 'dimred' ]], tags$p)
              ) # div
            ) # column
          ) # fluidRow
        )

        # get cell summary row
        if(is.null(app_object()$qc)){
          cell_tags <- tagAppendChildren(
            tagList(
              tags$div(
                class='div-cells-card',
                fluidRow(
                  column(3,
                    h1(paste0(ncells, ' cells')),
                    br()
                  ) # column
                ) # fluidRow
              )
            ), # tagList
            tagList(
              rem_tags
            )
          ) # tagAppendChildren
        } else {
          cell_tags <- tagAppendChildren(
            tagList(
              fluidRow(
                column(6,
                  tags$div(
                    class='div-cells-card',
                    h1(paste0(ncells_raw, ' cells')),
                    h4('(raw)')
                  )
                ), # column
                column(6,
                  tags$div(
                    class='div-cells-card',
                    h1(paste0(ncells, ' cells')),
                    h4('(filtered)')
                  ) # div
                ) # column
              ) # fluidRow
            ), # tagList
            tagList(
              rem_tags
            )
          ) # tagAppendChildren
        }

        # generate ui
        tagList(
          br(),
          tags$div(
            fluidRow(
              column(6,
                span('Project:', style='font-size: 30px; font-weight: bold;'),
                span(basename(args()$project),
                     style='font-size: 30px; font-style: italic;')
              ) # column
            ), # fluidRow
            br(),
            fluidRow(
              column(6,
                span('Analysis:', style='font-size: 25px;'),
                span(basename(dirname(args()$analysis)),
                     style='font-size: 25px; font-style: italic')
              ) # column
            ) # fluidRow
          ), # div
          br(),

          cell_tags
        )

      })

    }
  ) # moduleServer
} # summaryServer
