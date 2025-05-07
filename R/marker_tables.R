#' Marker tables module UI
#'
#' This is a master wrapper for generalized marker tables.
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
markerTableUI <- function(id, panel){
  ns <- NS(id)

  if(panel == 'global'){
    tagList(
      div(id=ns('assay_menu'),
          selectizeInput(ns('all_assay'), label='Assay',
                         choices=NULL,
                         selected=NULL,
                         multiple=TRUE)
      ), # div
      div(id=ns('resolution_menu'),
          selectizeInput(ns('all_resolution'), label='Clustering resolution',
                         choices=NULL,
                         selected=NULL,
                         multiple=TRUE)
      ), # div
      div(id=ns('comparison_menu'),
          selectizeInput(ns('all_comparison'), label='Comparison',
                         choices=NULL,
                         selected=NULL,
                         multiple=TRUE)
      ), # div
      div(id=ns('group_menu'),
          selectizeInput(ns('all_group'), label='Group',
                         choices=NULL,
                         selected=NULL,
                         multiple=TRUE)
      ) # div
    )
  } else if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(6,
          tags$label(class='control-label',
                     'Filters'
          ) # tags$label
        ), # column
        column(6, align='right',
          helpButtonUI(ns('filters_help'))
        ) # column
      ), # fluidRow

      fluidRow(
        column(6, 'Max adjusted p-value'),
        column(6,
          numericInput(ns('max_padj'), label=NULL,
                       value=0.1)
        ) # column
      ), # fluidRow

      fluidRow(
        column(6, 'Min log2 fold-change'),
        column(6,
          numericInput(ns('min_lfc'), label=NULL,
                       value=0)
        ) # column
      ), # fluidRow

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "allmarkers"'),
        markerTableGeneralUI(ns('allmarkers'), panel='sidebar',
                             type='allmarkers', label='Cluster Markers')
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "consmarkers"'),
        markerTableGeneralUI(ns('consmarkers'), panel='sidebar',
                             type='consmarkers', label='Conserved Markers')
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "demarkers"'),
        markerTableGeneralUI(ns('demarkers'), panel='sidebar',
                             type='demarkers', label='DE Markers')
      )

    ) # tagList
  } else if(panel == 'selection'){
    tagList(
      fluidRow(
        column(12, strong('Selection options'))
      ), # fluidRow
      br(),

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "allmarkers"'),
        markerTableGeneralUI(ns('allmarkers'), panel='selection',
                             type='allmarkers', label='Cluster Markers')
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "consmarkers"'),
        markerTableGeneralUI(ns('consmarkers'), panel='selection',
                             type='consmarkers', label='Conserved Markers')
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('tbl_type'), '"] == "demarkers"'),
        markerTableGeneralUI(ns('demarkers'), panel='selection',
                             type='demarkers', label='DE Markers')
      ), # conditionalPanel

    ) # tagList

  } else if(panel == 'main'){
    tagList(
      tabsetPanel(type='tabs', id=ns('tbl_type'),

        markerTableGeneralUI(ns('allmarkers'), panel='main',
                             type='allmarkers',
                             label='Cluster Markers'),

        markerTableGeneralUI(ns('consmarkers'), panel='main',
                             type='consmarkers',
                             label='Conserved Markers'),

        markerTableGeneralUI(ns('demarkers'), panel='main',
                             type='demarkers',
                             label='DE Markers'),

      ) # tabsetPanel

    ) # tagList
  }
} # markerTableUI

#' Marker table module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param genes_to_plot reactive with list of genes to be plotted/selected
#' @param reset_genes reactive to trigger gene selection reset
#' @param reload_global reactive to trigger reload
#' @param config reactive list with config settings
#'
#' @export
#'
markerTableServer <- function(id, obj,
                              genes_to_plot, reset_genes,
                              reload_global, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        list(
          rds=obj$rds,
          allmarkers=obj$allmarkers,
          consmarkers=obj$consmarkers,
          demarkers=obj$demarkers
        )
      })

      # reactive to keep clicked genes
      clicked_genes <- reactiveValues(allmarkers=NULL,
                                      consmarkers=NULL,
                                      demarkers=NULL)

      #################### Filters ####################

      # keep track of resolutions
      all_resolution <- reactiveValues(allmarkers=NULL,
                                       consmarkers=NULL,
                                       demarkers=NULL)

      # keep track of assays
      all_assay <- reactiveValues(allmarkers=NULL,
                                  consmarkers=NULL,
                                  demarkers=NULL)
      # keep track of comparison
      all_comparison <- reactiveValues(allmarkers=NULL,
                                       consmarkers=NULL,
                                       demarkers=NULL)

      # keep track of groups
      all_group <- reactiveValues(allmarkers=NULL,
                                  consmarkers=NULL,
                                  demarkers=NULL)

      # consolidate resolutions
      observeEvent(c(all_resolution$allmarkers,
                     all_resolution$consmarkers,
                     all_resolution$demarkers), {
        res <- unique(c(all_resolution$allmarkers,
                        all_resolution$consmarkers,
                        all_resolution$demarkers))

        if(!is.null(input$all_resolution)){
          selected <- intersect(input$all_resolution,
                                res)
        } else {
          selected <- res
        }
        updateSelectizeInput(session, 'all_resolution',
                             choices=res,
                             selected=selected)
      })

      # consolidate assays
      observeEvent(c(all_assay$allmarkers,
                     all_assay$consmarkers,
                     all_assay$demarkers), {
        res <- unique(c(all_assay$allmarkers,
                        all_assay$consmarkers,
                        all_assay$demarkers))

        if(!is.null(input$all_assay)){
          selected <- intersect(input$all_assay,
                                res)
        } else {
          selected <- res
        }
        updateSelectizeInput(session, 'all_assay',
                             choices=res,
                             selected=selected)
      })

      # consolidate comparisons
      observeEvent(c(all_comparison$allmarkers,
                     all_comparison$consmarkers,
                     all_comparison$demarkers), {
        res <- unique(c(all_comparison$allmarkers,
                        all_comparison$consmarkers,
                        all_comparison$demarkers))

        if(!is.null(input$all_comparison)){
          selected <- intersect(input$all_comparison,
                                res)
        } else {
          selected <- res
        }
        updateSelectizeInput(session, 'all_comparison',
                             choices=res,
                             selected=selected)
      })

      # consolidate groups
      observeEvent(c(all_group$allmarkers,
                     all_group$consmarkers,
                     all_group$demarkers), {
        res <- unique(c(all_group$allmarkers,
                        all_group$consmarkers,
                        all_group$demarkers))

        if(!is.null(input$all_group)){
          selected <- intersect(input$all_group,
                                res)
        } else {
          selected <- res
        }
        updateSelectizeInput(session, 'all_group',
                             choices=res,
                             selected=selected)
      })

      #################### Global args ####################

      # reactives with global args that are passed to each marker module
      global_args <- reactive({
        list(
          resolution=input$all_resolution,
          assay=input$all_assay,
          comparison=input$all_comparison,
          group=input$all_group
        )
      })

      marker_args <- reactive({
        list(
          max_padj=input$max_padj,
          min_lfc=input$min_lfc
        )
      })

      #################### Cluster markers module ####################

      allmarker_data <- markerTableGeneralServer('allmarkers',
                                                 reactive({ list(markers=app_object()$allmarkers) }),
                                                 type='allmarkers',
                                                 genes_to_plot,
                                                 reset_genes,
                                                 global_args,
                                                 marker_args,
                                                 reload_global,
                                                 config)

      observeEvent(allmarker_data(), {
        validate(
          need(!is.null(app_object()$rds), '')
        )
        ll <- allmarker_data()
        all_resolution$allmarkers <- ll$filters$resolution
        all_assay$allmarkers <- ll$filters$assay
        all_comparison$allmarkers <- ll$filters$comparison
        all_group$allmarkers <- ll$filters$group
        clicked_genes$allmarkers <- ll$g
      })

      observeEvent(c(all_resolution$allmarkers, all_assay$allmarkers,
                     all_comparison$allmarkers, all_group$allmarkers), {
        showNotification(
          'Loaded markers table ...'
        )
      })

      #################### Conserved markers module ####################

      consmarker_data <- markerTableGeneralServer('consmarkers',
                                                  reactive({ list(markers=app_object()$consmarkers) }),
                                                  type='consmarkers',
                                                  genes_to_plot,
                                                  reset_genes,
                                                  global_args,
                                                  marker_args,
                                                  reload_global,
                                                  config)

      observeEvent(consmarker_data(), {
        validate(
          need(!is.null(app_object()$rds), '')
        )
        ll <- consmarker_data()
        all_resolution$consmarkers <- ll$filters$resolution
        all_assay$consmarkers <- ll$filters$assay
        all_comparison$consmarkers <- ll$filters$comparison
        all_group$consmarkers <- ll$filters$group
        clicked_genes$consmarkers <- ll$g
      })

      observeEvent(c(all_resolution$consmarkers, all_assay$consmarkers,
                     all_comparison$consmarkers, all_group$consmarkers), {
        showNotification(
          'Loaded conserved markers table ...'
        )
      })

      #################### DE markers module ####################

      demarker_data <- markerTableGeneralServer('demarkers',
                                                reactive({ list(markers=app_object()$demarkers) }),
                                                type='demarkers',
                                                genes_to_plot,
                                                reset_genes,
                                                global_args,
                                                marker_args,
                                                reload_global,
                                                config)

      observeEvent(demarker_data(), {
        validate(
          need(!is.null(app_object()$rds), '')
        )
        ll <- demarker_data()
        all_resolution$demarkers <- ll$filters$resolution
        all_assay$demarkers <- ll$filters$assay
        all_comparison$demarkers <- ll$filters$comparison
        all_group$demarkers <- ll$filters$group
        clicked_genes$demarkers <- ll$g
      })

      observeEvent(c(all_resolution$demarkers, all_assay$demarkers,
                     all_comparison$demarkers, all_group$demarkers), {
        showNotification(
          'Loaded DE markers table ...'
        )
      })

      ################## Load data ####################

      observeEvent(app_object(), {
        all_types <- c('allmarkers',
                       'consmarkers',
                       'demarkers')

        tbl_select <- FALSE

        # flags used to show filters in global settings
        global_selected <- list(assay=0, resolution=0,
                                comparison=0, group=0)

        for(type in all_types){
          # reset reactive values
          clicked_genes[[ type ]] <- NULL

          all_resolution[[ type ]] <- NULL
          all_assay[[ type ]] <- NULL
          all_comparison[[ type ]] <- NULL
          all_group[[ type ]] <- NULL

          df <- app_object()[[ type ]]
          if(!is.null(df)){

            if(!tbl_select){
              tbl_select <- TRUE
              showTab(inputId = 'tbl_type', target=type,
                      select=TRUE)
            } else {
              showTab(inputId = 'tbl_type', target=type)
            }

            for(col in names(global_selected)){
              if(col %in% colnames(df)){
                global_selected[[ col ]] <- global_selected[[ col ]] + 1
              }
            }
          } else {
              hideTab(inputId = 'tbl_type', target=type)
          }

        }

        # show global filter if filter column is present in more than one table
        for(col in names(global_selected)){
          if(global_selected[[ col ]] >= 2){
            shinyjs::show(id=paste0(col, '_menu'))
          } else {
            shinyjs::hide(id=paste0(col, '_menu'))
          }
        }
      })

      #################### gene scratchpad ####################

      all_clicked_genes <- reactive({
        unique(c(clicked_genes$allmarkers,
                 clicked_genes$consmarkers,
                 clicked_genes$demarkers))
      })

      observeEvent(reset_genes(), {
        clicked_genes$allmarkers <- NULL
        clicked_genes$consmarkers <- NULL
        clicked_genes$demarkers <- NULL
      })

      helpButtonServer('filters_help', size='l')

      return(
        reactive({
          list(
            g=all_clicked_genes()
          )
        })
      )
    } # function
  ) # moduleServer
} # markerTableServer
