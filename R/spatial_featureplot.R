#' Spatial feature plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
spatialFeaturePlotUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(col1, 'Free axes?'),
        column(col2,
          selectInput(ns('free_axes'),
                      label=NULL,
                      choices=c('no', 'yes'),
                      selected='yes')
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Color map'),
        column(col2,
          selectInput(ns('colormap'),
                      label=NULL,
                      choices=c('blues', 'yellow-green-blue', 'viridis'))
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Marker size'),
        column(col2,
          numericInput(ns('marker_size'),
                       label=NULL,
                       value=3, min=1, step=1)
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Opacity'),
        column(col2,
          sliderInput(ns('marker_opacity'),
                      label=NULL,
                      value=0.5, step=0.1,
                      min=0, max=1, ticks=FALSE)
        ) # column
      ), # fluidRow

      fluidRow(
        column(6, 'Scale plot'),
        column(6,
          sliderInput(ns('scale'),
                      label=NULL,
                      value=1.0, step=0.1,
                      min=0.5, max=2, ticks=FALSE)
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Downsample empty cells'),
        column(col2,
          selectInput(ns('downsample'),
                      label=NULL,
                      choices=c('yes', 'no'))
        ) # column
      ) # fluidRow
    )
  } else if(panel == 'main'){
    tabPanel('Spatial',
      br(),
      fluidRow(
        column(2, 'Select gene to plot', style='font-size: 16px;', align='center'),
        column(3,
          selectizeInput(ns('plt_genes'),
                       label=NULL,
                       choices=NULL, selected=NULL, multiple=TRUE)
        ),
        column(4, align='left',
          actionButton(ns('plt_do'), 'Generate plot',
                       class='btn-primary')
        ),
        column(2, align='right',
          downloadPlotUI(ns('plt_dload'))
        ), # column
        column(1,
          align='left',
          helpButtonUI(ns('spatial_featureplt_help'))
        ) #column
      ), # fluidRow
      div(align='center',
        withSpinner(
          plotlyOutput(ns('spatial_featureplt'),
                       width='auto',
                       height='auto')
        ) # withSpinner
      ) # div
    ) # tabPanel
  }
} # spatialFeaturePlotUI


#' Spatial feature plot module server
#'
#' @param id Input id
#' @param app_object Cascade app object
#' @param filtered barcodes to filter object
#' @param genes_to_plot reactive list with genes in scratchpad
#' @param args reactive list with elements: 'assay' for selected assay,
#'        'dimred' for which dimension reduction to use and
#'        'grp_by' for grouping variable
#' @param gene_choices reactive list with all genes present in object
#' @param slice reactive with slices to be used for plotting
#' @param reload_global reactive to trigger reload
#' @param refresh reactive to trigger plot refresh from sidebar button
#' @param config reactive list with config settings
#'
#' @export
#'
spatialFeaturePlotServer <- function(id, app_object, filtered, genes_to_plot,
                                     args, gene_choices, slice,
                                     reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      observeEvent(gene_choices(), {
        updateSelectizeInput(session, 'plt_genes',
                             choices=gene_choices(),
                             selected='',
                             server=TRUE)
      })

      # if gene scratchpad has genes, show those at the top of
      # dropdown choices
      observeEvent(genes_to_plot(), {
        g <- genes_to_plot()

        if(any(g != '')){
          choices <- c(g, setdiff(gene_choices(), g))

          ## NOTE: default returned value for selectizeInput with *multiple=TRUE*
          ##       is NULL, not ''
          if(!is.null(input$plt_genes)) selected <- input$plt_genes
          else selected <- ''
          updateSelectizeInput(session, 'plt_genes',
                               choices=choices,
                               selected=selected,
                               server=TRUE)
        }
      })

      #################### Main plotting function ####################

      get_spatial_feature_plot <- eventReactive(c(app_object()$rds,
                                                  filtered(),
                                                  input$plt_do,
                                                  refresh()), {
        validate(
          need(!is.null(input$plt_genes),
               paste0(
                 '\nNo marker genes selected!\n\n',
                 'Please select markers above and ',
                 'then click the button to visualize here')
               )
        )

        # get genes to plot
        g <- input$plt_genes

        max_genes <- config()$server$plots$spatial_featureplt$max_genes
        if(length(g) > max_genes){
          showNotification(
            paste0('Feature plot supports upto ', max_genes,
                   ' genes at a time. Using first ', max_genes),
            type='warning'
          )
          g <- g[1:max_genes]
        }

        obj_type <- app_object()$obj_type

        if(obj_type == 'seurat'){
          validate(
            need(any(grepl('Spatial', names(app_object()$rds@assays))) |
                 any(grepl('Xenium', names(app_object()$rds))),
                 'Spatial analysis not available')
          )
        } else if(obj_type == 'anndata'){
          validate(
            need('spatial' %in% names(app_object()$rds$obsm),
                 'Spatial analysis not available')
          )
        }

        # make sure to only use slices that are present
        curr_slices <- slice()

        df <- get_marker_plot_data(g, app_object, filtered(), args, slice=slice())

        # get color range & set floor
        gdat <- df[,(ncol(df) - length(g) + 1):ncol(df)]
        crange <- c(min(gdat), max(gdat))

        # downsample 0 expression rows
        if(length(g) > 1) zero_rows <- rowSums(gdat > crange[1]) == 0
        else zero_rows <- gdat == crange[1]
        if(sum(zero_rows) > 50000){
          if(input$downsample == 'yes'){
            showNotification(
              'Number of empty cells very large! Downsampling to 50000',
              type='warning'
            )
            idx <- c(which(!zero_rows), sample(which(zero_rows), 50000))
            df <- df[idx,]
          } else {
            showNotification(
              'Number of empty cells very large! Consider downsampling for faster plotting',
              type='warning'
            )
          }
        }

        # add check for empty marker_size
        if(is.na(input$marker_size)) marker_size <- 3
        else marker_size <- input$marker_size

        alpha <- input$marker_opacity

        # rename (eventual) axis labels
        colnames(df)[colnames(df) == 'imagecol'] <- 'spatial1'
        colnames(df)[colnames(df) == 'imagerow'] <- 'spatial2'

        if(length(g) > 1){
          row_view <- 'single'
        } else {
          row_view <- 'auto'
        }

        # colormaps
        reversescale <- FALSE
        if(input$colormap == 'blues'){
          colors <- 'Blues'
          reversescale <- TRUE
        } else if(input$colormap == 'yellow-green-blue'){
          colors <- 'YlGnBu'
        } else if(input$colormap == 'viridis'){
          colors <- 'Viridis'
        }

        ht <- config()$server$plots$spatial_featureplt$base_ht*input$scale

        if(length(curr_slices) > 1){
          split_var <- 'slice'
          df[[ split_var ]] <- factor(df[[ split_var ]], levels=curr_slices)
        } else {
          split_var <- NULL
        }

        free_axes <- ifelse(input$free_axes == 'yes', TRUE, FALSE)

        # arrange multi-gene views into row
        if(length(g) > 1){

          if(!is.null(split_var)){
            if(length(curr_slices) > 5){
              showNotification(
                paste0('Warning: More than five slices selected. For best results, ',
                       'plot a single gene when viewing many slices'),
                type='warning', duration=10
              )
            }
          }

          # get list of plotly handles
          plist <- lapply(1:length(g), function(x){
                     if(x == 1) showscale <- TRUE
                     else showscale <- FALSE

                     if(length(curr_slices) <= 2){
                       wd <- 0.9*ht*length(curr_slices)
                       ht <- 0.7*ht*length(g)
                     } else {
                       wd <- NULL
                       ht <- 0.3*ht*length(g)
                     }

                     p <- feature_ly(df,
                                     xcol='spatial1',
                                     ycol='spatial2',
                                     color=g[x],
                                     colors=colors,
                                     crange=crange,
                                     row_view=row_view,
                                     showscale=showscale,
                                     reversescale=reversescale,
                                     showticklabels=FALSE,
                                     marker_size=marker_size,
                                     alpha=alpha,
                                     split=split_var,
                                     free_axes=free_axes,
                                     width=wd,
                                     height=ht)
                     p
                   })

          # arrange multi-gene view into multiple row
          p <- subplot(plist, nrows=length(g))
        } else {
          if(length(curr_slices) == 1){
            wd <- ht
            ht <- 0.75*ht
          } else if(length(curr_slices) == 2){
            wd <- 0.75*ht*length(curr_slices)
            ht <- 0.5*ht
          } else {
            wd <- 1.25*ht
            ht <- ht
          }

          p <- feature_ly(df,
                          xcol='spatial1',
                          ycol='spatial2',
                          color=g,
                          colors=colors,
                          crange=crange,
                          row_view=row_view,
                          showscale=TRUE,
                          reversescale=reversescale,
                          showticklabels=FALSE,
                          marker_size=marker_size,
                          alpha=alpha,
                          split=split_var,
                          free_axes=free_axes,
                          width=wd,
                          height=ht,
                          margin=0.05)

        }

        p
      })

      output$spatial_featureplt <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        p <- get_spatial_feature_plot()

        p
      })

      helpButtonServer('spatial_featureplt_help', size='l')
      downloadPlotServer('plt_dload', get_spatial_feature_plot, 'spatial_feature_plot')

    } # function
  ) # moduleServer
} # spatialFeaturePlotServer
