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
#' @param all_selected reactive containing list of selected points
#' @param show_selection reactive to show selection
#' @param reset_selection reactive to reset selection
#' @param reload_global reactive to trigger reload
#' @param refresh reactive to trigger plot refresh from sidebar button
#' @param config reactive list with config settings
#'
#' @export
#'
spatialFeaturePlotServer <- function(id, app_object, filtered, genes_to_plot,
                                     args, gene_choices, slice,
                                     all_selected, show_selection, reset_selection,
                                     reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      # keep track of point selection here
      selected_points <- reactiveValues(full=list(),
                                        current=list())

      plot_obj <- reactiveValues(df=NULL)
      plot_labeled <- reactiveVal(FALSE)

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

      observeEvent(app_object()$metadata_levels, {
        # reset data
        plot_obj$df <- NULL
        selected_points$full <- list()
        selected_points$current <- list()
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
            need(!is.null(app_object()$spatial_coords),
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
            idx <- idx[order(idx)]
            df <- data.table::as.data.table(df)
            df <- df[idx,]
            df <- as.data.frame(df)
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
          num_split <- length(curr_slices)
        } else {
          split_var <- NULL
        }

        free_axes <- ifelse(input$free_axes == 'yes', TRUE, FALSE)

        source <- 'spatial_featureplot'

        if(!is.null(split_var)) num_traces <- num_split
        else num_traces <- 1

        # Keep saved data order aligned with the trace order used for restyle.
        if(length(g) == 1) df <- df[order(df[[g]]),]

        # save plotted data
        plot_obj$df <- list(data=df,
                            xcol='spatial1',
                            ycol='spatial2',
                            color=g,
                            colors=colors,
                            split=split_var,
                            crange=crange,
                            marker_size=marker_size + 0.25*marker_size, # slightly increase marker size
                            alpha=alpha,
                            free_axes=free_axes,
                            source=source,
                            num_traces=num_traces)
        plot_labeled(FALSE)

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
                                     height=ht,
                                     source=source)
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
                          reorder=FALSE,
                          width=wd,
                          height=ht,
                          margin=0.05,
                          source=source)

        }

        event_register(p, 'plotly_selected')

        p
      })

      output$spatial_featureplt <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        p <- get_spatial_feature_plot()

        p
      })

      ##################### lasso selection ###########################

      # proxy for plot
      plotProxy <- plotlyProxy('spatial_featureplt', session)

      observeEvent(input$show_selection, {
        if(length(input$plt_genes) > 1){
          showNotification(
            'Cannot show selection in multi-gene view',
            type='warning'
          )
        }
      })

      # function to restyle current selection using marker opacity
      restyle_selection <- function(marker_opacity){
        split_var <- plot_obj$df$split

        if(is.null(split_var)){
          opacity_list <- list(marker_opacity)
          trace_idx <- list(0)
        } else {
          split_values <- plot_obj$df$data[[ split_var ]]
          if(is.factor(split_values)){
            split_levels <- levels(droplevels(split_values))
          } else {
            split_levels <- unique(split_values)
          }

          opacity_list <- split(marker_opacity, f=split_values, drop=TRUE)
          opacity_list <- opacity_list[as.character(split_levels)]
          opacity_list <- opacity_list[!vapply(opacity_list, is.null, logical(1))]
          trace_idx <- as.list(seq_along(opacity_list) - 1)
        }

        restyle_args <- list(
          'marker.opacity' = I(unname(opacity_list))
        )

        plotProxy %>%
          plotlyProxyInvoke('restyle', restyle_args, trace_idx)
      }

      observeEvent(show_selection(), {

        validate(
          need(!is.null(app_object()$rds), '')
        )
        validate(
          need(!is.null(plot_obj$df), '')
        )

        sel_pts <- unique(unlist(all_selected()))

        validate(
          need(length(sel_pts) > 0, '')
        )

        if(length(plot_obj$df$color) > 1){
          showNotification(
            'Cannot show selection in multi-gene view',
            type='warning'
          )
          return()
        }

        if(!plot_labeled()){
          is_selected <- plot_obj$df$data$rn %in% sel_pts
          marker_opacity <- rep(plot_obj$df$alpha * 0.05, nrow(plot_obj$df$data))
          marker_opacity[which(is_selected)] <- 1
        } else {
          marker_opacity <- rep(plot_obj$df$alpha * 1.95, nrow(plot_obj$df$data))
        }

        restyle_selection(marker_opacity)
        plot_labeled(!plot_labeled())

        ## OLD APPROACH USING addTraces/deleteTraces
        # if(length(slice()) == 1){
        #   if(length(sel_pts) > 0){
        #     new_trace <- get_label_trace(plot_obj$df,
        #                                  sel_pts)
        #     num_traces <- plot_obj$df$num_traces
        #
        #     # remove last trace
        #     # NOTE: this is 0-based indexed
        #     if(plot_labeled()){
        #       plotProxy %>%
        #         plotlyProxyInvoke('deleteTraces', num_traces)
        #     }
        #
        #     plotProxy %>%
        #       plotlyProxyInvoke('addTraces', new_trace)
        #
        #     plot_labeled(TRUE)
        #   } else if(plot_labeled()){
        #     num_traces <- plot_obj$df$num_traces
        #     plotProxy %>%
        #       plotlyProxyInvoke('deleteTraces', num_traces)
        #     plot_labeled(FALSE)
        #   }
        # }
      })

      get_selected <- reactive({
        validate(
          need(!is.null(plot_obj$df), '')
        )

        event_data('plotly_selected', source=plot_obj$df$source)
      })

      observeEvent(get_selected(), {
        validate(
          need(!is.null(app_object()$rds), '')
        )

        df <- get_selected()

        # get points by matching coords & key
        keys <- paste(df$x, df$y)

        # plot data
        data_df <- as.data.frame(plot_obj$df$data)
        xcol <- plot_obj$df$xcol
        ycol <- plot_obj$df$ycol

        # fix names if starting with number
        if(grepl('^\\d', xcol)) xcol <- make.names(xcol)
        if(grepl('^\\d', ycol)) ycol <- make.names(ycol)

        data_keys <- paste(data_df[, xcol],
                           data_df[, ycol])

        new <- unique(data_df$rn[which(data_keys %in% keys)])

        curr <- unique(unlist(all_selected()))

        # only add new points
        if(!all(new %in% curr)){
          new_idx <- which(!new %in% curr)
          showNotification(
              paste0('Adding ', length(new_idx), ' points to selection')
          )

          selected_points$full[[ length(selected_points$full) + 1 ]] <- new[new_idx]
        } else if(length(new) > 0){
          showNotification(
              paste0('All selected points already in selection'),
              type='warning'
          )
        }
      })

      # observer to reset clicks
      observeEvent(reset_selection(), {
        if(plot_labeled() & !is.null(plot_obj$df)){
          marker_opacity <- rep(plot_obj$df$alpha, nrow(plot_obj$df$data))
          restyle_selection(marker_opacity)
          plot_labeled(FALSE)
        }
        selected_points$full <- list()
      })

      helpButtonServer('spatial_featureplt_help', size='l')
      helpButtonServer('umap_ptselect_help', size='l')
      downloadPlotServer('plt_dload', get_spatial_feature_plot, 'spatial_feature_plot')

      # return selected points
      return(
        reactive({ selected_points$full })
      )
    } # function
  ) # moduleServer
} # spatialFeaturePlotServer
