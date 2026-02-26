#' Feature plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
featurePlotUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(col1, strong('Split by')),
        column(col2,
          selectInput(ns('split_by'),
                      label=NULL,
                      choices=NULL,
                      selected=NULL)
        ) # column
      ), # fluidRow

      bsCollapse(
        bsCollapsePanel(span(icon('gear'), 'Edit split levels'),
                        value='edit split',
          controlUI(ns('plt_split_lvls'), label=NULL)
        ) # bsCollapsePanel
      ), # bsCollapse

      fluidRow(
        column(col1, 'Free axes?'),
        column(col2,
          selectInput(ns('free_axes'),
                      label=NULL,
                      choices=c('no', 'yes'))
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
  } else if(panel == 'selection'){
    tagList(
      fluidRow(
        column(6,
          strong('Point selection')
        ), # column
        column(6, align='right',
          helpButtonUI(ns('umap_ptselect_help'))
        ) # column
      ), # fluidRow

      uiOutput(ns('pt_selected')),

      fluidRow(
        column(12,
          align='center',
          style='margin-bottom: 10px;',
          actionButton(ns('show_selection'),
                       label='Show selection')
        ),
        column(12,
          align='center',
          style='margin-bottom: 10px;',
          downloadButton(ns('dload_clicks'),
                         label='Download selection')
        ),
        column(12,
          align='center',
          style='margin-bottom: 10px;',
          actionButton(ns('reset_clicks'),
                       label='Reset selection',
                       class='btn-primary')
        )
      ) # fluidRow
    )
  } else if(panel == 'main'){
    tabPanel('UMAP',
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
          helpButtonUI(ns('featureplt_help'))
        ) #column
      ), # fluidRow
      div(align='center',
        withSpinner(
          plotlyOutput(ns('featureplt'),
                       width='auto',
                       height='auto')
        ) # withSpinner
      ) # div
    ) # tabPanel
  }
} # featurePlotUI


#' Feature plot module server
#'
#' @param id Input id
#' @param app_object Cascade app object
#' @param filtered barcodes to filter object
#' @param genes_to_plot reactive list with genes in scratchpad
#' @param args reactive list with elements: 'assay' for selected assay,
#'        'dimred' for which dimension reduction to use and
#'        'grp_by' for grouping variable
#' @param gene_choices reactive list with all genes present in object
#' @param all_selected reactive containing list of selected points
#' @param show_selection reactive to show selection
#' @param reset_selection reactive to reset selection
#' @param reload_global reactive to trigger reload
#' @param refresh reactive to trigger plot refresh from sidebar button
#' @param config reactive list with config settings
#'
#' @export
#'
featurePlotServer <- function(id, app_object, filtered, genes_to_plot,
                              args, gene_choices,
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

        grouping_vars <- app_object()$grouping_vars

        updateSelectInput(session, 'split_by',
                          choices=c('none', grouping_vars))

      })

      #################### edit split levels ####################

      plt_split_lvls <- controlServer('plt_split_lvls',
                                   reactive({ app_object()$metadata_levels }),
                                   reactive({ req(input$split_by); input$split_by }),
                                   reactive({ NULL }))

      #################### Main plotting function ####################

      get_feature_plot <- eventReactive(c(app_object()$rds,
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

        if(input$split_by == 'none') split_var <- NULL
        else split_var <- input$split_by

        # get genes to plot
        g <- input$plt_genes

        # feature plot config
        plt_args <- config()$server$plots$ftrplt

        max_genes <- plt_args$max_genes
        if(length(g) > max_genes){
          showNotification(
            paste0('Feature plot supports upto ', max_genes,
                   ' genes at a time. Using first ', max_genes),
            type='warning'
          )
          g <- g[1:max_genes]
        }

        # adjust plot height based on number of genes
        ht <- plt_args$base_ht

        df <- get_marker_plot_data(g, app_object, filtered(), args, reduction=TRUE)

        if(!is.null(split_var)){
          validate(
            need(length(plt_split_lvls()) > 0,
                 'Need at least one split level to plot!')
          )

          # only keep selected split var levels
          keep_idx <- df[, split_var] %in% plt_split_lvls()

          validate(
            need(sum(keep_idx) > 0,
                 'No cells left after filtering levels!')
          )
          df <- df[keep_idx, ]

          df[, split_var] <- factor(df[, split_var],
                                    levels=plt_split_lvls())
          num_split <- length(levels(df[[ split_var ]]))
        }

        # add check for empty marker size
        marker_size <- input$marker_size
        if(is.na(marker_size)) marker_size <- 2

        alpha <- input$marker_opacity
        free_axes <- ifelse(input$free_axes == 'yes', TRUE, FALSE)

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

        if(length(g) > 1){
          row_view <- 'single'
        } else {
          row_view <- 'auto'
        }

        # get color range & set floor
        crange <- c(min(df[, g]), max(df[, g]))

        # save column names
        df_cols <- colnames(df)

        # downsample 0 expression rows
        if(length(g) > 1) zero_rows <- rowSums(df[,g] > crange[1]) == 0
        else zero_rows <- df[,g] == crange[1]
        if(sum(zero_rows) > 50000){
          if(input$downsample == 'yes'){
            showNotification(
              'Number of empty cells very large! Downsampling to 50000',
              type='warning'
            )
            idx <- c(which(!zero_rows), sample(which(zero_rows), 50000))
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

        ht <- ht*input$scale

        lvls <- plt_split_lvls()

        source <- 'featureplot'

        if(!is.null(split_var)) num_traces <- num_split
        else num_traces <- 1

        # save plotted data
        plot_obj$df <- list(data=df,
                            xcol=df_cols[1],
                            ycol=df_cols[2],
                            color=g,
                            colors=colors,
                            split=split_var,
                            crange=crange,
                            marker_size=marker_size + 0.25*marker_size, # slightly increase marker size
                            alpha=alpha,
                            free_axes=free_axes,
                            source=source,
                            num_traces=num_traces)

        # arrange multi-gene view into rows
        if(length(g) > 1){
          if(!is.null(split_var)){
            if(length(lvls) > 5){
              showNotification(
                paste0('Warning: Split variable has more than five levels. For best results, ',
                       'plot a single gene when splitting the plot into many subplots'),
                type='warning', duration=10
              )
            }
          }

          # get list of plotly handles
          plist <- lapply(1:length(g), function(x){
                     if(x == 1) showscale <- TRUE
                     else showscale <- FALSE

                     p <- feature_ly(df,
                                     xcol=df_cols[1],
                                     ycol=df_cols[2],
                                     color=g[x],
                                     colors=colors,
                                     crange=crange,
                                     row_view=row_view,
                                     showscale=showscale,
                                     reversescale=reversescale,
                                     marker_size=marker_size,
                                     alpha=alpha,
                                     reorder=FALSE,
                                     split=split_var,
                                     free_axes=free_axes,
                                     height=0.75*ht*length(g),
                                     source=source)
                     p
                   })

          p <- subplot(plist, nrows=length(g))
        } else {
          if(!is.null(split_var) & length(lvls) <= 2){
              ht <- 0.75*ht
          }

          p <- feature_ly(df,
                          xcol=df_cols[1],
                          ycol=df_cols[2],
                          color=g,
                          colors=colors,
                          crange=crange,
                          row_view=row_view,
                          showscale=TRUE,
                          reversescale=reversescale,
                          marker_size=marker_size,
                          alpha=alpha,
                          reorder=FALSE,
                          split=split_var,
                          free_axes=free_axes,
                          height=ht,
                          margin=0.05,
                          source=source)
        }

        event_register(p, 'plotly_selected')

        p
      })

      output$featureplt <- renderPlotly({
          get_feature_plot()
        }
      )

      ##################### lasso selection ###########################

      # proxy for plot
      plotProxy <- plotlyProxy('featureplt', session)

      observeEvent(show_selection(), {

        isolate({
          flag <- is.null(app_object()$rds)
          split_var <- input$split_by
        })

        validate(
          need(!flag, '')
        )

        sel_pts <- unique(unlist(all_selected()))

        validate(
          need(length(sel_pts) > 0, '')
        )

        # Check if plotting multiple genes or split view
        if(length(plot_obj$df$color) > 1){
          showNotification(
            'Cannot show selection in multi-gene view',
            type='warning'
          )
          return()
        }

        # Build vectors for marker styling
        is_selected <- plot_obj$df$data$rn %in% sel_pts

        if(!plot_labeled()){
          marker_opacity <- rep(plot_obj$df$alpha * 0.25, nrow(plot_obj$df$data))
          marker_opacity[which(is_selected)] <- 1
        } else {
          marker_opacity <- rep(plot_obj$df$alpha * 1.75, nrow(plot_obj$df$data))
        }

        # For continuous color feature plots (single gene, no split)
        if(split_var == 'none'){
          # Single plot view - one trace for continuous color

          restyle_args <- list(
            'marker.opacity' = I(list(marker_opacity))
          )

          trace_idx <- list(0)

        } else {
          # Split plot view - one trace per split level
          if(is.factor(plot_obj$df$data[[split_var]])) {
            split_levels <- levels(plot_obj$df$data[[split_var]])
          } else {
            split_levels <- unique(plot_obj$df$data[[split_var]])
          }

          # Split attribute vectors by split variable only
          opacity_list <- split(marker_opacity, f=plot_obj$df$data[[ split_var ]])

          restyle_args <- list(
            'marker.opacity' = I(unname(opacity_list))
          )

          num_traces <- length(split_levels)
          trace_idx <- as.list(seq_len(num_traces) - 1)
        }

        plotProxy %>%
          plotlyProxyInvoke('restyle', restyle_args, trace_idx)

        current <- plot_labeled()
        plot_labeled(!current)

        ## OLD APPROACH USING addTraces/deleteTraces
        # if(split_var == 'none'){
        #   if(length(sel_pts) > 0){
        #     new_trace <- get_label_trace(plot_obj$df,
        #                                  sel_pts)
        #     num_traces <- plot_obj$df$num_traces
        #
        #     # remove last trace
        #     # NOTE: this is 0-based indexed
        #     if(plot_labeled()){
        #       if(length(plot_obj$df$color) == 1){
        #         plotProxy %>%
        #           plotlyProxyInvoke('deleteTraces', num_traces)
        #       }
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
        data_df <- plot_obj$df$data
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

      # dynamic UI to show number of points selected
      output$pt_selected <- renderUI({
        np <- length(unique(unlist(selected_points$full)))

        tagList(
          fluidRow(
            column(12, style='margin-bottom: 10px;',

              paste(np, 'points selected')
            )
          )
        )
      })

      # download handler for selected cells
      output$dload_clicks <- downloadHandler(
        filename = function(){
          paste0('clicked-points.tsv')
        },
        content = function(file){
          bc <- unique(unlist(selected_points$full))

          # only output unique barcodes
          mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=T)
          idx <- mdata$rn %in% bc

          mdata_sel <- as.data.frame(mdata[idx,])
          rn_idx <- which(colnames(mdata_sel) == 'rn')
          colnames(mdata_sel)[rn_idx] <- 'barcodes'

          write.table(mdata_sel, file=file, sep='\t', quote=FALSE,
                      row.names=FALSE)
        }
      )

      # observer to reset clicks
      observeEvent(input$reset_clicks, {
        np <- length(unique(unlist(selected_points$full)))
        showNotification(
            paste0('Clearing ', np,
                   ' points from selection')
        )
      # observer to reset clicks and hide selection
      observeEvent(reset_selection(), {
        selected_points$full <- list()
      })

      helpButtonServer('featureplt_help', size='l')
      helpButtonServer('umap_ptselect_help', size='l')
      downloadPlotServer('plt_dload', get_feature_plot, 'feature_plot')

      # return selected points
      return(
        reactive({ selected_points$full })
      )
    } # function
  ) # moduleServer
} # featurePlotServer
