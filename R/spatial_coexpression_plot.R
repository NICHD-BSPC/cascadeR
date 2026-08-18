#' Spatial coexpression plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
spatialCoexpressionPlotUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      strong('Expression thresholds (%)'),

      fluidRow(
        column(2, style='margin-top: 10px;',
          textOutput(ns('thres_1_title'))),
        column(10,
          sliderInput(ns('thres_1'),
                      label=NULL,
                      min=0, max=100,
                      step=5, value=50)
        ) # column
      ), # fluidRow

      fluidRow(
        column(2, style='margin-top: 10px;',
          textOutput(ns('thres_2_title'))),
        column(10,
          sliderInput(ns('thres_2'),
                      label=NULL,
                      min=0, max=100,
                      step=5, value=50)
        ) # column
      ), # fluidRow

      div(align='center',
        plotlyOutput(ns('legend'),
                     width='75%',
                     height='200px')
      ), # div

      strong('Other options', style='margin-top: 10px;'),

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
                      choices=c('red-blue', 'red-green'))
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
    tabPanel('Spatial',
      br(),
      fluidRow(
        column(2, 'Select genes to plot',
               style='font-size: 16px;', align='center'),
        column(3,
          selectizeInput(ns('plt_genes'),
                       label=NULL,
                       choices=NULL, selected=NULL,
                       multiple=TRUE)
        ),
        column(6, align='left',
          actionButton(ns('plt_do'), 'Generate plot',
                       class='btn-primary')
        ),
        column(1,
          align='left',
          helpButtonUI(ns('spatial_coexplt_help'))
        ) #column
      ), # fluidRow
      div(align='center',
        fluidRow(
          column(9,
            fluidRow(
              column(12, align='left',
                downloadPlotUI(ns('plt_dload'))
              ) # column
            ), # fluidRow
            fluidRow(
              withSpinner(
                plotlyOutput(ns('spatial_coexplt'),
                             width='800px',
                             height='700px')
              ) # withSpinner
            )
          ),

          column(3, align='center',
            style='margin-top: 200px;',
            DTOutput(ns('spatial_coexp_tbl')),
            br(),
            uiOutput(ns('spatial_coexp_thres'))
          )
        ) # fluidRow
      ) # div
    ) # tabPanel
  }
} # spatialCoexpressionPlotUI


#' Spatial coexpression plot module server
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
spatialCoexpressionPlotServer <- function(id, app_object, filtered, genes_to_plot,
                                          args, gene_choices, slice,
                                          all_selected, show_selection, reset_selection,
                                          reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      plot_data <- reactiveValues(spat_coexp_tbl=NULL)

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

      get_spatial_coexp_plot <- eventReactive(c(app_object()$rds,
                                               filtered(),
                                               input$plt_do,
                                               refresh()), {
        g <- input$plt_genes
        validate(
          need(length(g) >= 2,
               paste0(
                 '\nFewer than 2 genes selected!\n\n',
                 'Please select markers above and ',
                 'then click the button to visualize here')
               )
        )

        if(length(g) > 2){
          showNotification(
            'More that 2 genes selected, using first two ...'
          )
          g <- g[1:2]
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

        df <- get_marker_plot_data(g, app_object, filtered(), args, slice=slice())

        # get color range & set floor
        gdat <- df[,(ncol(df) - length(g) + 1):ncol(df)]
        crange <- c(min(gdat), max(gdat))

        # downsample 0 expression rows
        if(length(g) > 1) zero_rows <- rowSums(gdat > crange[1]) == 0
        else zero_rows <- gdat == crange[1]

        # downsample to these many cells
        downsample_target <- config()$server$downsample_target
        if(sum(zero_rows) > downsample_target){
          if(input$downsample == 'yes'){
            showNotification(
              paste('Number of empty cells very large! Downsampling to',
              downsample_target),
              type='warning'
            )
            # find sample indices, then reorder to maintain original order
            idx <- c(which(!zero_rows), sample(which(zero_rows), downsample_target))
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

        if(length(slice()) > 1){
          split_var <- 'slice'
          df[[ split_var ]] <- factor(df[[ split_var ]], levels=slice())
          num_split <- length(slice())
        } else {
          split_var <- NULL
        }

        colors <- get_coexplt_colors(input$colormap)

        ht <- config()$server$plots$spatial_coexplt$base_ht*input$scale
        wd <- 1.25*ht
        if(length(slice()) == 2){
          ht <- 0.6*ht
        }

        free_axes <- ifelse(input$free_axes == 'yes', TRUE, FALSE)

        source <- 'spatial_coexpression_plot'

        pp <- feature_blend(df,
                            xcol='spatial1',
                            ycol='spatial2',
                            blend_cols=g,
                            colors=colors[2:4],
                            split=split_var,
                            col_threshold_1=input$thres_1/100,
                            col_threshold_2=input$thres_2/100,
                            neutral_color=colors[1],
                            #bin_mode='quantile',
                            showlegend=TRUE,
                            showticklabels=FALSE,
                            type='scattergl',
                            marker_size=marker_size,
                            alpha=alpha,
                            free_axes=free_axes,
                            width=wd,
                            height=ht,
                            source=source)

        # get list elements
        p <- pp$plot
        df <- pp$data

        num_traces <- length(unique(df[[ 'color' ]]))
        if(!is.null(split_var)) num_traces <- num_traces*num_split

        # save plotted data
        plot_obj$df <- list(data=df,
                            xcol='spatial1',
                            ycol='spatial2',
                            color='color',
                            blend_cols=g,
                            colors=colors[2:4],
                            split=split_var,
                            col_threshold_1=input$thres_1/100,
                            col_threshold_2=input$thres_2/100,
                            neutral_color=colors[1],
                            marker_size=marker_size*1.25,
                            alpha=alpha,
                            source=source,
                            free_axes=free_axes,
                            num_traces=num_traces)
        plot_labeled(FALSE)

        # save trace names for split-view restyle alignment
        trace_data <- plotly::plotly_build(p)$x$data
        plot_obj$df$trace_names <- unlist(lapply(trace_data, function(x) unique(x$meta)))

        pct_df <- get_coexp_tbl(df, g,
                                threshold1=input$thres_1/100,
                                threshold2=input$thres_2/100)


        # save thresholds
        xlims <- get_limits(df[[ g[1] ]])
        ylims <- get_limits(df[[ g[2] ]])
        thres <- list(geneX=input$thres_1*(xlims[2] - xlims[1])/100,
                      geneY=input$thres_2*(ylims[2] - ylims[1])/100)
        names(thres) <- g
        plot_data$spat_coexp_thres <- thres

        # add alpha
        colors <- paste0(colors, alpha*100)
        names(colors) <- pct_df$labels

        plot_data$spat_coexp_tbl <- list(tbl=pct_df,
                                         colors=colors)

        event_register(p, 'plotly_selected')

        p
      })

      output$spatial_coexplt <- renderPlotly({
          get_spatial_coexp_plot()
      })

      output$spatial_coexp_tbl <- renderDT({
        validate(
          need(!is.null(plot_data$spat_coexp_tbl), '')
        )

        tbl <- plot_data$spat_coexp_tbl$tbl
        colors <- plot_data$spat_coexp_tbl$colors

        # - dom='t', only shows table
        # - formatSignif shows 4 significant digits for '%' column
        # - formatStyle colors the 1st column (labels) using the
        #   colors vector
        datatable(tbl,
                  rownames=FALSE, selection='none',
                  caption=tags$caption(style='font-weight: bold; font-size: 15px;',
                                       'Summary'),
                  options=list(dom='t')) %>%
        formatSignif(columns='%', digits=4) %>%
        formatStyle(1,
                    backgroundColor=styleEqual(names(colors), colors))

      })

      output$spatial_coexp_thres <- renderUI({
        validate(
          need(!is.null(plot_data$spat_coexp_thres), '')
        )

        # print thresholds
        thres_msg <- NULL
        for(g in names(plot_data$spat_coexp_thres)){
          tmp <- paste0(g, ' = ', plot_data$spat_coexp_thres[[ g ]])
          if(is.null(thres_msg)) thres_msg <- tmp
          else thres_msg <- paste0(thres_msg, '; ', tmp)
        }

        tagList(
          strong('Thresholds'),
          h5(thres_msg)
        )

      })

      output$legend <- renderPlotly({

        colors <- get_coexplt_colors(input$colormap)

        g <- input$plt_genes
        validate(
          need(length(g) >= 2, '')
        )
        g <- g[1:2]

        p1 <- get_coexp_legend(colors[2:4],
                               dimnames=g,
                               xline=input$thres_1/100,
                               yline=input$thres_2/100,
                               neutral_color=colors[1])

        p1
      })

      output$thres_1_title <- renderText({
        input$plt_genes[1]
      })

      output$thres_2_title <- renderText({
        input$plt_genes[2]
      })

      ##################### lasso selection ###########################

      # proxy for plot
      plotProxy <- plotlyProxy('spatial_coexplt', session)

      restyle_selection <- function(marker_opacity){
        if(is.null(plot_obj$df$split)){
          color_values <- plot_obj$df$data[[ plot_obj$df$color ]]
          if(is.factor(color_values)){
            color_levels <- levels(droplevels(color_values))
          } else {
            color_levels <- unique(color_values)
          }

          opacity_list <- split(marker_opacity, f=color_values, drop=TRUE)
          opacity_list <- opacity_list[as.character(color_levels)]
          opacity_list <- opacity_list[!vapply(opacity_list, is.null, logical(1))]

          trace_match <- match(plot_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        } else {
          split_var <- plot_obj$df$split

          # Split by color and slice to match Plotly trace groups.
          opacity_list <- split(marker_opacity,
                                f=list(plot_obj$df$data[[ plot_obj$df$color ]],
                                       plot_obj$df$data[[ split_var ]]),
                                drop=TRUE)

          trace_match <- match(plot_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        }

        if(length(opacity_list) == 0) return(invisible(NULL))

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

        if(!plot_labeled()){
          is_selected <- plot_obj$df$data$rn %in% sel_pts
          if(!any(is_selected)){
            showNotification(
              'No selected points found in current plot',
              type='warning'
            )
            return()
          }

          marker_opacity <- rep(plot_obj$df$alpha * 0.05, nrow(plot_obj$df$data))
          marker_opacity[which(is_selected)] <- 1
        } else {
          marker_opacity <- rep(plot_obj$df$alpha * 1.95, nrow(plot_obj$df$data))
        }

        restyle_selection(marker_opacity)
        plot_labeled(!plot_labeled())
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

      # observer to reset clicks
      observeEvent(reset_selection(), {
        if(plot_labeled() & !is.null(plot_obj$df)){
          marker_opacity <- rep(plot_obj$df$alpha * 1.95, nrow(plot_obj$df$data))
          restyle_selection(marker_opacity)
          plot_labeled(FALSE)
        }

        selected_points$full <- list()
      })

      helpButtonServer('spatial_coexplt_help', size='l')
      helpButtonServer('umap_ptselect_help', size='l')
      downloadPlotServer('plt_dload', get_spatial_coexp_plot, 'spatial_coexpression_plot')

      # return selected points
      return(
        reactive({ selected_points$full })
      )
    } # function
  ) # moduleServer
} # spatialCoexpressionPlotServer
