#' Coexpression plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @return Shiny UI elements for the coexpression plot module
#'
#' @export
#'
coexpressionPlotUI <- function(id, panel){
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

      div(align='center', style='margin-bottom: 5px;',
        plotlyOutput(ns('legend'),
                     width='75%',
                     height='200px')
      ), # div

      #fluidRow(
      #  column(6, 'Bin mode'),
      #  column(6,
      #    selectInput(ns('bin'),
      #                label=NULL,
      #                choices=c('quantile', 'range'),
      #                selected='quantile')
      #  ) # column
      #), # fluidRow

      strong('Other options', style='margin-top: 10px;'),

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
        column(col1, 'Aspect ratio'),
        column(col2,
          selectInput(ns('plot_aspect'),
                      label=NULL,
                      choices=c('narrow', 'wide'))
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
        column(1, align='left',
          helpButtonUI(ns('coexplt_help'))
        )
      ), # fluidRow

      fluidRow(
        column(9,
          fluidRow(
            column(12, align='left',
              downloadPlotUI(ns('plt_dload'))
            ) # column
          ), # fluidRow
          fluidRow(
            div(align='center',
            withSpinner(
              plotlyOutput(ns('coexplt'),
                           height='700px')
            ) # withSpinner
            )
          )
        ),
        column(3, align='center',
          style='margin-top: 200px;',
          DTOutput(ns('coexp_tbl')),
          br(),
          uiOutput(ns('coexp_thres'))
        )
      )
    ) # tabPanel
  }
} # coexpressionPlotUI


#' Coexpression plot module server
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
#' @return reactive expression containing selected points from the coexpression plot
#'
#' @export
#'
coexpressionPlotServer <- function(id, app_object, filtered, genes_to_plot,
                                   args, gene_choices,
                                   all_selected, show_selection, reset_selection,
                                   reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      plot_data <- reactiveValues(coexp_tbl=NULL)

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
          choices <- list(gene_scratchpad=g, other=setdiff(gene_choices(), g))

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

      get_coexpression_plot <- eventReactive(c(app_object()$rds,
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

        if(input$split_by == 'none') split_var <- NULL
        else split_var <- input$split_by

        # check if gene is present in current slot
        if(length(g) > 2){
          showNotification(
            'More that 2 genes selected, using first two ...'
          )
          g <- g[seq_len(2)]
        }

        df <- get_marker_plot_data(g, app_object, filtered(), args, reduction=TRUE)

        if(!is.null(split_var)){
          validate(
            need(length(plt_split_lvls()) > 0,
                 'Need at least one split level to plot!')
          )

          if(length(plt_split_lvls()) > config()$server$max_split_levels){
            showNotification(
              "Many levels in splitting variable. This can take a while ...",
              type='warning'
            )
          }

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

        # save column names
        df_cols <- colnames(df)

        # get color range & set floor
        crange <- c(min(df[,g]), max(df[,g]))

        # downsample 0 expression rows
        if(length(g) > 1) zero_rows <- rowSums(df[,g] > crange[1]) == 0
        else zero_rows <- df[,g] == crange[1]

        # downsample to these many cells
        downsample_target <- config()$server$downsample_target
        if(sum(zero_rows) > downsample_target){
          if(input$downsample == 'yes'){
            showNotification(
              paste('Number of empty cells very large! Downsampling to',
                    downsample_target),
              type='warning'
            )
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

        # add check for empty marker size
        marker_size <- input$marker_size
        if(is.na(marker_size)) marker_size <- 3

        alpha <- input$marker_opacity

        if(input$free_axes == 'yes'){
          free_axes <- TRUE
        } else {
          free_axes <- FALSE
        }

        colors <- get_coexplt_colors(input$colormap)

        ht <- config()$server$plots$coexplt$base_ht*input$scale
        if(!is.null(split_var) & length(plt_split_lvls()) == 2){
            ht <- 0.75*ht
        }

        # change aspect ratio
        if(input$plot_aspect == 'narrow') wd <- 1.35*ht
        else wd <- NULL

        source <- 'coexpression_plot'

        pp <- feature_blend(df,
                            xcol=df_cols[1],
                            ycol=df_cols[2],
                            blend_cols=g,
                            colors=colors[2:4],
                            split=split_var,
                            col_threshold_1=input$thres_1/100,
                            col_threshold_2=input$thres_2/100,
                            neutral_color=colors[1],
                            #bin_mode=input$bin,
                            showlegend=TRUE,
                            type='scattergl',
                            marker_size=marker_size,
                            alpha=alpha,
                            free_axes=free_axes,
                            width=wd,
                            height=ht,
                            margin=0.05,
                            source=source)

          # get list elements
          p <- pp$plot
          df <- pp$data

          num_traces <- length(unique(df[[ 'color' ]]))
          if(!is.null(split_var)) num_traces <- num_traces*num_split

          # save plotted data
          plot_obj$df <- list(data=df,
                              xcol=df_cols[1],
                              ycol=df_cols[2],
                              color='color',
                              blend_cols=g,
                              colors=colors[2:4],
                              split=split_var,
                              col_threshold_1=input$thres_1/100,
                              col_threshold_2=input$thres_2/100,
                              neutral_color=colors[1],
                              marker_size=marker_size,
                              alpha=alpha,
                              source=source,
                              free_axes=free_axes,
                              num_traces=num_traces)
          plot_labeled(FALSE)

          # save trace names for split-view restyle alignment
          trace_data <- plotly::plotly_build(p)$x$data
          plot_obj$df$trace_names <- unlist(lapply(trace_data, function(x) unique(x$meta)))

        # save thresholds
        xlims <- get_limits(df[, g[1]])
        ylims <- get_limits(df[, g[2]])
        thres <- list(geneX=input$thres_1*(xlims[2] - xlims[1])/100,
                      geneY=input$thres_2*(ylims[2] - ylims[1])/100)
        names(thres) <- g
        plot_data$coexp_thres <- thres

        pct_df <- get_coexp_tbl(df, g,
                                threshold1=input$thres_1/100,
                                threshold2=input$thres_2/100)

        # add alpha
        colors <- paste0(colors, alpha*100)
        names(colors) <- pct_df$labels

        plot_data$coexp_tbl <- list(tbl=pct_df,
                                    colors=colors)

        event_register(p, 'plotly_selected')

        p
      })

      output$coexplt <- renderPlotly({
          get_coexpression_plot()
      })

      # summary table
      output$coexp_tbl <- renderDT({
        validate(
          need(!is.null(plot_data$coexp_tbl), '')
        )

        tbl <- plot_data$coexp_tbl$tbl
        colors <- plot_data$coexp_tbl$colors

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

      # print thresholds
      output$coexp_thres <- renderUI({
        validate(
          need(!is.null(plot_data$coexp_thres), '')
        )

        thres_msg <- NULL
        for(g in names(plot_data$coexp_thres)){
          tmp <- paste0(g, ' = ', plot_data$coexp_thres[[ g ]])
          if(is.null(thres_msg)) thres_msg <- tmp
          else thres_msg <- paste0(thres_msg, '; ', tmp)
        }

        tagList(
          strong('Thresholds'),
          h5(thres_msg)
        )
      })


      # interactive legend
      output$legend <- renderPlotly({
        colors <- get_coexplt_colors(input$colormap)

        g <- input$plt_genes
        validate(
          need(length(g) >= 2, '')
        )
        g <- g[seq_len(2)]

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
      plotProxy <- plotlyProxy('coexplt', session)

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
          trace_idx <- as.list(seq_along(opacity_list) - 1)
        } else {
          split_var <- plot_obj$df$split

          # Split by color and split variable to match Plotly trace groups.
          opacity_list <- split(marker_opacity,
                                f=list(plot_obj$df$data[[ plot_obj$df$color ]],
                                       plot_obj$df$data[[ split_var ]]),
                                drop=TRUE)

          trace_match <- match(plot_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
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

      helpButtonServer('coexplt_help', size='l')
      helpButtonServer('umap_ptselect_help', size='l')
      downloadPlotServer('plt_dload', get_coexpression_plot, 'coexpression_plot')

      # return selected points
      return(
        reactive({ selected_points$full })
      )
    } # function
  ) # moduleServer
} # coexpressionPlotServer
