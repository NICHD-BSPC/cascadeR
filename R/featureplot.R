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
#' @param reload_global reactive to trigger reload
#' @param refresh reactive to trigger plot refresh from sidebar button
#' @param config reactive list with config settings
#'
#' @export
#'
featurePlotServer <- function(id, app_object, filtered, genes_to_plot,
                              args, gene_choices, reload_global, refresh, config){
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

      observeEvent(app_object()$metadata_levels, {

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
                                     split=split_var,
                                     free_axes=free_axes,
                                     height=0.75*ht*length(g))
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
                          split=split_var,
                          free_axes=free_axes,
                          height=ht,
                          margin=0.05)
        }

        p
      })

      output$featureplt <- renderPlotly({
          get_feature_plot()
        }
      )

      helpButtonServer('featureplt_help', size='l')
      downloadPlotServer('plt_dload', get_feature_plot, 'feature_plot')

    } # function
  ) # moduleServer
} # featurePlotServer
