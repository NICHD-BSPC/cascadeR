#' Coexpression plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
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
            withSpinner(
              plotlyOutput(ns('coexplt'),
                           height='700px')
            ) # withSpinner
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
#' @param reload_global reactive to trigger reload
#' @param refresh reactive to trigger plot refresh from sidebar button
#' @param config reactive list with config settings
#'
#' @export
#'
coexpressionPlotServer <- function(id, app_object, filtered, genes_to_plot,
                                   args, gene_choices, reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      plot_data <- reactiveValues(coexp_tbl=NULL)

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
          g <- g[1:2]
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
        }

        # save column names
        df_cols <- colnames(df)

        # get color range & set floor
        crange <- c(min(df[,g]), max(df[,g]))

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

        p <- feature_blend(df,
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
                           height=ht,
                           margin=0.05)

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

      helpButtonServer('coexplt_help', size='l')
      downloadPlotServer('plt_dload', get_coexpression_plot, 'coexpression_plot')

    } # function
  ) # moduleServer
} # coexpressionPlotServer
