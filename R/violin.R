#' Violin plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
violinUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(col1, strong('Group by')),
        column(col2,
          selectInput(ns('grp_by'),
                      label=NULL,
                      choices=NULL,
                      selected=NULL)
        ) # column
      ), # fluidRow

      bsCollapse(
        bsCollapsePanel(span(icon('gear'), 'Edit group levels'),
                        value='edit group',
          controlUI(ns('plt_grp_lvls'), label=NULL)
        ) # bsCollapsePanel
      ), # bsCollapse

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
        column(col1, 'Show points?'),
        column(col2,
          selectInput(ns('plt_pts'),
                      label=NULL,
                      choices=c(TRUE, FALSE),
                      selected=FALSE)
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Free y-axis?'),
        column(col2,
          selectInput(ns('plt_scales'),
                      label=NULL,
                      choices=c('no', 'yes'))
        ) # column
      ) # fluidRow
    ) # tagList
  } else if(panel == 'main'){
    tabPanel('Violin Plot',
      br(),
      fluidRow(
        column(2, 'Select genes to plot', style='font-size: 16px;', align='center'),
        column(3,
          selectizeInput(ns('plt_genes'),
                       label=NULL,
                       choices=NULL, selected=NULL,
                       multiple=TRUE)
        ),
        column(4, align='left',
          actionButton(ns('plt_do'), 'Generate plot',
                       class='btn-primary')
        ),
        column(2, align='right',
          downloadPlotUI(ns('plt_dload'))
        ),
        column(1,
          align='left',
          helpButtonUI(ns('vlnplt_help'))
        )
      ), # fluidRow
      withSpinner(
        plotOutput(ns('vlnplt'), height='800px')
      ) # withSpinner
    ) # tabPanel
  }
} # violinUI


#' Violin plot module server
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
violinServer <- function(id, app_object, filtered, genes_to_plot,
                         args, gene_choices, reload_global, refresh, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      global_args <- reactiveValues(grp_by=NULL)

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

        updateSelectInput(session, 'grp_by',
                          choices=grouping_vars)

        updateSelectInput(session, 'split_by',
                          choices=c('none', grouping_vars))

      })

      observeEvent(reload_global(), {
        global_args$grp_by <- args()$grp_by

        updateSelectInput(session, 'grp_by',
                          selected=args()$grp_by)

      })

      #################### edit group levels ####################

      plt_grp_lvls <- controlServer('plt_grp_lvls',
                                  reactive({ app_object()$metadata_levels }),
                                  reactive({ req(input$grp_by); input$grp_by }),
                                  reactive({ NULL }))

      observeEvent(input$grp_by, {
        req(input$grp_by)

        global_args$grp_by <- input$grp_by
      })

      #################### edit split levels ####################

      plt_split_lvls <- controlServer('plt_split_lvls',
                                   reactive({ app_object()$metadata_levels }),
                                   reactive({ req(input$split_by); input$split_by }),
                                   reactive({ NULL }))

      #################### Main plotting function ####################

      get_violin_plot <- eventReactive(c(app_object()$rds,
                                         filtered(),
                                         input$plt_do,
                                         refresh()), {

        g <- input$plt_genes
        validate(
          need(g != '',
            paste0(
              '\nNo marker genes selected!\n\n',
              'Please select markers above and ',
              'then click the button to visualize here')
          )
        )

        max_vlnplt_genes <- config()$server$plots$vlnplt$max_genes
        if(length(g) > max_vlnplt_genes){
          showNotification(
            paste0('Warning: More than ', max_vlnplt_genes, ' selected! ',
                   'Plotting first ', max_vlnplt_genes),
            type='warning'
          )
          g <- g[1:max_vlnplt_genes]
        }

        if(input$split_by == 'none') split_var <- NULL
        else split_var <- input$split_by

        grp_var <- global_args$grp_by

        scales <- ifelse(input$plt_scales == 'yes',
                         'fixed', 'free_y')

        df <- get_marker_plot_data(g, app_object, filtered(), args)

        validate(
          need(length(plt_grp_lvls()) > 0,
               'Need at least one group level to plot!')
        )

        # only keep selected grp var levels
        keep_idx <- df[, grp_var] %in% plt_grp_lvls()
        if(!is.null(split_var)){
          keep_idx <- keep_idx & df[, split_var] %in% plt_split_lvls()
        }

        validate(
          need(sum(keep_idx) > 0,
               'No cells left after filtering levels!')
        )
        df <- df[keep_idx, ]
        df[, grp_var] <- factor(df[, grp_var],
                                levels=plt_grp_lvls())

        if(!is.null(split_var)){
          if(length(plt_split_lvls()) > config()$server$max_split_levels){
            showNotification(
              "Many levels in splitting variable. This can take a while ...",
              type='warning'
            )
          }

          df[, split_var] <- factor(df[, split_var],
                                  levels=plt_split_lvls())
        }

        if(!is.null(split_var)){
          colors <- app_object()$cluster_colors[[split_var]]
        } else {
          colors <- NULL
        }

        # melt df before plotting
        df2 <- reshape2::melt(df, id.vars=setdiff(colnames(df), g))

        p <- violin2(df2, xcol=grp_var, ycol='value',
                     color=split_var, colors=colors,
                     draw_points=as.logical(input$plt_pts),
                     scales=scales)

        #t3 <- system.time({
        ## NOTE: plotly version (keeping this for reference)
        #plist <- lapply(g, function(x){
        #           if(x == g[1]) showlegend <- TRUE
        #           else showlegend <- FALSE

        #           p <- violin_ly(df,
        #                          xcol=group_var,
        #                          ycol=x,
        #                          color=split_var,
        #                          colors=colors,
        #                          showlegend=showlegend)
        #           p
        #         })

        #suppressWarnings({
        #  if(n > 1)
        #    p1 <- subplot(plist, nrows=n, shareX=TRUE)
        #  else p1 <- plist[[1]]
        #})
        #})
        #cat('plotly: ', t3, '\n')

        p
      })

      output$vlnplt <- renderPlot(
        height = eventReactive(c(app_object()$rds,
                                 input$plt_do,
                                 refresh()), {

          n <- length(input$plt_genes)
          args <- config()$server$plots$vlnplt

          # scale height proportional to # genes
          if(n <= args$max_genes) args$base_ht
          else args$base_ht*n/args$max_genes
        }),
        {
        get_violin_plot()
      })

      helpButtonServer('vlnplt_help', size='l')
      downloadPlotServer('plt_dload', get_violin_plot, 'violin_plot')

    } # function
  ) # moduleServer
} # violinServer
