#' Dotplot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
dotPlotUI <- function(id, panel){
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
        column(col1, 'Scale expression?'),
        column(col2,
          selectInput(ns('scale'),
                      label=NULL,
                      choices=c(TRUE, FALSE))
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Color map'),
        column(col2,
          selectInput(ns('colormap'),
                      label=NULL,
                      choices=c('viridis', 'blues'))
        ) # column
      ) # fluidRow
    ) # tagList
  } else if(panel == 'main'){
    tabPanel('DotPlot',
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
        column(1, align='left',
          helpButtonUI(ns('dotplt_help'))
        )
      ), # fluidRow
      withSpinner(
        plotOutput(ns('dotplt'), height='700px')
      ) # withSpinner
    ) # tabPanel
  }
} # dotPlotUI

#' Dotplot module server
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
dotPlotServer <- function(id, app_object, filtered, genes_to_plot,
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

      get_dot_plot <- eventReactive(c(app_object()$rds,
                                      filtered(),
                                      input$plt_do,
                                      refresh()), {

        g <- input$plt_genes
        validate(
          need(g != '',
               paste0(
                 '\nNo marker genes selected!\n\n',
                 'Please select markers in the "Gene scratchpad" and ',
                 'then click the button to visualize here')
               )
        )

        grp_var <- global_args$grp_by

        if(input$split_by == 'none'){
            split_var <- NULL
        } else {
            split_var <- input$split_by
        }

        df <- get_marker_plot_data(g, app_object, filtered(), args)


        validate(
          need(length(plt_grp_lvls()) > 0,
               'Need at least one group level to plot!')
        )

        if(!is.null(split_var)){
          validate(
            need(length(plt_split_lvls()) > 0,
                 'Need at least one split level to plot!')
          )
        }

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

        p <- dotplot(df, xcol=grp_var, ycol=g, split=split_var,
                     scale=as.logical(input$scale))

        #t2 <- system.time({
        #p1 <- DotPlot(obj,
        #             features=g,
        #             assay=input$assay,
        #             split.by=split_var,
        #             cols='Blues',
        #             dot.scale=10)
        #})
        #cat('seurat: ', t2, '\n')

        if(input$colormap == 'viridis')
          p <- p + scale_color_viridis()

        p + ylab(split_var) +
        #  coord_flip() + # this was needed for Seurat DotPlot
          theme(axis.title.y=element_blank(),
                axis.text.y=element_text(size=15, face='bold'),
                axis.text.x=element_text(size=15, angle=30, hjust=1),
                axis.title.x=element_text(size=15),
                legend.title=element_text(size=13),
                legend.text=element_text(size=11))
      })

      output$dotplt <- renderPlot(
        height = eventReactive(c(app_object()$rds,
                                 input$plt_do,
                                 refresh()), {

          n <- length(input$plt_genes)
          args <- config()$server$plots$dotplt
          if(input$split_by != 'none'){
            n <- n*length(plt_split_lvls())
          }

          # scale height proportional to # genes
          if(n <= args$max_genes) args$base_ht
          else args$base_ht*n/args$max_genes
        }),
        {
        get_dot_plot()
      })

      helpButtonServer('dotplt_help', size='l')
      downloadPlotServer('plt_dload', get_dot_plot, 'dotplot')

    } # function
  ) # moduleServer
} # dotPlotServer
