#' Line plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
linePlotUI <- function(id, panel){
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
                      choices=c('no', 'yes'))
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Free y-axis?'),
        column(col2,
          selectInput(ns('plt_scales'),
                      label=NULL,
                      choices=c('no', 'yes'))
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Autoscale y-axis?'),
        column(col2,
          selectInput(ns('plt_autoscale'),
                      label=NULL,
                      choices=c('yes', 'no'))
        ) # column
      ) # fluidRow

    ) # tagList
  } else if(panel == 'main'){
    tabPanel('Line Plot',
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
        ), # column
        column(1, align='left',
          helpButtonUI(ns('lineplt_help'))
        )
      ), # fluidRow
      withSpinner(
        plotlyOutput(ns('lineplt'),
                     height='800px')
      ) # withSpinner
    ) # tabPanel
  }
} # linePlotUI


#' Line plot module server
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
linePlotServer <- function(id, app_object, filtered, genes_to_plot,
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
                          choices=c('none', 'gene', grouping_vars),
                          selected='gene')

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

      get_line_plot <- eventReactive(c(app_object()$rds,
                                       filtered(),
                                       input$plt_do,
                                       refresh()), {

        g <- input$plt_genes
        validate(
          need(length(g) > 0,
               paste0(
                 '\nNo genes selected!\n\n',
                 'Please select markers above and ',
                 'then click the button to visualize here')
               )
        )

        df <- get_marker_plot_data(g, app_object, filtered(), args)

        grp_var <- global_args$grp_by
        if(input$split_by == 'none') split_var <- NULL
        else split_var <- input$split_by

        scales <- ifelse(input$plt_scales == 'no',
                         'fixed', 'free_y')

        points <- ifelse(input$plt_pts == 'no',
                         FALSE, TRUE)

        autoscale <- ifelse(input$plt_autoscale == 'no',
                         FALSE, TRUE)

        validate(
          need(length(plt_grp_lvls()) > 0,
               'Need at least one group level to plot!')
        )

        # only keep selected grp var levels
        keep_idx <- df[[ grp_var ]] %in% plt_grp_lvls()
        if(!is.null(split_var)){
          # only keep selected split var levels
          if(split_var != 'gene') keep_idx <- df[[ split_var ]] %in% plt_split_lvls()
        }

        validate(
          need(sum(keep_idx) > 0,
               'No cells left after filtering levels!')
        )
        df <- df[keep_idx, ]

        # drop extra columns before melting
        if(!is.null(split_var)){
          if(split_var == 'gene') df_minimal <- df[, c(grp_var, 'rn', g)]
          else df_minimal <- df[, c(grp_var, split_var, 'rn', g)]
        } else {
          df_minimal <- df[, c(grp_var, 'rn', g)]
        }
        df_melt <- reshape2::melt(df_minimal)

        # order grp_var and split_var levels
        df_melt[[ grp_var ]] <- factor(df_melt[[ grp_var ]],
                                       levels=plt_grp_lvls())
        if(!is.null(split_var)){
          if(split_var != 'gene'){
            df_melt[[ split_var ]] <- factor(df_melt[[ split_var ]],
                                            levels=plt_split_lvls())
          } else {
            # gene names are in the 'variable' column
            df_melt[[ 'variable' ]] <- factor(df_melt[[ 'variable' ]],
                                              levels=g)
          }
        }

        # make plot
        p <- ggplot(df_melt, aes(x=.data[[ grp_var ]], y=.data$value,
                                 group=.data$variable, fill=.data$variable, color=.data$variable))

        if(points)
          p <- p + geom_point(size=0.3, alpha=0.3, position=position_jitter(width=0.2))

        p <- p +
          geom_boxplot(aes(group=.data$variable), outliers=FALSE, alpha=0.3) +
          stat_summary(fun=median, geom='line', linetype='dashed', linewidth=0.5)

        if(autoscale) p <- p + ylim(range(df_melt$value))

        if(!is.null(split_var)){
          if(split_var == 'gene') p <- p + facet_wrap(~.data[[ 'variable' ]], scales=scales)
          else p <- p + facet_wrap(~.data[[ split_var ]], scales=scales)
        }
        p + theme_bw() +
          theme(axis.text.x=element_text(angle=90, size=10, hjust=1),
            strip.background=element_blank(),
            strip.text.x=element_text(size=12),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank())

        #lvls <- plt_grp_lvls()

        ## get mean expression within groups
        #all_idx <- lapply(lvls, function(x) df[, grp_var] == x)
        #names(all_idx) <- lvls

        #gdat2 <- lapply(all_idx, function(x){
        #           idx <- which(x)
        #           if(length(idx) == 1) idx <- c(idx, idx)
        #           colMeans(gdat[idx, ])
        #         })

        #df2 <- as.data.frame(do.call('rbind', gdat2))
        #df2 <- cbind(rownames(df2), df2)
        #colnames(df2)[1] <- grp_var
        #df2[, grp_var] <- factor(df2[, grp_var], levels=lvls)

        #df2 <- df2[, unique(colnames(df2))]


        #p <- plot_ly(x=~df2[, grp_var],
        #             y=~df2[, g[1]],
        #             type='scatter',
        #             mode='lines+markers',
        #             name=g[1])

        #for(gn in g[2:length(g)]){
        #  p <- p %>% add_trace(y=~df2[, gn],
        #                       name=gn,
        #                       mode='lines+markers')
        #}

        #p %>% layout(xaxis=list(title=grp_var),
        #             yaxis=list(title='Average expression',
        #                        type='linear'))
      })

      output$lineplt <- renderPlotly({
          get_line_plot()
      })

      helpButtonServer('lineplt_help', size='l')
      downloadPlotServer('plt_dload', get_line_plot, 'lineplot')

    } # function
  ) # moduleServer
} # linePlotServer
