#' Gene-gene scatter plot module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
scatterPlotUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(col1, strong('Color by')),
        column(col2,
          selectInput(ns('color_by'),
                      label=NULL,
                      choices=c('none', 'metadata', 'gene'),
                      selected='none')
        ) # column
      ), # fluidRow

      conditionalPanel(paste0('input["', ns('color_by'), '"] == "metadata"'),
        bsCollapse(open='edit group',
          bsCollapsePanel(span(icon('gear'), 'Metadata settings'),
                          value='edit group',
            fluidRow(
              column(col1, 'Choose metadata'),
              column(col2,
                selectInput(ns('grp_by'),
                            label=NULL,
                            choices=NULL,
                            selected=NULL)
              ) # column
            ), # fluidRow

            controlUI(ns('plt_grp_lvls'), label=NULL)
          ) # bsCollapsePanel
        ) # bsCollapse
      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('color_by'), '"] == "gene"'),
        bsCollapse(open='edit gene',
          bsCollapsePanel(span(icon('gear'), 'Gene settings'),
                          value='edit gene',
            fluidRow(
              column(col1, 'Choose gene'),
              column(col2,
                selectizeInput(ns('grp_gene'),
                               label=NULL,
                               choices=NULL,
                               selected=NULL)
              ) # column
            ), # fluidRow

            div(id=ns('assay_menu'),
              fluidRow(
                column(col1, em('Assay')),
                column(col2,
                  selectInput(ns('assay'),
                              label=NULL,
                              choices=NULL,
                              selected=NULL)
                ) # column
              ), # fluidRow
              fluidRow(
                column(col1, em('Data slot')),
                column(col2,
                  selectInput(ns('slot'),
                              label=NULL,
                              choices=NULL,
                              selected=NULL)
                ) # column
              ) # fluidRow
            ), # div

            fluidRow(
              column(col1, 'Color map'),
              column(col2,
                selectInput(ns('colormap'),
                            label=NULL,
                            choices=c('blues', 'yellow-green-blue', 'viridis'))
              ) # column
            ) # fluidRow

          ) # bsCollapsePanel
        ) # bsCollapse
      ), # conditionalPanel

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
          numericInput(ns('marker_opacity'),
                       label=NULL,
                       value=0.5, step=0.1,
                       min=0, max=1)
        ) # column
      ), # fluidRow

      fluidRow(
        column(col1, 'Scale plot'),
        column(col2,
          sliderInput(ns('scale'),
                      label=NULL,
                      value=1.0, step=0.1,
                      min=0.5, max=2, ticks=FALSE)
        ) # column
      ) # fluidRow

    ) # tagList
  } else if(panel == 'main'){
    tabPanel('Gene-gene Scatter',
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
          helpButtonUI(ns('scatterplt_help'))
        )
      ), # fluidRow
      div(align='center',
        withSpinner(
          plotlyOutput(ns('scatterplt'), height='800px')
        ) # withSpinner
      ) # div
    ) # tabPanel
  }
} # scatterPlotUI


#' Gene-gene scatter plot module server
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
scatterPlotServer <- function(id, app_object, filtered, genes_to_plot,
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

        updateSelectizeInput(session, 'grp_gene',
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

          ## NOTE: default returned value for selectizeInput with *multiple=TRUE*
          ##       is NULL, not ''
          if(!is.null(input$grp_gene)) selected <- input$grp_gene
          else selected <- ''
          updateSelectizeInput(session, 'grp_gene',
                               choices=choices,
                               selected=selected,
                               server=TRUE)

        }
      })

      observeEvent(app_object()$metadata_levels, {

        obj_type <- app_object()$obj_type
        if(obj_type == 'seurat'){

          shinyjs::show(id='assay_menu')

        } else if(obj_type == 'anndata'){
          shinyjs::hide(id='assay_menu')

        }

        grouping_vars <- app_object()$grouping_vars

        updateSelectInput(session, 'grp_by',
                          choices=grouping_vars)

        updateSelectInput(session, 'split_by',
                          choices=c('none', grouping_vars))

      })

      observeEvent(args()$assay_list, {
        updateSelectInput(session, 'assay',
                          choices=names(args()$assay_list))
      })

      observeEvent(input$assay, {
        updateSelectInput(session, 'slot',
                          choices=args()$assay_list[[ input$assay ]])
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

      get_scatter_plot <- eventReactive(c(app_object()$rds,
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

        # check if gene is present in current slot
        if(length(g) > 2){
          showNotification(
            'More that 2 genes selected, using first two ...'
          )
          g <- g[1:2]
        }

        grp_var <- global_args$grp_by
        if(input$split_by == 'none') split_var <- NULL
        else split_var <- input$split_by

        df <- get_marker_plot_data(g, app_object, filtered(), args)

        if(input$color_by == 'metadata'){
          validate(
            need(length(plt_grp_lvls()) > 0,
                 'Need at least one group level to plot!')
          )

          # only keep selected grp var levels
          keep_idx <- df[[ grp_var ]] %in% plt_grp_lvls()
          df <- df[keep_idx,]
        } else if(input$color_by == 'gene'){
          validate(
            need(input$grp_gene != '',
                '\nNo genes selected to color with!')
          )

          df_gene <- get_marker_plot_data(input$grp_gene, app_object, filtered(),
                                          args=reactive({ list(assay=input$assay, slot=input$slot) }))
          df <- cbind(df, df_gene[[ input$grp_gene ]])
          colnames(df)[ncol(df)] <- input$grp_gene
          g <- c(g, input$grp_gene)
        }

        if(!is.null(split_var)){
          if(length(plt_split_lvls()) >  config()$server$max_split_levels){
            showNotification(
              "Many levels in splitting variable. This can take a while ...",
              type='warning'
            )
          }

          # only keep selected split var levels
          keep_idx <- df[[ split_var ]] %in% plt_split_lvls()
          df <- df[keep_idx,]
        }

        validate(
          need(nrow(df) > 0,
               'No cells left after filtering levels!')
        )

        if(input$color_by == 'metadata'){
          df[[ grp_var ]] <- factor(df[[ grp_var ]],
                                  levels=plt_grp_lvls())
        }

        if(!is.null(split_var)){
          df[[ split_var ]] <- factor(df[[ split_var ]],
                                  levels=plt_split_lvls())
        }

        # bin gene expressions & create size column
        gdat_chr <- paste(df[, g[1]],
                          df[, g[2]])
        gdat_count <- table(gdat_chr)
        binned <- gdat_count[gdat_chr]

        # use hist to bin counts
        hh <- hist(gdat_count[gdat_chr], breaks=10, plot=FALSE)
        tmp <- findInterval(gdat_count[gdat_chr], hh$breaks)

        # build bin names
        # NOTE: not used
        bin_names <- paste0(hh$breaks[1:length(tmp)], '-',
                            hh$breaks[2:length(hh$breaks)])
        names(bin_names) <- tmp

        # build size column & hover text
        df$size <- as.numeric(tmp)*5

        # map binned counts to the 1:100 interval
        #df$size <- round((binned/diff(range(binned)))*100)

        # ggplot version
        # NOTE: untested
        #p <- ggplot(df, aes_string(x=g[1],
        #                           y=g[2],
        #                           size='size',
        #                           label='text')) +
        #     geom_point(alpha=0.5)
        #
        #ggplotly(p)

        # build color column
        if(input$color_by == 'metadata'){
          colors <- app_object()$cluster_colors[[ input$grp_by ]]
          df[, 'color'] <- df[, input$grp_by]

          df$text <- paste0(df[, 'color'], ' (n = ', binned, ')')
        } else if(input$color_by == 'gene'){
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
          df[, 'color'] <- factor(rep('.', nrow(df)))
          df[, 'n'] <- binned
        } else {
          colors <- NULL
          df[, 'color'] <- factor(rep('.', nrow(df)))
          df$text <- paste0('n = ', binned)
        }


        # only keep unique points
        df <- df[!duplicated(gdat_chr), ]

        # pad the x & y axis ranges
        xrange <- range(df[, g[1]])
        xrange[1] <- xrange[1] - 0.1*diff(xrange)
        xrange[2] <- xrange[2] + 0.1*diff(xrange)

        yrange <- range(df[, g[2]])
        yrange[1] <- yrange[1] - 0.1*diff(yrange)
        yrange[2] <- yrange[2] + 0.1*diff(yrange)

        all_range <- c(min(c(xrange, yrange)),
                       max(c(xrange, yrange)))

        # add check for empty marker_size
        marker_size <- input$marker_size
        if(is.na(marker_size)) marker_size <- 3

        # make sure alpha is within [0, 1]
        if(is.na(input$marker_opacity) | input$marker_opacity < 0)
          alpha <- 0.5
        else if(input$marker_opacity > 1) alpha <- 1
        else alpha <- input$marker_opacity

        # if gene names start with number, add character
        for(i in 1:length(g)){
          g_i <- g[i]
          cidx <- which(colnames(df) == g_i)
          if(regexpr('^\\d+', g_i) > 0){
            repl <- paste0('X', g_i)
            cidx <- which(colnames(df) == g_i)
            colnames(df)[cidx] <- repl
            g[i] <- repl
          }
        }

        # calculate plot dimensions
        ht <- 700*input$scale
        wd <- 1.25*ht
        if(!is.null(split_var)){
          if(length(plt_split_lvls()) == 2) ht <- 0.75*ht
        }

        # if coloring by metadata/none, use umap_ly
        # otherwise, if coloring by gene, use feature_ly
        if(input$color_by != 'gene'){
          p <- umap_ly(df, xcol=g[1], ycol=g[2],
                       color='color', colors=colors,
                       split=split_var,
                       type='scattergl',
                       label_cols='text',
                       alpha=alpha,
                       height=ht,
                       width=wd,
                       marker_size=marker_size)
        } else {
          crange <- range(df[[ input$grp_gene ]])

          p <- feature_ly(df,
                          xcol=g[1],
                          ycol=g[2],
                          color=g[3],
                          colors=colors,
                          label_cols=c(g[3], 'n'),
                          crange=range(df[[ g[3] ]]),
                          row_view='auto',
                          showscale=TRUE,
                          reversescale=reversescale,
                          marker_size=marker_size,
                          alpha=alpha,
                          split=split_var,
                          height=ht,
                          width=wd,
                          margin=0.05)

        }

        p

      })

      output$scatterplt <- renderPlotly({
          get_scatter_plot()
      })

      helpButtonServer('scatterplt_help', size='l')
      downloadPlotServer('plt_dload', get_scatter_plot, 'scatter_plot')

    } # function
  ) # moduleServer
} # scatterPlotServer
