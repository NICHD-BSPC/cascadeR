#' Metadata viewer module UI
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
clustSummaryUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 5
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(12, align='right',
          helpButtonUI(ns('cell_counts_help'))
        ) # column
      ), # fluidRow

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] == "Summary"'),
        'Summary of metadata columns'
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] == "Cluster Distribution"'),
        fluidRow(
          column(col1, strong('Group by')),
          column(col2,
            selectInput(ns('cnt_grp_by'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL)
          ) # column
        ), # fluidRow

        bsCollapse(
          bsCollapsePanel(span(icon('gear'), 'Edit group levels'),
                          value='edit group',
            controlUI(ns('cnt_grp'), label=NULL)
          ) # bsCollapsePanel
        ), # bsCollapse

        conditionalPanel(
          paste0('input["', ns('clust_type'), '"] == "Histogram" | ',
                 'input["', ns('clust_type'), '"] == "Summary Table"'),

          fluidRow(
            column(col1, strong('Split by')),
            column(col2,
              selectInput(ns('cnt_split_by'),
                          label=NULL,
                          choices=NULL,
                          selected=NULL)
            ) # column
          ), # fluidRow

          bsCollapse(
            bsCollapsePanel(span(icon('gear'), 'Edit split levels'),
                            value='edit split',
              controlUI(ns('cnt_split'), label=NULL)
            ) # bsCollapsePanel
          ), # bsCollapse

          fluidRow(
            column(col1, 'Value type'),
            column(col2,
              selectInput(ns('cnt_type'),
                          label=NULL,
                          choices=c('proportion', 'count')
              ) # selectInput
            ) # column
          ), # fluidRow

          conditionalPanel(paste0('input["', ns('cnt_type'), '"] == "proportion"'),
            fluidRow(
              column(col1, 'Proportion w.r.t.'),
              column(col2,
                selectInput(ns('cnt_prop_type'),
                            label=NULL,
                            choices=c('Split by'='split',
                                      'Group by'='group')
                ) # selectInput
              ) # column
            ) # fluidRow
          ), # conditionalPanel

          conditionalPanel(paste0('input["', ns('clust_type'), '"] == "Histogram"'),
            fluidRow(
              column(col1, 'Split plot?'),
              column(col2,
                selectInput(ns('cnt_bar_facet'),
                            label=NULL,
                            choices=c('no', 'yes')
                ) # selectInput
              ) # column
            ) # fluidRow
          ) # conditionalPanel
        ) # conditionalPanel

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] == "Heatmap"'),

        fluidRow(
          column(col1, strong('Group by')),
          column(col2,
            selectInput(ns('hmap_grp_by'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL)
          ) # column
        ), # fluidRow

        bsCollapse(
          bsCollapsePanel(span(icon('gear'), 'Edit group levels'),
                          value='edit group',
            controlUI(ns('hmap_grp'), label=NULL)
          ) # bsCollapsePanel
        ), # bsCollapse

        wellPanel(
          controlUI(ns('hmap_col'), label='Choose columns'),
          fluidRow(
            column(col1, 'Select by pattern'),
            column(col2,
              selectizeInput(ns('hmap_select'),
                             label=NULL,
                             choices=c("Enter a pattern"=""),
                             multiple=TRUE,
                             options=list(create=TRUE))
            ) # column
          ) # fluidRow

        ), # wellPanel

        fluidRow(
          column(col1, 'Cluster rows?'),
          column(col2,
            selectInput(ns('hmap_rowclust'),
                        label=NULL,
                        choices=c('yes'=TRUE, 'no'=FALSE),
                        selected=TRUE)
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Cluster columns?'),
          column(col2,
            selectInput(ns('hmap_colclust'),
                        label=NULL,
                        choices=c('yes'=TRUE, 'no'=FALSE),
                        selected=TRUE)
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Scale by'),
          column(col2,
            selectInput(ns('hmap_scale'),
                        label=NULL,
                        choices=c('row', 'column', 'none'))
          ) # column
        ) # fluidRow
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] == "Feature Plot" & ',
               'input["', ns('ftrplt_type'), '"] == "Spatial"'),

        controlUI(ns('slice_ctrl'), label='Select slice'),

        fluidRow(
          column(col1, 'Free axes?'),
          column(col2,
            selectInput(ns('spat_ftrplt_free_axes'),
                        label=NULL,
                        choices=c('no', 'yes'),
                        selected='yes')
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Color map'),
          column(col2,
            selectInput(ns('spat_ftrplt_colormap'),
                        label=NULL,
                        choices=c('blues', 'yellow-green-blue', 'viridis'))
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Marker size'),
          column(col2,
            numericInput(ns('spat_ftrplt_marker_size'),
                         label=NULL,
                         value=3, min=1, step=1)
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Opacity'),
          column(col2,
            sliderInput(ns('spat_ftrplt_marker_opacity'),
                        label=NULL,
                        value=0.5, step=0.1,
                        min=0, max=1, ticks=FALSE)
          ) # column
        ) # fluidRow


      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] == "Feature Plot" & ',
               'input["', ns('ftrplt_type'), '"] == "UMAP"'),

        fluidRow(
          column(col1, strong('Split by')),
          column(col2,
            selectInput(ns('ftrplt_split_by'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL)
          ) # column
        ), # fluidRow

        bsCollapse(
          bsCollapsePanel(span(icon('gear'), 'Edit split levels'),
                          value='edit split',
            controlUI(ns('ftrplt_split'), label=NULL)
          ) # bsCollapsePanel
        ), # bsCollapse

        fluidRow(
          column(col1, 'Free axes?'),
          column(col2,
            selectInput(ns('ftrplt_free_axes'),
                        label=NULL,
                        choices=c('no', 'yes'))
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Color map'),
          column(col2,
            selectInput(ns('ftrplt_colormap'),
                        label=NULL,
                        choices=c('blues', 'yellow-green-blue', 'viridis'))
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Marker size'),
          column(col2,
            numericInput(ns('ftrplt_marker_size'),
                         label=NULL,
                         value=3, min=1, step=1)
          ) # column
        ), # fluidRow

        fluidRow(
          column(col1, 'Opacity'),
          column(col2,
            sliderInput(ns('ftrplt_marker_opacity'),
                        label=NULL,
                        value=0.5, step=0.1,
                        min=0, max=1, ticks=FALSE)
          ) # column
        ) # fluidRow

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('cell_counts'), '"] != "Summary"'),

        fluidRow(align='center',
          column(12,
            actionButton(ns('plt_do'), 'Refresh',
                         class='btn-primary',
                         style='margin-bottom: 10px;')
          ) # column
        ) # fluidRow
      ) # conditionalPanel
    ) # tagList
  } else if(panel == 'main'){
    tagList(
      tabsetPanel(type='tabs', id=ns('cell_counts'),

        tabPanel('Summary',
          fluidRow(
            style='margin-top: 10px;',
            column(2, 'Select metadata type', align='right', style='margin-top: 5px;'),
            column(6,
              selectInput(ns('summary_type'),
                          label=NULL,
                          choices=c('categorical', 'numeric'))
            ), # column
            column(3, align='right',
              conditionalPanel(
                paste0('input["', ns('summary_type'), '"] == "categorical"'),
                downloadFileUI(ns('summary_categorical_dload'))
              ),
              conditionalPanel(
                paste0('input["', ns('summary_type'), '"] == "numeric"'),
                downloadPlotUI(ns('summary_numeric_dload'))
              )
            ), # column
            column(1,
              align='left',
              helpButtonUI(ns('metadata_summary_help'))
            ) #column
          ), # fluidRow
          conditionalPanel(
            paste0('input["', ns('summary_type'), '"] == "categorical"'),
            withSpinner(
              DTOutput(ns('summary_tbl'))
            )
          ), # conditionalPanel

          conditionalPanel(
            paste0('input["', ns('summary_type'), '"] == "numeric"'),
            fluidRow(
              style='margin-top: 10px;',
              column(3,
                fluidRow(
                  column(4, strong('Plot type')),
                  column(8,
                    selectInput(ns('summary_plt_type'),
                                label=NULL, width='100%',
                                choices=c('bar', 'violin'))
                  )
                ), # fluidRow

                conditionalPanel(
                  paste0('input["', ns('summary_plt_type'), '"] == "violin"'),

                  fluidRow(
                    column(4, 'Group by'),
                    column(8,
                      selectizeInput(ns('summary_grp_by'),
                                  label=NULL, width='100%',
                                  choices=NULL)
                    ) # column
                  ), # fluidRow

                  bsCollapse(
                    bsCollapsePanel(span(icon('gear'), 'Edit group levels'),
                                    value='edit group',
                      controlUI(ns('summary_grp'), label=NULL)
                    ) # bsCollapsePanel
                  ) # bsCollapse

                ), # conditionalPanel

                bsCollapse(
                  bsCollapsePanel('Columns to plot',
                                  value='choose cols',
                    controlUI(ns('summary_col'), label=NULL),

                    fluidRow(
                      column(col1, 'Select by pattern'),
                      column(col2,
                        selectizeInput(ns('summary_select'),
                                       label=NULL,
                                       choices=c("Enter a pattern"=""),
                                       multiple=TRUE,
                                       options=list(create=TRUE))
                      ) # column
                    ) # fluidRow
                  ) # bsCollapsePanel
                ), # bsCollapse

                fluidRow(
                  column(12, align='center',
                    actionButton(ns('summary_plt_do'), 'Refresh',
                                 class='btn-primary',
                                 style='margin-bottom: 10px;')
                  ) # column
                ) # fluidRow
              ), # column
              column(9,
                withSpinner(
                  plotlyOutput(ns('summary_num'), height="700px")
                )
              ) # column
            ) # fluidRow
          ), # conditionalPanel
        ), # tabPanel

        tabPanel('Cluster Distribution',
          tabsetPanel(type='tabs', id=ns('clust_type'),
            tabPanel('Histogram',
              fluidRow(
                column(11, align='right',
                  downloadPlotUI(ns('cnt_bar_dload'))
                ), # column
                column(1,
                  align='left',
                  helpButtonUI(ns('metadata_histogram_help'))
                ) #column
              ), # fluidRow

              withSpinner(
                plotlyOutput(ns('cnt_bar'), height='700px')
              ) # withSpinner
            ), # tabPanel

            tabPanel('Summary Table',
              fluidRow(
                column(11, align='right',
                  downloadFileUI(ns('cnt_tbl_dload'))
                ), # column
                column(1,
                  align='left',
                  helpButtonUI(ns('metadata_summary_table_help'))
                ) #column
              ), # fluidRow

              withSpinner(
                DTOutput(ns('cnt_tbl'))
              ) # withSpinner
            ) # tabPanel
          ) # tabsetPanel
        ), # tabPanel

        tabPanel('Feature Plot',
          tabsetPanel(type='tabs', id=ns('ftrplt_type'),
            tabPanel('Spatial',
              br(),
              fluidRow(
                column(2, 'Select variable', style='font-size: 16px;', align='center'),
                column(3,
                  selectizeInput(ns('spat_ftrplt_var'),
                               label=NULL,
                               choices=NULL, selected=NULL)
                ),
                column(4, align='left',
                  actionButton(ns('spatial_featureplt_do'), 'Generate plot',
                               class='btn-primary')
                ),
                column(2, align='right',
                  downloadPlotUI(ns('spatial_featureplt_dload'))
                ), # column
                column(1,
                  align='left',
                  helpButtonUI(ns('metadata_spatial_featureplt_help'))
                ) #column
              ), # fluidRow
              div(align='center',
                withSpinner(
                  plotlyOutput(ns('spatial_featureplt'),
                               width='auto',
                               height='auto')
                ) # withSpinner
              ) # div
            ), # tabPanel

            tabPanel('UMAP',
              br(),
              fluidRow(
                column(2, 'Select variable', style='font-size: 16px;', align='center'),
                column(3,
                  selectizeInput(ns('ftrplt_var'),
                               label=NULL,
                               choices=NULL, selected=NULL)
                ),
                column(4, align='left',
                  actionButton(ns('featureplt_do'), 'Generate plot',
                               class='btn-primary')
                ),
                column(2, align='right',
                  downloadPlotUI(ns('featureplt_dload'))
                ), # column
                column(1,
                  align='left',
                  helpButtonUI(ns('metadata_featureplt_help'))
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

          ) # tabsetPanel
        ), # tabPanel

        tabPanel('Heatmap',
          fluidRow(
            column(11, align='right',
              downloadPlotUI(ns('cnt_hmap_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('metadata_heatmap_help'))
            ) # column
          ), # fluidRow

          withSpinner(
            plotlyOutput(ns('cnt_hmap'), height='700px')
          ) # withSpinner
        ) # tabPanel

      ) # tabsetPanel
    ) # tagList

  }
}

#' Metadata viewer module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param filtered cell barcodes for filtering object
#' @param args reactive list with elements, 'grp_by' for grouping variable
#'        and 'dimred' for which dimension reduction to use
#' @param reload_global reactive to trigger reload
#' @param config reactive list with config settings
#'
#' @export
#'
clustSummaryServer <- function(id, obj, filtered, args, reload_global, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        list(
          rds=obj$rds,
          obj_type=obj$obj_type,
          metadata=obj$metadata,
          metadata_levels=obj$metadata_levels$filtered,
          metadata_numeric=obj$metadata_numeric$all,
          cluster_colors=obj$cluster_colors,
          grouping_vars=obj$grouping_vars,
          spatial_coords=obj$spatial_coords,
          imagerow_max=obj$imagerow_max,
          imagerow_min=obj$imagerow_min
        )
      })

      obj_info <- reactiveValues(filtered=NULL,
                                 metadata_levels=list(),
                                 metadata_num_cols=list(all=NULL, hmap_select=NULL, summary_select=NULL),
                                 slice_choices=NULL,
                                 summary=list(),
                                 imagerow_max=list(),
                                 imagerow_min=list())

      global_args <- reactiveValues(cnt_grp_by=NULL,
                                    hmap_grp_by=NULL,
                                    summary_grp_by=NULL)

      # read & save filtered object data
      observeEvent(filtered(), {
        # reset data on (re)load
        obj_info$filtered <- NULL
        obj_info$slice_choices <- NULL
        obj_info$metadata_levels <- app_object()$metadata_levels

        bc <- filtered()
        mdata <- app_object()$metadata
        obj_info$filtered <- bc
        idx <- rownames(mdata) %in% bc

        if(app_object()$obj_type == 'seurat'){

          if(!'Spatial' %in% names(app_object()$rds@assays)){
            hideTab(inputId='ftrplt_type', target='Spatial')

            sel_plt <- input$cell_counts
            # make sure something is selected
            updateTabsetPanel(session, inputId='cell_counts',
                              selected=sel_plt)

            updateTabsetPanel(session, inputId='ftrplt_type',
                              selected='UMAP')
            updateTabsetPanel(session, inputId='coexplt_type',
                              selected='UMAP')
          } else {
            showTab(inputId='ftrplt_type', target='Spatial',
                    select=TRUE)
            showTab(inputId='coexplt_type', target='Spatial',
                    select=TRUE)

            slice_choices <- intersect(unique(app_object()$spatial_coords$slice),
                                       unique(mdata$orig.ident))

            # if 'orig.ident' and slice names don't match, show everything
            if(length(slice_choices) == 0) slice_choices <- unique(app_object()$spatial_coords$slice)

            obj_info$slice_choices <- slice_choices
          }

        } else if(app_object()$obj_type == 'anndata'){
          obj_info$slot <- 'none'

          if(!'spatial' %in% names(app_object()$rds$obsm)){
            hideTab(inputId='ftrplt_type', target='Spatial')

            sel_plt <- input$cell_counts
            # make sure something is selected
            updateTabsetPanel(session, inputId='cell_counts',
                              selected=sel_plt)

            updateTabsetPanel(session, inputId='ftrplt_type',
                              selected='UMAP')
          } else {
            showTab(inputId='ftrplt_type', target='Spatial',
                    select=TRUE)

            # NOTE: we only support a single slice in an anndata object
            obj_info$slice_choices <- 'slice'
          }

        }

        showNotification(
          'Loaded metadata module ...'
        )
      })

      # update grouping var menus
      observeEvent(app_object()$grouping_vars, {
        obj_info$metadata_levels <- app_object()$metadata_levels
        obj_info$metadata_num_cols <- list(all=NULL, hmap_select=NULL, summary_select=NULL)
        obj_info$summary <- NULL

        grouping_vars <- app_object()$grouping_vars

        # update split_by menus
        if('orig.ident' %in% grouping_vars) selected <- 'orig.ident'
        else if('idents' %in% grouping_vars) selected <- 'idents'
        else selected <- 'none'

        grp_plt_choices <- c('cnt', 'hmap', 'summary')
        for(plt in grp_plt_choices){
          updateSelectInput(session, paste0(plt, '_grp_by'),
                            choices=grouping_vars)
        }

        # make sure splitting variables don't have too many levels
        max_split_levels <- config()$server$max_split_levels
        split_idx <- NULL
        for(i in 1:length(grouping_vars)){
          gv <- grouping_vars[i]
          if(length(app_object()$metadata_levels[[ gv ]]) <= max_split_levels){
            split_idx <- c(split_idx, i)
          }
        }
        split_vars <- grouping_vars[split_idx]

        split_plt_choices <- c('cnt', 'ftrplt')
        for(plt in split_plt_choices){
          updateSelectInput(session, paste0(plt, '_split_by'),
                            choices=c('none', split_vars))
        }

        num_cols <- names(app_object()$metadata_numeric)
        obj_info$metadata_num_cols$all <- num_cols
        obj_info$metadata_num_cols$hmap_select <- num_cols
        obj_info$metadata_num_cols$summary_select <- num_cols

        updateSelectizeInput(session, 'ftrplt_var',
                             choices=c('none'='', num_cols))
        updateSelectizeInput(session, 'spat_ftrplt_var',
                             choices=c('none'='', num_cols))

        # get summary of factor cols
        factor_cols <- lapply(obj_info$metadata_levels, function(x){
                         data.frame(num_levels=length(x), levels=paste(x, collapse=', '))
                       })
        df <- do.call('rbind', factor_cols)
        df <- cbind(rownames(df), df)
        colnames(df)[1] <- c('factor_column')
        obj_info$summary[[ 'factor' ]] <- df

      })

      observeEvent(reload_global(), {
        grp_by <- args()$grp_by

        plt_choices <- c('cnt', 'hmap', 'summary')

        for(plt in plt_choices){
          global_args[[ paste0(plt, '_grp_by') ]] <- grp_by

          updateSelectInput(session, paste0(plt, '_grp_by'),
                            selected=grp_by)
        }
      })

      #################### overall summary ####################

      summary_grp <- controlServer('summary_grp',
                                   reactive({ obj_info$metadata_levels }),
                                   reactive({ req(input$summary_grp_by); input$summary_grp_by }),
                                   reactive({ NULL }))

      observeEvent(input$summary_grp_by, {
        req(input$summary_grp_by)

        global_args$summary_grp_by <- input$summary_grp_by
      })

      #################### select columns menu ################

      summary_col <- controlServer('summary_col',
                                   reactive({ list(all=obj_info$metadata_num_cols$all) }),
                                   'all',
                                   reactive({ obj_info$metadata_num_cols$summary_select }))

      observeEvent(input$summary_select, {
        validate(
          need(!all(input$summary_select == ''), '')
        )
        idx <- unique(unlist(lapply(input$summary_select, function(x){
                  grep(x, obj_info$metadata_num_cols$all)
               })))
        obj_info$metadata_num_cols$summary_select <- obj_info$metadata_num_cols$all[idx]
      })


      ################## summary table ###########################

      output$summary_tbl <- renderDT({
        req(obj_info$summary[[ 'factor' ]])
        datatable(obj_info$summary[[ 'factor' ]], rownames=FALSE)
      })

      ################## summary plot ############################

      summary_num_plot <- eventReactive(c(input$summary_plt_do, obj_info$summary), {
        if(input$summary_plt_type == 'bar'){
          if(is.null(summary_col())){
            validate(
              need(!is.null(summary_col()),
                   'Must select at least one column to plot!')
            )
          }
          # get summary of numeric cols
          mn <- lapply(summary_col(), function(x){
                  data.frame(num_col=x, app_object()$metadata_numeric[[ x ]])
                })
          df <- do.call('rbind', mn)

          # calculate appropriate binwidths for each column
          df_ranges <- unlist(lapply(split(df, f=df[['num_col']]), function(x){
                          tmp <- range(x$mids)
                          (tmp[2] - tmp[1])/nrow(x)
                       }))
          df[[ 'binwidth' ]] <- df_ranges[df[['num_col']]]

          p <- ggplot(df, aes(y=.data$counts, width=.data$binwidth, x=.data$mids)) +
                geom_col(position='dodge')

          # if faceting var has too many levels, set num cols to 5
          # and increase plot height
          n <- length(summary_col())
          if(n <= 25){
            p <- p + facet_wrap(~num_col, scales='free')
            ht  <- 700
          } else {
            p <- p + facet_wrap(~num_col, scales='free', ncol=5)
            ht <- ceiling(n/5)*(700/5)
          }

          p <- p +
                xlab('') +
                theme_bw() +
                theme(axis.text.x=element_text(angle=30, hjust=1))

          ggplotly(p, height=ht)

        } else if(input$summary_plt_type == 'violin'){
          grp_var <- input$summary_grp_by
          split_vars <- summary_col()

          df <- app_object()$metadata[, c(grp_var, split_vars)]

          # melt df before plotting
          df2 <- reshape2::melt(df)

          df2[[ grp_var ]] <- factor(df2[[ grp_var ]],
                                     levels=summary_grp())

          if(length(summary_col()) >  config()$server$max_split_levels){
            showNotification(
              "Generating plots for many metadata columns. This can take a while ...",
              type='warning'
            )
          }

          df2[[ 'variable' ]] <- factor(df2[[ 'variable' ]],
                                        levels=summary_col())

          p <- violin2(df2, xcol=grp_var, ycol='value',
                       scales='free', text_scale=0.7) +
                theme(legend.position='none',
                      axis.text.x=element_text(angle=30, hjust=1))

          if(length(summary_col()) <= 5) ggplotly(p, height=700)
          else ggplotly(p, height=length(summary_col())*700/5)
        }
      })

      output$summary_num <- renderPlotly({
        req(app_object()$rds)
        summary_num_plot()
      })

      #################### Count plot settings ####################

      #################### edit group levels ####################

      cnt_grp <- controlServer('cnt_grp',
                               reactive({ obj_info$metadata_levels }),
                               reactive({ req(input$cnt_grp_by); input$cnt_grp_by }),
                               reactive({ NULL }))

      observeEvent(input$cnt_grp_by, {
        req(input$cnt_grp_by)

        global_args$cnt_grp_by <- input$cnt_grp_by
      })

      #################### edit split levels ####################

      cnt_split <- controlServer('cnt_split',
                                 reactive({ obj_info$metadata_levels }),
                                 reactive({ req(input$cnt_split_by); input$cnt_split_by }),
                                 reactive({ NULL }))

      #################### Count table ####################

      get_cluster_counts <- eventReactive(c(app_object()$rds,
                                            obj_info$filtered,
                                            reload_global(),
                                            input$plt_do), {

        validate(
          need(input$cnt_grp_by != '' & input$cnt_split_by != '', 'Waiting for selection')
        )

        validate(
          need(input$cnt_grp_by != input$cnt_split_by,
               'Please select different variables to group and split cells')
        )

        mdata <- app_object()$metadata

        # only keep filtered barcodes
        bc <- obj_info$filtered
        idx <- rownames(mdata) %in% bc
        mdata <- mdata[idx, ]

        grp_by <- input$cnt_grp_by
        split_by <- input$cnt_split_by
        cnt_type <- input$cnt_type

        # only keep selected grp levels & order by input
        validate(
          need(length(cnt_grp()) > 0,
               'Need at least one group level to plot!')
        )
        mdata <- mdata[mdata[, grp_by] %in% cnt_grp(), ]
        mdata[, grp_by] <- factor(mdata[, grp_by], levels=cnt_grp())

        if(split_by != 'none'){
            validate(
              need(length(cnt_split()) > 0,
                   'Need at least one split level to plot!')
            )

            # only keep selected split levels & order by input
            mdata <- mdata[mdata[, split_by] %in% cnt_split(), ]
            mdata[, split_by] <- factor(mdata[, split_by], levels=cnt_split())

            df <- data.frame(row.names=rownames(mdata),
                             group=mdata[, grp_by],
                             split=mdata[, split_by])

            df <- df %>%
              dplyr::count(.data$group, .data$split)
            if(input$cnt_prop_type == 'split'){
              df <- df %>%
              dplyr::add_count(.data$split, wt=.data$n, name='nn') %>%
              dplyr::rename(count=.data$n) %>%
              dplyr::mutate(proportion=.data$count/.data$nn) %>%
              dplyr::select(-.data$nn)
            } else {
              df <- df %>%
              dplyr::add_count(.data$group, wt=.data$n, name='nn') %>%
              dplyr::rename(count=.data$n) %>%
              dplyr::mutate(proportion=.data$count/.data$nn) %>%
              dplyr::select(-.data$nn)
            }

          if(is.factor(mdata[, split_by])){
            df$split <- factor(df$split, levels(mdata[, split_by]))
          }

        } else {
            counts <- table(mdata[, grp_by])
            lvls <- names(counts)
            cnts <- as.vector(counts)
            idx <- cnts > 0

            df <- data.frame(row.names=lvls[idx],
                             group=lvls[idx],
                             count=cnts[idx],
                             proportion=cnts[idx]/sum(cnts))

        }

        if(is.factor(mdata[, grp_by])){
          df$group <- factor(df$group, levels(mdata[, grp_by]))
        }

        df
      })

      #################### Count table for download ####################

      current_cluster_counts <- eventReactive(c(get_cluster_counts(),
                                                input$plt_do), {

        df <- get_cluster_counts()

        if(input$cnt_split_by != 'none'){
          if(input$cnt_type == 'proportion'){
              df <- df %>%
                  dplyr::select(-.data$count) %>%
                  tidyr::spread(.data$split, .data$proportion)
          } else {
              df <- df %>%
                  dplyr::select(-.data$proportion) %>%
                  tidyr::spread(.data$split, .data$count)
          }
        } else {
          df <- df[, c('group', input$cnt_type)]
        }

        df
      })

      #################### Summary table ####################

      output$cnt_tbl <- renderDT({
        df <- get_cluster_counts()

        # isolate to prevent unnecessary redraw
        isolate({
          split_by <- input$cnt_split_by
          cnt_type <- input$cnt_type
          grp_by <- input$cnt_grp_by
        })

        if(split_by != 'none'){
          if(cnt_type == 'proportion'){
              df <- df %>%
                  dplyr::select(-.data$count) %>%
                  tidyr::spread(.data$split, .data$proportion)
              format.idx <- 2:ncol(df)
          } else {
              df <- df %>%
                  dplyr::select(-.data$proportion) %>%
                  tidyr::spread(split, count)
              format.idx <- NULL
          }
        } else {
          df <- df[, c('group', cnt_type)]

          if(cnt_type == 'proportion') format.idx <- 'proportion'
          else format.idx <- NULL
        }

        idx <- colnames(df) == 'group'
        colnames(df)[idx] <- grp_by

        df <- df %>%
          datatable(rownames=FALSE,
                    options=list(paging=FALSE))
        if(!is.null(format.idx)){
            df <- df %>% formatSignif(columns=format.idx, digits=4)
        }

        df
      })

      #################### Histogram ####################

      get_cluster_bar <- reactive({
        df <- get_cluster_counts()

        # isolate to prevent unnecessary redraw
        isolate({
          split_by <- input$cnt_split_by
          grp_by <- input$cnt_grp_by
          cnt_bar_facet <- input$cnt_bar_facet
          cnt_type <- input$cnt_type
        })

        if('split' %in% colnames(df)){
          if(cnt_bar_facet == 'yes'){
            p <- ggplot(df, aes_string(x='group', y=cnt_type)) +
                    facet_grid(split~.) +
                    geom_col(position='dodge', alpha=0.7)
          } else {
            p <- ggplot(df, aes_string(x='group', y=cnt_type,
                                       fill='split')) +
                    geom_col(position='stack', alpha=0.7)
          }
        } else {
            p <- ggplot(df, aes_string(x='group', y=cnt_type)) +
                    geom_col(position='dodge', alpha=0.7)
        }

        p <- p +
            theme_bw() +
            scale_fill_manual(values=app_object()$cluster_colors[[ split_by ]]) +
            theme(
              axis.text.x=element_text(angle = 45,
                                       vjust = 0.5,
                                       hjust=1)
              ) +
            xlab(grp_by) +
            guides(fill=guide_legend(''))
      })

      output$cnt_bar <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), 'Waiting for selection')
        )
        get_cluster_bar()
      })

      #################### Heatmap settings ####################

      #################### edit group levels ####################

      hmap_grp <- controlServer('hmap_grp',
                                reactive({ obj_info$metadata_levels }),
                                reactive({ req(input$hmap_grp_by); input$hmap_grp_by }),
                                reactive({ NULL }))

      observeEvent(input$hmap_grp_by, {
        req(input$hmap_grp_by)

        global_args$hmap_grp_by <- input$hmap_grp_by
      })

      #################### edit numeric columns ####################

      hmap_col <- controlServer('hmap_col',
                                reactive({ list(all=obj_info$metadata_num_cols$all) }),
                                'all',
                                reactive({ obj_info$metadata_num_cols$hmap_select }))

      observeEvent(input$hmap_select, {
        validate(
          need(!all(input$hmap_select == ''), '')
        )
        idx <- unique(unlist(lapply(input$hmap_select, function(x){
                  grep(x, obj_info$metadata_num_cols$all)
               })))
        obj_info$metadata_num_cols$hmap_select <- obj_info$metadata_num_cols$all[idx]
      })

      ######################### Heatmap #########################

      get_cluster_heatmap <- eventReactive(c(app_object()$rds,
                                             obj_info$filtered,
                                             input$plt_do), {

        validate(
          need(length(hmap_col()) > 0,
               'Need at least one score column to generate heatmap!')
        )

        mdata <- app_object()$metadata

        # only keep filtered barcodes
        bc <- obj_info$filtered
        idx <- rownames(mdata) %in% bc
        mdata <- mdata[idx, ]

        # check that grouping levels are not empty
        validate(
          need(length(hmap_grp()) > 0,
               'Need at least one group level to plot!')
        )
        # get all clusters
        all_clusters <- hmap_grp()

        # check that grouping levels are not empty
        validate(
          need(length(hmap_col()) > 1,
               'Need at least two columns to generate heatmap!')
        )
        # get scores matrix
        scores_mat <- mdata[, hmap_col()]

        # aggregate scores & save to df
        # here we save the mean of scores
        cluster_scores <- list()
        for (cl in all_clusters) {
          cells_cl <- rownames(mdata)[mdata[, input$hmap_grp_by] == cl]
          cluster_scores[[ cl ]] <- colMeans(scores_mat[cells_cl, ], na.rm=TRUE)
        }

        cluster_scores_final <- do.call("rbind", cluster_scores)

        if(input$hmap_scale == 'row'){
          cluster_scores_final <- scale(cluster_scores_final)
        } else if(input$hmap_scale == 'column'){
          cluster_scores_final <- t(scale(t(cluster_scores_final)))
        }

        # cluster by rows and/or columns
        if(input$hmap_colclust){
          row_clust <- tryCatch(
                         {
                           hclust(dist(cluster_scores_final))
                         },
                         error=function(e){ e }
                       )

          validate(
            need(!any(class(row_clust) %in% c('simpleError', 'error')),
                 'Error clustering columns. Try setting "Cluster columns?" to "no"')

          )
          cluster_scores_final <- cluster_scores_final[row_clust$order, ]
        }

        if(input$hmap_rowclust){
          col_clust <- tryCatch(
                         {
                           hclust(dist(t(cluster_scores_final)))
                         },
                         error=function(e){ e }
                       )

          validate(
            need(!any(class(col_clust) %in% c('simpleError', 'error')),
                 'Error clustering rows. Try setting "Cluster rows?" to "no"')

          )
          cluster_scores_final <- cluster_scores_final[, col_clust$order]
        }

        df <- reshape2::melt(cluster_scores_final)
        colnames(df) <- c('cluster', 'cell_type', 'value')
        df[, 'cell_type'] <- factor(df[, 'cell_type'],
                                    levels=colnames(cluster_scores_final))
        df[, 'cluster'] <- factor(df[, 'cluster'],
                                  levels=rownames(cluster_scores_final))

        p <- ggplot(df, aes(x=.data$cluster, y=.data$cell_type, fill=.data$value)) +
             geom_tile() +
             scale_fill_viridis(discrete=FALSE) +
             xlab('') + ylab('') +
             theme(
               axis.text.x = element_text(angle=45,
                                          vjust=0.5,
                                          hjust=1)
             )

        p
      })

      output$cnt_hmap <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), 'Waiting for selection')
        )
        get_cluster_heatmap()
      })

      #################### Feature plot settings ####################

      # function to return plot data
      #
      # - this returns a data frame with metadata, gene data
      #   and reduction (if reduction=TRUE)
      #
      get_var_data <- function(var, reduction=FALSE){
        obj_type <- app_object()$obj_type
        mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=T)
        bc <- obj_info$filtered

        # get metadata
        idx <- mdata$rn %in% bc
        df <- mdata[idx,]

        # extract reduction data if needed
        if(reduction){
          if(obj_type == 'seurat'){
            dimred <- app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings[idx, ]
          } else if(obj_type == 'anndata'){
            dimred <- app_object()$rds$obsm[[ args()$dimred ]][idx,]

            label <- sub('X_', '', args()$dimred)
            colnames(dimred) <- paste0(label, 1:2)
          }

          df <- cbind(as.data.frame(dimred), df)
        }

        df
      }

      #################### edit split levels ####################

      ftrplt_split <- controlServer('ftrplt_split',
                                    reactive({ obj_info$metadata_levels }),
                                    reactive({ req(input$ftrplt_split_by); input$ftrplt_split_by }),
                                    reactive({ NULL }))

      ##################### Feature Plot ########################

      get_feature_plot <- eventReactive(c(app_object()$rds,
                                          obj_info$filtered,
                                          input$featureplt_do,
                                          input$plt_do), {
        validate(
          need(input$ftrplt_var != '',
               paste0(
                 '\nNo metadata variables selected!\n\n',
                 'Please select above and ',
                 'then click the button to visualize here')
               )
        )

        if(input$ftrplt_split_by == 'none') split_var <- NULL
        else split_var <- input$ftrplt_split_by

        # get variables to plot
        var <- input$ftrplt_var

        # feature plot config
        args <- config()$server$plots$ftrplt

        max_var <- args$max_genes
        if(length(var) > max_var){
          showNotification(
            paste0('Feature plot supports upto ', max_var,
                   ' variables at a time. Using first ', max_var),
            type='warning'
          )
          var <- var[1:max_var]
        }

        # adjust plot height based on number of genes
        ht <- args$base_ht

        df <- get_var_data(var, reduction=TRUE)

        if(!is.null(split_var)){
          validate(
            need(length(ftrplt_split()) > 0,
                 'Need at least one split level to plot!')
          )

          # only keep selected split var levels
          keep_idx <- df[, split_var] %in% ftrplt_split()

          validate(
            need(sum(keep_idx) > 0,
                 'No cells left after filtering levels!')
          )
          df <- df[keep_idx, ]

          df[, split_var] <- factor(df[, split_var],
                                    levels=ftrplt_split())
        }

        # add check for empty marker size
        marker_size <- input$ftrplt_marker_size
        if(is.na(marker_size)) marker_size <- 2

        alpha <- input$ftrplt_marker_opacity
        free_axes <- ifelse(input$ftrplt_free_axes == 'yes', TRUE, FALSE)

        # colormaps
        reversescale <- FALSE
        if(input$ftrplt_colormap == 'blues'){
          colors <- 'Blues'
          reversescale <- TRUE
        } else if(input$ftrplt_colormap == 'yellow-green-blue'){
          colors <- 'YlGnBu'
        } else if(input$ftrplt_colormap == 'viridis'){
          colors <- 'Viridis'
        }

        if(length(var) > 1){
          row_view <- 'single'
        } else {
          row_view <- 'auto'
        }

        # get color range & set floor
        crange <- c(min(df[, var], na.rm=TRUE), max(df[, var], na.rm=TRUE))
        if(crange[1] < 0) crange[1] <- 0

        lvls <- ftrplt_split()
        # arrange multi-var view into rows
        if(length(var) > 1){
          if(!is.null(split_var)){
            if(length(lvls) > 5){
              showNotification(
                paste0('Warning: Split variable has more than five levels. For best results, ',
                       'plot a single variable when splitting the plot into many subplots'),
                type='warning', duration=10
              )
            }
          }

          # get list of plotly handles
          plist <- lapply(1:length(var), function(x){
                     if(x == 1) showscale <- TRUE
                     else showscale <- FALSE

                     p <- feature_ly(df,
                                     xcol=colnames(df)[1],
                                     ycol=colnames(df)[2],
                                     color=var[x],
                                     colors=colors,
                                     crange=crange,
                                     row_view=row_view,
                                     showscale=showscale,
                                     reversescale=reversescale,
                                     marker_size=marker_size,
                                     alpha=alpha,
                                     split=split_var,
                                     free_axes=free_axes,
                                     height=0.5*ht*length(var))
                     p
                   })

          p <- subplot(plist, nrows=length(var))
        } else {
          if(!is.null(split_var) & length(lvls) <= 2){
              ht <- 0.75*ht
          }

          p <- feature_ly(df,
                          xcol=colnames(df)[1],
                          ycol=colnames(df)[2],
                          color=var,
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

      #################### observer for slice select buttons ###############

      slice <- controlServer('slice_ctrl',
                             reactive({ list(all=obj_info$slice_choices) }),
                             'all',
                             reactive({ NULL }),
                             default=1)

      ##################### Spatial Feature Plot ###############

      get_spatial_feature_plot <- eventReactive(c(app_object()$rds,
                                                  obj_info$filtered,
                                                  input$spatial_featureplt_do,
                                                  input$plt_do), {
        validate(
          need(input$spat_ftrplt_var != '',
               paste0(
                 '\nNo metadata variables selected!\n\n',
                 'Please select above and ',
                 'then click the button to visualize here')
               )
        )

        # get genes to plot
        var <- input$spat_ftrplt_var

        args <- config()$server$plots$spatial_featureplt

        max_var <- args$max_genes
        if(length(var) > max_var){
          showNotification(
            paste0('Feature plot supports upto ', max_var,
                   ' genes at a time. Using first ', max_var),
            type='warning'
          )
          var <- var[1:max_var]
        }

        # adjust plot height based on number of variables
        ht <- args$base_ht

        obj_type <- app_object()$obj_type
        bc <- obj_info$filtered

        # get metadata & barcode indices
        mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=T)
        idx <- mdata$rn %in% bc
        mdata <- mdata[idx, ]

        if(obj_type == 'seurat'){
          # TODO: add Xenium support
          validate(
            need('Spatial' %in% names(app_object()$rds@assays),
                 'Spatial analysis not available')
          )
        } else if(obj_type == 'anndata'){
          validate(
            need('spatial' %in% names(app_object()$rds$obsm),
                 'Spatial analysis not available')
          )
        }

        # make sure to only use slices that are present
        curr_slices <- intersect(slice(), obj_info$slice_choices)

        idx <- app_object()$spatial_coords$rn %in% bc & app_object()$spatial_coords$slice %in% curr_slices
        coords <- app_object()$spatial_coords[idx,]

        for(sl in curr_slices){
          idx <- coords$slice == sl
          coords$imagerow[idx] <- app_object()$imagerow_max[[ sl ]] - coords$imagerow[idx] + app_object()$imagerow_min[[ sl ]]
        }

        # get spatial coordinates
        coords$barcode <- coords$rn

        validate(
          need(nrow(coords) > 0, 'No cells in selected slice')
        )

        if(obj_type == 'seurat'){
          # NOTE: this is needed to only keep metadata from the selected slice
          idx <- match(coords$rn, mdata$rn)
          mdata <- mdata[idx,]

          # drop rn column from metadata to prevent duplication with coords
          mdata <- mdata[,c("rn"):=NULL]
        }

        # add gene expression data
        df <- cbind(coords, mdata)

        # add check for empty marker_size
        if(is.na(input$spat_ftrplt_marker_size)) marker_size <- 3
        else marker_size <- input$spat_ftrplt_marker_size

        alpha <- input$spat_ftrplt_marker_opacity

        # colormaps
        reversescale <- FALSE
        if(input$spat_ftrplt_colormap == 'blues'){
          colors <- 'Blues'
          reversescale <- TRUE
        } else if(input$spat_ftrplt_colormap == 'yellow-green-blue'){
          colors <- 'YlGnBu'
        } else if(input$spat_ftrplt_colormap == 'viridis'){
          colors <- 'Viridis'
        }

        # rename (eventual) axis labels
        colnames(df)[colnames(df) == 'imagecol'] <- 'spatial1'
        colnames(df)[colnames(df) == 'imagerow'] <- 'spatial2'

        # get color range & set floor
        crange <- c(min(df[[ var ]]), max(df[[ var ]]))
        if(crange[1] < 0) crange[1] <- 0

        if(length(var) > 1){
          row_view <- 'single'
        } else {
          row_view <- 'auto'
        }

        if(length(curr_slices) > 1){
          split_var <- 'slice'
          df[[ split_var ]] <- factor(df[[ split_var ]], levels=curr_slices)
        } else {
          split_var <- NULL
        }

        free_axes <- ifelse(input$spat_ftrplt_free_axes == 'yes', TRUE, FALSE)

        # arrange multi-gene views into row
        if(length(var) > 1){

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
          plist <- lapply(1:length(var), function(x){
                     if(x == 1) showscale <- TRUE
                     else showscale <- FALSE

                     if(length(curr_slices) <= 2) wd <- 0.75*ht*length(curr_slices)
                     else wd <- NULL

                     p <- feature_ly(df,
                                     xcol='spatial1',
                                     ycol='spatial2',
                                     color=var[x],
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
                                     height=0.5*ht*length(var))
                     p
                   })

          # arrange multi-gene view into multiple row
          p <- subplot(plist, nrows=length(var))
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
                          color=var,
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
                          width=wd,
                          height=ht,
                          margin=0.05)

        }

        p
      })

      output$spatial_featureplt <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        p <- get_spatial_feature_plot()

        p
      })

      #################### download buttons ####################

      downloadFileServer('summary_categorical_dload', reactive({ obj_info$summary[[ 'factor' ]] }),
                         'summary_categorical')
      downloadPlotServer('summary_numeric_dload', summary_num_plot,
                         'summary_numeric')
      downloadFileServer('cnt_tbl_dload', current_cluster_counts,
                         'cluster_counts')
      downloadPlotServer('cnt_bar_dload', get_cluster_bar,
                         'cluster_counts_barplot')
      downloadPlotServer('featureplt_dload', get_feature_plot,
                         'metadata_feature_plot')
      downloadPlotServer('spatial_featureplt_dload', get_spatial_feature_plot,
                         'metadata_spatial_feature_plot')
      downloadPlotServer('cnt_hmap_dload', get_cluster_heatmap, 'metadata_heatmap')


      #################### help buttons ####################

      helpButtonServer('cell_counts_help', size='l')
      helpButtonServer('metadata_summary_help', size='l')
      helpButtonServer('metadata_histogram_help', size='l')
      helpButtonServer('metadata_summary_table_help', size='l')
      helpButtonServer('metadata_featureplt_help', size='l')
      helpButtonServer('metadata_spatial_featureplt_help', size='l')
      helpButtonServer('metadata_heatmap_help', size='l')

    }
  ) # moduleServer
}
