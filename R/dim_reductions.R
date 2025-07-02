#' Cell embeddings module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
dimredUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 5
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      selectInput(ns('color_var'),
                  label='Color by',
                  choices=NULL,
                  selected=NULL),

      conditionalPanel(paste0('input["', ns('dimplt_type'), '"] == "UMAP"'),

        fluidRow(
          column(col1, strong('Split by')),
          column(col2,
            selectInput(ns('umap_split_by'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL)
          ) # column
        ), # fluidRow

        bsCollapse(
          bsCollapsePanel(span(icon('gear'), 'Edit split levels'),
                          value='edit split',
            controlUI(ns('umap_split'), label=NULL)
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
          column(col1, 'Scale plot'),
          column(col2,
            sliderInput(ns('plot_scale'),
                        label=NULL,
                        value=1.0, step=0.1,
                        min=0.5, max=2, ticks=FALSE)
          ) # column
        ) # fluidRow
      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('dimplt_type'), '"] == "Spatial Plot"'),
        fluidRow(
          column(col1, strong('Interactive?')),
          column(col2,
            selectInput(ns('spatial_dimplt_switch'),
                        label=NULL,
                        choices=c('no', 'yes'),
                        selected='yes')
          ) # column
        ), # fluidRow

        conditionalPanel(paste0('input["', ns('spatial_dimplt_switch'), '"] == "yes"'),
          fluidRow(
            column(col1, 'Marker size'),
            column(col2,
              numericInput(ns('spat_marker_size'),
                           label=NULL,
                           value=3, min=1, step=1)
            ) # column
          ), # fluidRow

          fluidRow(
            column(col1, 'Marker opacity'),
            column(col2,
              sliderInput(ns('spat_marker_opacity'),
                          label=NULL,
                          value=0.5, step=0.1,
                          min=0, max=1, ticks=FALSE)
            ) # column
          ), # fluidRow

          fluidRow(
            column(col1, 'Scale plot'),
            column(col2,
              sliderInput(ns('spat_plot_scale'),
                          label=NULL,
                          value=1.0, step=0.1,
                          min=0.5, max=2, ticks=FALSE)
            ) # column
          ) # fluidRow

        ), # conditionalPanel


        conditionalPanel(paste0('input["', ns('spatial_dimplt_switch'), '"] == "no"'),


          bsCollapse(
            bsCollapsePanel(span(icon('gear'), 'Colors to show'),
                            value='edit clusters',
              controlUI(ns('spatial_grp'), label=NULL)
            ) # bsCollapsePanel
          ), # bsCollapse

          fluidRow(
            column(col1, 'Image opacity'),
            column(col2,
              sliderInput(ns('spat_img_alpha'),
                          label=NULL,
                          value=0.3, step=0.1,
                          min=0, max=1, ticks=FALSE)
            ) # column
          ), # fluidRow

          fluidRow(
            column(col1, 'Marker opacity'),
            column(col2,
              sliderInput(ns('spat_marker_opacity2'),
                          label=NULL,
                          value=0.7, step=0.1,
                          min=0, max=1, ticks=FALSE)
            ) # column
          ), # fluidRow

          fluidRow(
            column(col1, 'Marker size'),
            column(col2,
              numericInput(ns('spat_pt_scale'),
                           label=NULL,
                           value=1.6, step=0.1,
                           min=0.5, max=3)
            ) # column
          ), # fluidRow

          fluidRow(
            column(col1, 'Label colors'),
            column(col2,
              selectInput(ns('spat_label'),
                          label=NULL,
                          choices=c(TRUE, FALSE),
                          selected=TRUE)
            ) # column
          ), # fluidRow

        ) # conditionalPanel
      ), # conditionalPanel

      fluidRow(align='center',
        column(12,
          actionButton(ns('plt_do'), 'Refresh plot',
                       class='btn-primary',
                       style='margin-bottom: 10px;')
        ) # column
      ) # fluidRow

    )
  } else if(panel == 'selection'){
    tagList(
      conditionalPanel(paste0('input["', ns('dimplt_type'), '"] == "UMAP"'),
        fluidRow(
          column(6,
            strong('Point selection')
          ), # column
          column(6, align='right',
            helpButtonUI(ns('umap_ptselect_help'))
          ) # column
        ), # fluidRow

        uiOutput(ns('umap_selected')),

        fluidRow(
          column(12,
            align='center',
            style='margin-bottom: 10px;',
            actionButton(ns('show_umap_selection'),
                         label='Show selection')
          ),
          column(12,
            align='center',
            style='margin-bottom: 10px;',
            downloadButton(ns('dload_umap_clicks'),
                           label='Download selection')
          ),
          column(12,
            align='center',
            style='margin-bottom: 10px;',
            actionButton(ns('reset_umap_clicks'),
                         label='Reset selection',
                         class='btn-primary')
          )
        ) # fluidRow

      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('dimplt_type'), '"] == "Spatial Plot"'),
        fluidRow(
          column(6,
            strong('Point selection')
          ), # column
          column(6, align='right',
            helpButtonUI(ns('ptselect_help'))
          ) # column
        ), # fluidRow

        uiOutput(ns('spatial_selected')),

        fluidRow(
          column(12,
            align='center',
            style='margin-bottom: 10px;',
            actionButton(ns('show_spatial_selection'),
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

      ) # conditionalPanel
    ) # tagList

  } else if(panel == 'main'){
    tagList(
      tabsetPanel(type='tabs', id=ns('dimplt_type'),

        tabPanel('Spatial Plot',
          br(),
          fluidRow(
            column(2, 'Select slice',
                   style='font-size: 16px;', align='center'), # column
            column(3,
              controlUI(ns('slice_ctrl'), label=NULL)
            ), # column
            column(4, align='left',
              actionButton(ns('spatial_dimplt_do'), 'Generate plot',
                           class='btn-primary')
            ), # column
            column(2, align='right',
              downloadPlotUI(ns('spatial_dimplt_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('spatial_embed_help'))
            ) # column
          ), # fluidRow

          conditionalPanel(paste0('input["', ns('spatial_dimplt_switch'), '"] == "yes"'),
            div(align='center',
              withSpinner(
                plotlyOutput(ns('spatial_dimplt2'),
                              width='900px', height='750px')
              ) # withSpinner
            ) # div
          ), # conditionalPanel

          conditionalPanel(paste0('input["', ns('spatial_dimplt_switch'), '"] == "no"'),
            withSpinner(
              plotOutput(ns('spatial_dimplt1'),
                         width='100%', height='700px')
            ) # withSpinner
          ) # conditionalPanel

        ), # tabPanel

        tabPanel('UMAP',
          br(),
          fluidRow(
            column(2, align='left',
              actionButton(ns('dimplt_do'), 'Generate plot',
                           class='btn-primary')
            ), # column
            column(9, align='right',
              downloadPlotUI(ns('dimplt_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('dimred_help'))
            ) # column
          ), # fluidRow

          div(align='center',
            withSpinner(
              plotlyOutput(ns('umapplt'), height='700px')
            ) # withSpinner
          )
        ), # tabPanel

      ) # tabsetPanel
    ) # tagList
  }
}

#' Cell embeddings module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param filtered barcodes to filter object
#' @param args reactive list with global args, 'grp_by' for grouping variable
#'        and 'dimred' for which dimension reduction to use
#' @param reload_global reactive to trigger reload
#' @param config reactive list with config settings
#'
#' @export
#'
dimredServer <- function(id, obj,
                         filtered,
                         args, reload_global, config){
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
          cluster_colors=obj$cluster_colors,
          grouping_vars=obj$grouping_vars,
          spatial_coords=obj$spatial_coords,
          imagerow_max=obj$imagerow_max,
          imagerow_min=obj$imagerow_min
        )
      })

      current_barcodes <- reactiveValues(bc=NULL)

      # keep track of point selection here
      selected_points <- reactiveValues(umap=list(),
                                        spatial=list())

      global_args <- reactiveValues(grp_by=NULL)

      spatial_obj <- reactiveValues(df=NULL)
      umap_obj <- reactiveValues(df=NULL)
      flags <- reactiveValues(data_loaded=FALSE)
      plot_labeled <- reactiveValues(umap=FALSE, spatial=FALSE)
      spatial_info <- reactiveValues(col_levels=NULL, slice=NULL)

      observeEvent(app_object()$metadata, {
        # reset clicked points
        selected_points$umap <- list()
        selected_points$spatial <- list()
        spatial_obj$df <- NULL
        umap_obj$df <- NULL
        flags$data_loaded <- FALSE
        plot_labeled$umap <- FALSE
        plot_labeled$spatial <- FALSE
        spatial_info$col_levels <- NULL
        spatial_info$slice <- NULL

        validate(
          need(!is.null(app_object()$rds), '')
        )

        obj_type <- app_object()$obj_type
        mdata <- app_object()$metadata


        grouping_vars <- app_object()$grouping_vars

        updateSelectInput(session, 'umap_split_by',
                          choices=c('none', grouping_vars),
                          selected='none')

        label_cols <- grouping_vars[which(grouping_vars %in% colnames(mdata))]
        col_var <- label_cols[1]
        updateSelectInput(session, 'color_var',
                          choices=label_cols,
                          selected=col_var)

        if(obj_type == 'seurat'){

          if(!any(grepl('Spatial', names(app_object()$rds@assays))) & !any(grepl('Xenium', names(app_object()$rds@assays)))){
            hideTab(inputId='dimplt_type', target='Spatial Plot')
            updateTabsetPanel(session, 'dimplt_type', selected='UMAP')
          } else {
            showTab(inputId='dimplt_type', target='Spatial Plot')
            updateTabsetPanel(session, 'dimplt_type', selected='Spatial Plot')

            slice_choices <- intersect(unique(app_object()$spatial_coords$slice),
                                       unique(mdata$orig.ident))

            # if 'orig.ident' and slice names don't match, show everything
            if(length(slice_choices) == 0) slice_choices <- unique(app_object()$spatial_coords$slice)

            spatial_info$slice <- slice_choices

            # we only need this for SpatialDimPlots for Seurat objects
            # 1. get the barcodes
            # 2. subset metadata & extract colors
            # 3. keep colors that also show up in metadata_levels *in the same order*
            idx <- which(app_object()$spatial_coords$slice %in% slice_choices[1])
            bc <- app_object()$spatial_coords$rn[idx]
            all_cols <- unique(app_object()$metadata[bc, col_var])
            all_grps <- intersect(app_object()$metadata_levels[[ col_var ]], all_cols)
            spatial_info$col_levels <- all_grps
          }

        } else if(obj_type == 'anndata'){

          if(!'spatial' %in% names(app_object()$rds$obsm)){
            hideTab(inputId='dimplt_type', target='Spatial Plot')
            updateTabsetPanel(session, 'dimplt_type', selected='UMAP')
          } else {
            showTab(inputId='dimplt_type', target='Spatial Plot')
            updateTabsetPanel(session, 'dimplt_type', selected='Spatial Plot')

            # NOTE: anndata objects only have 1 slice
            spatial_info$slice <- 'slice'
          }

        }

        global_args$grp_by <- args()$grp_by

        flags$data_loaded <- TRUE

        showNotification(
          'Loaded embeddings ...'
        )
      }) # observeEvent

      observeEvent(reload_global(), {
        updateSelectInput(session, 'color_var',
                          selected=args()$grp_by)

        global_args$grp_by <- args()$grp_by

      })

      observeEvent(input$color_var, {
        global_args$grp_by <- input$color_var
      })

      observeEvent(c(input$color_var, slice(), current_barcodes$bc), {
        validate(
          need(!is.null(app_object()$rds), '')
        )

        if(app_object()$obj_type == 'seurat' && 'Spatial' %in% names(app_object()$rds@assays)){
          # we only need this for SpatialDimPlots for Seurat objects
          # 1. get the barcodes
          # 2. subset metadata & extract colors
          # 3. keep colors that also show up in metadata_levels *in the same order*
          idx <- which(app_object()$spatial_coords$slice %in% slice())
          bc <- intersect(app_object()$spatial_coords$rn[idx], current_barcodes$bc)
          if(length(bc) > 0){
            all_cols <- unique(app_object()$metadata[bc, input$color_var])
            all_grps <- intersect(app_object()$metadata_levels[[ input$color_var ]], all_cols)
            spatial_info$col_levels <- all_grps
          } else {
            spatial_info$col_levels <- ''
          }
        }
      })

      observeEvent(filtered(), {
        filt_bc <- filtered()
        if(is.null(current_barcodes$bc)){
          current_barcodes$bc <- filt_bc
        } else if(any(!current_barcodes$bc %in% filt_bc) | any(!filt_bc %in% current_barcodes$bc)){
          current_barcodes$bc <- filt_bc
        }
      })

      ##################### slice select controls ####################

      slice <- controlServer('slice_ctrl',
                             reactive({ list(all=spatial_info$slice) }),
                             'all',
                             reactive({ NULL }),
                             default=1)

      ##################### edit split levels ####################

      umap_split <- controlServer('umap_split',
                                  reactive({ app_object()$metadata_levels }),
                                  reactive({ input$umap_split_by }),
                                  reactive({ NULL }))

      ##################### UMAP plot ########################

      get_umap_plot <- eventReactive(c(current_barcodes$bc,
                                       input$dimplt_do,
                                       input$plt_do,
                                       reload_global()), {
        validate(
          need(!is.null(app_object()$rds) & args()$dimred != '',
               '')
        )

        dimred <- args()$dimred
        obj_type <- app_object()$obj_type
        if(obj_type == 'seurat'){
          all_dimred <- names(app_object()$rds@reductions)
        } else if(obj_type == 'anndata'){
          all_dimred <- names(app_object()$rds$obsm)
          all_dimred <- setdiff(all_dimred, 'spatial')
        }

        validate(
          need(length(all_dimred) > 0, 'No UMAP embeddings found in object!')
        )

        validate(
          need(dimred %in% all_dimred, '')
        )

        if(input$umap_split_by == 'none') split_var <- NULL
        else split_var <- input$umap_split_by

        bc <- current_barcodes$bc

        # filter to current barcodes
        mdata <- app_object()$metadata
        idx <- which(rownames(mdata) %in% bc)

        mdata <- data.table::as.data.table(mdata, keep.rownames=T)
        mdata <- mdata[idx,]
        mdata <- as.data.frame(mdata)
        rownames(mdata) <- mdata$rn

        # get dimred coordinates
        if(obj_type == 'seurat'){
          df <- app_object()$rds@reductions[[ dimred ]]@cell.embeddings
          df <- df[idx,]
        } else if(obj_type == 'anndata'){
          df <- app_object()$rds$obsm[[ dimred ]]
          df <- df[idx, 1:2]

          label <- sub('X_', '', dimred)
          colnames(df) <- paste0(label, 1:2)

        }

        df <- as.data.frame(df)
        rownames(df) <- rownames(mdata)
        if(nrow(df) >= 100000){
          showNotification(
            'Plotting a large number of cells! This could take a while ...',
            type='warning'
          )
        }

        xcol <- colnames(df)[1]
        ycol <- colnames(df)[2]

        args <- config()$server$plots$dimplt
        color_var <- global_args$grp_by

        # add color column
        df <- cbind(df, mdata[[ color_var ]])
        colnames(df)[ncol(df)] <- color_var
        num_cols <-length(unique(df[[ color_var ]]))

        # get global color mapping
        cols <- app_object()$cluster_colors[[ color_var ]]

        # add split column, if needed
        if(!is.null(split_var)){
          df <- cbind(df, mdata[[ split_var ]])
          colnames(df)[ncol(df)] <- split_var

          # subset to split levels that are selected
          if(length(umap_split()) > config()$server$max_split_levels){
            showNotification(
              "Many levels in splitting variable. This can take a while ... ",
              type='warning'
            )
          } else if(length(umap_split()) == 0){
            showNotification(
              "No levels selected in splitting variable! Must choose at least 1 ... ",
              type='error'
            )
            validate(
              need(length(umap_split()) > 0, "No levels selected in splitting variable! Must choose at least 1")
            )
          }

          df <- df[df[[ split_var ]] %in% umap_split(), ]
          df[[ split_var ]] <- factor(df[[ split_var ]], levels=umap_split())

          num_split <- length(unique(df[[ split_var ]]))
          num_traces <- num_cols*num_split
        } else {
          num_traces <- num_cols
        }

        # add check for empty marker size
        if(is.na(input$marker_size)) marker_size <- 2
        else marker_size <- input$marker_size

        alpha <- input$marker_opacity

        free_axes <- ifelse(input$free_axes == 'yes', TRUE, FALSE)

        source <- 'umaply'

        # save plotted data
        umap_obj$df <- list(data=df,
                            xcol=xcol,
                            ycol=ycol,
                            color=color_var,
                            colors=cols,
                            split=split_var,
                            marker_size=marker_size,
                            alpha=alpha,
                            free_axes=free_axes,
                            source=source,
                            num_traces=num_traces)

        ht <- config()$server$plots$dimplt$base_ht*input$plot_scale
        if(!is.null(split_var) & length(umap_split()) == 2){
          ht <- 0.6*ht
        }

        p <- umap_ly(df, xcol=xcol, ycol=ycol,
                     color=color_var,
                     colors=cols,
                     split=split_var,
                     marker_size=marker_size,
                     alpha=alpha,
                     free_axes=free_axes,
                     type='scattergl',
                     height=ht,
                     source=source)

        event_register(p, 'plotly_selected')
        event_register(p, 'plotly_click')

        p
      })

      output$umapplt <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        get_umap_plot()
      })

      # proxy for the interactive umap plot
      umapProxy <- plotlyProxy('umapplt', session)

      observeEvent(c(selected_points$umap,
                     input$show_umap_selection), {

        validate(
          need(!is.null(app_object()$rds) & args()$dimred != '',
               '')
        )

        isolate({
          split_var <- input$umap_split_by
        })

        sel_pts <- unique(c(selected_points$umap,
                            selected_points$spatial))
        if(split_var == 'none'){
          if(length(sel_pts) > 0){
            new_trace <- get_label_trace(umap_obj$df,
                                         sel_pts)

            num_traces <- umap_obj$df$num_traces

            # remove last trace
            # NOTE: this is 0-based indexed
            if(plot_labeled$umap){
              umapProxy %>%
                plotlyProxyInvoke('deleteTraces', num_traces)
            }

            umapProxy %>%
              plotlyProxyInvoke('addTraces', new_trace)

            plot_labeled$umap <- TRUE
          } else if(plot_labeled$umap){
            num_traces <- umap_obj$df$num_traces
            umapProxy %>%
              plotlyProxyInvoke('deleteTraces', num_traces)
            plot_labeled$umap <- FALSE
          }

        #} else if(length(sel_pts) > 0){
        #  showNotification(
        #    'Warning: Cannot show selected points in split view',
        #    type='warning'
        #  )
        }

        # TODO: implement showing selection for split plot
        # - add empty 'selected' trace to each subplot?
        #   Then we could use append/prepend traces
        # - alternatively, restyle
        #} else {
        #  if(length(selected_points$umap) > 0){

        #    new_trace <- get_label_trace(umap_obj$df,
        #                                 selected_points$umap,
        #                                 split=TRUE)

        #    num_traces <- umap_obj$df$num_traces

        #    lvls <- unique(umap_obj$df$data[, umap_obj$df$split ])

        #    # remove all label traces
        #    # NOTE: this is 0-based indexed
        #    if(plot_labeled$umap){
        #      for(i in 1:length(lvls)){
        #        umapProxy %>%
        #          plotlyProxyInvoke('deleteTraces', num_traces)
        #      }
        #    }

        #    for(i in 1:length(new_trace)){
        #      tmp <- new_trace[[i]]
        #      print(tmp$pos)
        #      print(str(tmp$trace))
        #      umapProxy %>%
        #        plotlyProxyInvoke('addTraces',
        #                          tmp$trace)
        #                          #-1)
        #                          #tmp$pos)
        #                          ##tmp$pos[2])
        #    }
        #    plot_labeled$umap <- TRUE

        #  }
        #}

      })

      ##################### UMAP selection #########################

      get_umap_selected <- reactive({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        req(umap_obj$df)
        event_data('plotly_selected', source=umap_obj$df$source)
      })

      observeEvent(get_umap_selected(), {
        validate(
          need(!is.null(app_object()$rds) & flags$data_loaded, '')
        )
        df <- get_umap_selected()

        data_df <- umap_obj$df$data
        xcol <- umap_obj$df$xcol
        ycol <- umap_obj$df$ycol

        # get points by matching coords & key
        keys <- paste(df$x, df$y)
        data_keys <- paste(data_df[, xcol], data_df[, ycol])

        new <- rownames(data_df)[data_keys %in% keys]
        curr <- unique(unlist(selected_points$umap))

        # only add new points
        if(!all(new %in% curr)){
          new_idx <- which(!new %in% curr)
          showNotification(
              paste0('Adding ', length(new_idx), ' points to selection')
          )

          selected_points$umap[[ length(selected_points$umap) + 1 ]] <- new[new_idx]
        } else if(length(new) > 0){
          showNotification(
              paste0('All selected points already in selection'),
              type='warning'
          )
        }
      })

      output$umap_selected <- renderUI({
        np <- length(unique(unlist(c(selected_points$spatial,
                                     selected_points$umap))))

        tagList(
          fluidRow(
            column(12, style='margin-bottom: 10px;',

              paste(np, 'points selected')
            )
          )
        )
      })

      output$dload_umap_clicks <- downloadHandler(
        filename = function(){
          paste0('clicked-points.tsv')
        },
        content = function(file){
          bc <- unique(unlist(c(selected_points$umap,
                                selected_points$spatial)))

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

      observeEvent(input$reset_umap_clicks, {
        np <- length(unique(unlist(c(selected_points$umap,
                                     selected_points$spatial))))
        showNotification(
            paste0('Clearing ', np,
                   ' points from selection')
        )
        selected_points$umap <- list()
        selected_points$spatial <- list()
      })

      ##################### Spatial Dimplot ########################

      spatial_grp <- controlServer('spatial_grp',
                                   reactive({ app_object()$metadata_levels }),
                                   reactive({ input$color_var }),
                                   reactive({ spatial_info$col_levels }))

      get_spatial_dimplot <- eventReactive(c(app_object()$rds,
                                             current_barcodes$bc,
                                             input$spatial_dimplt_do,
                                             input$plt_do,
                                             reload_global()), {
        validate(
          need(!is.null(app_object()$rds), '')
        )

        obj_type <- app_object()$obj_type
        bc <- current_barcodes$bc

        if(obj_type == 'seurat'){
          validate(
            need(any(grepl('Spatial', names(app_object()$rds@assays))) |
                 any(grepl('Xenium', names(app_object()$rds@assays))),
                 'Spatial analysis not available')
          )
        } else if(obj_type == 'anndata'){
          validate(
            need('spatial' %in% names(app_object()$rds$obsm),
                 'Spatial analysis not available')
          )
        }

        obj_type <- app_object()$obj_type

        # filter
        idx <- which(rownames(app_object()$metadata) %in% bc)

        mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=T)
        mdata <- mdata[idx,]
        mdata <- as.data.frame(mdata)
        rownames(mdata) <- mdata$rn

        idx <- app_object()$spatial_coords$rn %in% bc & app_object()$spatial_coords$slice %in% slice()
        coords <- app_object()$spatial_coords[idx,]

        if(obj_type == 'seurat'){

          validate(
            need(all(slice() %in% unique(app_object()$spatial_coords$slice)),
                 'Selected slice(s) not present in filtered object')
          )

        }

        color_var <- global_args$grp_by

        if(obj_type == 'seurat'){

          # get cell ID matches
          idx <- match(coords$rn, rownames(mdata))

          # add coloring info
          if(color_var != '') coords[[ color_var ]] <- mdata[idx, color_var]

        } else if(obj_type == 'anndata'){

          if(color_var != '') coords[[ color_var ]] <- mdata[[ color_var ]]
        }

        for(sl in slice()){
          idx <- coords$slice == sl
          coords$imagerow[idx] <- app_object()$imagerow_max[[ sl ]] - coords$imagerow[idx] + app_object()$imagerow_min[[ sl ]]
        }

        # rename 'rn' column as 'barcode'
        rn_idx <- colnames(coords) == 'rn'
        colnames(coords)[rn_idx] <- 'barcode'

        validate(
          need(nrow(coords) > 0, 'No cells in selected slice(s)')
        )

        if(input$spatial_dimplt_switch == 'yes'){

          alpha <- input$spat_marker_opacity

          # add check for empty marker size
          if(is.na(input$spat_marker_size)) marker_size <- 3
          else marker_size <- input$spat_marker_size

          # save spatial object
          xcol <- 'imagecol'
          ycol <- 'imagerow'
          color <- color_var
          cols <- app_object()$cluster_colors[[ color_var ]]
          label_col <- 'barcode'
          marker_size <- marker_size
          alpha <- alpha
          source <- 'dimplt'

          # if no coloring variables found then add dummy column and color gray
          if(color == '' | is.null(cols)){
            coords[['color']] <- 'all'
            color <- 'color'
            cols <- 'gray'
            num_traces <- 1
          } else {
            num_traces <- length(unique(coords[[ color_var ]]))
          }

          spatial_obj$df <- list(data=coords,
                                 xcol=xcol,
                                 ycol=ycol,
                                 color=color,
                                 colors=cols,
                                 label_cols=label_col,
                                 alpha=alpha,
                                 marker_size=marker_size,
                                 source=source,
                                 num_traces=num_traces)

          # rename (eventual) axis labels
          colnames(coords)[colnames(coords) == 'imagecol'] <- 'spatial1'
          colnames(coords)[colnames(coords) == 'imagerow'] <- 'spatial2'

          # get final set of columns to pass to umap_ly
          final_cols <- c('spatial1', 'spatial2', color)

          ht <- config()$server$plots$dimplt$base_ht*input$plot_scale
          wd <- 1.25*ht
          if(length(slice()) == 2){
            ht <- 0.75*ht
          }

          if(length(slice()) > 1){
            split_var <- 'slice'
            coords[[ split_var ]] <- factor(coords[[ split_var ]], levels=slice())
            final_cols <- c(final_cols, 'slice')
          } else {
            split_var <- NULL
          }

          p <- umap_ly(coords[, final_cols, with=FALSE],
                       xcol='spatial1',
                       ycol='spatial2',
                       color=color,
                       colors=cols,
                       split=split_var,
                       type='scattergl',
                       alpha=alpha,
                       marker_size=marker_size,
                       showticklabels=FALSE,
                       free_axes=TRUE,
                       width=wd,
                       height=ht,
                       source=source)

        event_register(p, 'plotly_selected')
        event_register(p, 'plotly_click')

        } else if(input$spatial_dimplt_switch == 'no'){
          validate(
            need(app_object()$obj_type == 'seurat' &
                 !any(grepl('Xenium', names(app_object()$rds))),
                 'Non-interactive spatial plot only supported for Seurat Visium objects')
          )

          cols <- app_object()$cluster_colors[[ color_var ]]

          obj <- subset(app_object()$rds, cells=coords$barcode)
          if(input$color_var != 'idents'){
              Idents(obj) <- obj@meta.data[, color_var ]
          }

          # subset again to keep specific idents
          lidx <- spatial_grp() %in% obj@meta.data[[ color_var ]]

          if(length(which(lidx)) == 0){
            validate(
              need(length(lidx) > 0,
                   paste0('\nNo selected groups present in current slices!\n\n',
                          'Try choosing different groups and/or slices and generate plot')
              )
            )
          }

          # subset grp lvls & object
          grp_lvls <- spatial_grp()[which(lidx)]
          obj <- subset(obj, idents=grp_lvls)

          missing_img <- setdiff(slice(), names(obj@images))
          if(length(missing_img) > 0){
            validate(
              need(length(missing_img) == 0,
                   paste0('\n\nNo selected groups present in some slices: ',
                          paste(missing_img, collapse=','), '\n\n',
                          'Try choosing dfferent slices and regenerate plot'))
            )
          }

          if(length(slice()) <= 2) ncol <- length(slice())
          else {
            ncol <- round(sqrt(length(slice())))
          }

          alpha <- input$spat_marker_opacity2

          # make sure image alpha is within [0, 1]
          if(is.na(input$spat_img_alpha) | input$spat_img_alpha < 0)
            img_alpha <- 0.1
          else if(input$spat_img_alpha > 1) img_alpha <- 1
          else img_alpha <- input$spat_img_alpha

          p <- SpatialDimPlot(object=obj,
                              images=slice(),
                              group.by=color_var,
                              cols=cols,
                              ncol=ncol,
                              crop=FALSE,
                              label=as.logical(input$spat_label),
                              pt.size.factor=input$spat_pt_scale,
                              label.size=3,
                              alpha=alpha,
                              image.alpha=img_alpha,
                              stroke=0.3,
                              repel=TRUE)
        }

        p
      })

      output$spatial_dimplt2 <- renderPlotly({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        get_spatial_dimplot()
      })

      output$spatial_dimplt1 <- renderPlot({
        validate(
          need(!is.null(app_object()$rds), '')
        )

        isolate({
          obj_type <- app_object()$obj_type
        })

        validate(
          need(obj_type == 'seurat',
               'Non-interactive plot only supported for Seurat objects')
        )

        p <- get_spatial_dimplot()

        p

      })

      # proxy for the interactive spatial plot
      spatialProxy <- plotlyProxy('spatial_dimplt2', session)

      observeEvent(c(selected_points$spatial,
                     input$show_spatial_selection), {
        validate(
          need(!is.null(app_object()$rds) & args()$dimred != '',
               '')
        )

        sel_pts <- unique(c(selected_points$spatial,
                            selected_points$umap))
        # show selected points only if single slice is selected
        if(length(slice()) == 1){
          if(length(sel_pts) > 0){
            new_trace <- get_label_trace(spatial_obj$df,
                                         sel_pts)

            num_traces <- spatial_obj$df$num_traces

            # remove label trace
            # NOTE: this uses 0-based indexing
            if(plot_labeled$spatial){
              spatialProxy %>%
                plotlyProxyInvoke('deleteTraces',
                                  num_traces)
            }

            spatialProxy %>%
              plotlyProxyInvoke('addTraces',
                                new_trace)

            plot_labeled$spatial <- TRUE

          } else if(plot_labeled$spatial){
            num_traces <- spatial_obj$df$num_traces
            spatialProxy %>%
              plotlyProxyInvoke('deleteTraces', num_traces)
            plot_labeled$spatial <- FALSE
          }
        #} else if(length(sel_pts) > 0){
        #  showNotification(
        #    'Warning: Cannot show selected points in multi-slice view',
        #    type='warning'
        #  )
        }

      })


      ################### Spatial selection #############################

      # reactives to obtain plotly click/select/double click data
      get_clicks <- reactive({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        req(spatial_obj$df)
        event_data('plotly_click', source=spatial_obj$df$source)
      })

      get_selection <- reactive({
        validate(
          need(!is.null(app_object()$rds), '')
        )
        req(spatial_obj$df)
        event_data('plotly_selected', source=spatial_obj$df$source)
      })

      observeEvent(c(get_clicks(), get_selection()), {
        validate(
          need(!is.null(app_object()$rds) & flags$data_loaded, '')
        )

        clk <- get_clicks()
        sel <- get_selection()

        if(is.null(clk) | is.null(sel)){
          df <- rbind(clk, sel)
        } else {
          # get shared columns before rbind
          # NOTE: this is needed because selections from single view
          #       have 'key' column, but selections from split view don't
          shared_cols <- intersect(colnames(clk), colnames(sel))

          # data frame with current selections
          df <- rbind(clk[, shared_cols], sel[, shared_cols])
        }

        # current data
        data_df <- spatial_obj$df$data

        # all points
        keys <- paste(df$x, df$y)
        data_keys <- paste(data_df$imagecol, data_df$imagerow)

        new <- data_df$barcode[which(data_keys %in% keys)]
        curr <- unique(unlist(selected_points$spatial))

        # only add new points
        if(!all(new %in% curr)){
          new_idx <- which(!new %in% curr)
          showNotification(
              paste0('Adding ', length(new_idx), ' points to selection')
          )

          selected_points$spatial[[ length(selected_points$spatial) + 1 ]] <- new[new_idx]
        } else if(length(new) > 0){
          showNotification(
              paste0('All selected points already in selection'),
              type='warning'
          )
        }

      })

      output$spatial_selected <- renderUI({
        np <- length(unique(unlist(c(selected_points$spatial,
                                     selected_points$umap))))

        tagList(
          fluidRow(
            column(12, style='margin-bottom: 10px;',

              paste(np, 'points selected')
            )
          )
        )

      })

      output$dload_clicks <- downloadHandler(
        filename = function(){
          paste0('clicked-points-spatial.tsv')
        },
        content = function(file){
          bc <- unique(unlist(c(selected_points$umap,
                                selected_points$spatial)))

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

      observeEvent(input$reset_clicks, {
        np <- length(unique(unlist(c(selected_points$spatial,
                                     selected_points$umap))))
        showNotification(
            paste0('Clearing ', np,
                   ' points from selection')
        )
        selected_points$spatial <- list()
        selected_points$umap <- list()
      })

      ######################### Help ####################

      helpButtonServer('dimred_help', size='l')
      helpButtonServer('spatial_embed_help', size='l')
      helpButtonServer('umap_ptselect_help', size='l')
      helpButtonServer('ptselect_help', size='l')

      downloadPlotServer('spatial_dimplt_dload', get_spatial_dimplot,
                         'spatial_dimplot')
      downloadPlotServer('dimplt_dload', get_umap_plot,
                         'dimplot')

      return(
        reactive({
          list(
            umap=unique(unlist(selected_points$umap)),
            spatial=unique(unlist(selected_points$spatial))
          )
        })
      )
    } # function
  ) # moduleServer
}
