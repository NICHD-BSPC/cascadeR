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
        ), # fluidRow

        fluidRow(
          column(col1, 'Aspect ratio'),
          column(col2,
            selectInput(ns('plot_aspect'),
                        label=NULL,
                        choices=c('narrow', 'wide'))
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
#' @param all_selected reactive containing list of selected points
#' @param show_selection reactive to show selection
#' @param reset_selection reactive to reset selection
#' @param reload_global reactive to trigger reload
#' @param config reactive list with config settings
#'
#' @export
#'
dimredServer <- function(id, obj,
                         filtered, args,
                         all_selected, show_selection, reset_selection,
                         reload_global, config){
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

          if(is.null(app_object()$spatial_coords)){
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

        if(app_object()$obj_type == 'seurat' && !is.null(app_object()$spatial_coords)){
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
          # for sketched umaps, mdata & cell.embeddings don't have identical
          # rows, so subset both to common rows
          if(nrow(mdata) != nrow(app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings)){
            didx <- which(rownames(app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings) %in% mdata$rn)
            midx <- which(mdata$rn %in% rownames(app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings))
            mdata <- mdata[midx,]
            df <- app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings[didx,]
          } else {
            df <- app_object()$rds@reductions[[ dimred ]]@cell.embeddings[idx,]
          }
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

        if(input$plot_aspect == 'narrow') wd <- 1.5*ht
        else wd <- NULL

        p <- umap_ly(df, xcol=xcol, ycol=ycol,
                     color=color_var,
                     colors=cols,
                     split=split_var,
                     marker_size=marker_size,
                     alpha=alpha,
                     free_axes=free_axes,
                     type='scattergl',
                     width=wd,
                     height=ht,
                     source=source)

        # save trace names
        trace_data <- plotly::plotly_build(p)$x$data
        umap_obj$df$trace_names <- unlist(lapply(trace_data, function(x) unique(x$meta)))
        plot_labeled$umap <- FALSE

        event_register(p, 'plotly_selected')
        event_register(p, 'plotly_click')

        p
      })

      output$umapplt <- renderPlotly({
        isolate({
          flag <- is.null(app_object()$rds)
        })

        validate(
          need(!flag, '')
        )
        get_umap_plot()
      })

      # proxy for the interactive umap plot
      umapProxy <- plotlyProxy('umapplt', session)

      restyle_umap_selection <- function(marker_opacity){
        if(is.null(umap_obj$df$split)){
          color_values <- umap_obj$df$data[[ umap_obj$df$color ]]
          if(is.factor(color_values)){
            color_levels <- levels(droplevels(color_values))
          } else {
            color_levels <- unique(color_values)
          }

          opacity_list <- split(marker_opacity, f=color_values, drop=TRUE)
          opacity_list <- opacity_list[as.character(color_levels)]
          opacity_list <- opacity_list[!vapply(opacity_list, is.null, logical(1))]

          trace_match <- match(umap_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        } else {
          split_var <- umap_obj$df$split

          # Split by color and split variable to match Plotly trace groups.
          opacity_list <- split(marker_opacity,
                                f=list(umap_obj$df$data[[ umap_obj$df$color ]],
                                       umap_obj$df$data[[ split_var ]]),
                                drop=TRUE)

          trace_match <- match(umap_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        }

        if(length(opacity_list) == 0) return(invisible(NULL))

        restyle_args <- list(
          'marker.opacity' = I(unname(opacity_list))
        )

        umapProxy %>%
          plotlyProxyInvoke('restyle', restyle_args, trace_idx)
      }

      # Show selection on plot using restyle
      observeEvent(show_selection(), {
        validate(
          need(!is.null(app_object()$rds) & args()$dimred != '',
               '')
        )
        validate(
          need(!is.null(umap_obj$df), '')
        )

        sel_barcodes <- unique(unlist(all_selected()))

        validate(
          need(length(sel_barcodes) > 0, '')
        )

        marker_opacity <- rep(umap_obj$df$alpha, nrow(umap_obj$df$data))

        # if not labeled, show, else hide selection
        if(!plot_labeled$umap){
          # Create a vector indicating which points are selected
          is_selected <- which(rownames(umap_obj$df$data) %in% sel_barcodes)

          if(length(is_selected) == 0){
            showNotification(
              'No selected points found in current plot (data may have changed)',
              type='warning'
            )
            return()
          }

          marker_opacity <- marker_opacity*0.05
          marker_opacity[is_selected] <- 1
        } else {
          marker_opacity <- marker_opacity*1.95
        }

        restyle_umap_selection(marker_opacity)

        # toggle
        plot_labeled$umap <- !plot_labeled$umap
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
        curr <- unique(unlist(all_selected()))

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


      observeEvent(reset_selection(), {
        if(plot_labeled$umap & !is.null(umap_obj$df)){
          marker_opacity <- rep(umap_obj$df$alpha * 1.95, nrow(umap_obj$df$data))
          restyle_umap_selection(marker_opacity)
          plot_labeled$umap <- FALSE
        }

        if(plot_labeled$spatial & !is.null(spatial_obj$df)){
          marker_opacity <- rep(spatial_obj$df$alpha * 1.95, nrow(spatial_obj$df$data))
          restyle_spatial_selection(marker_opacity)
          plot_labeled$spatial <- FALSE
        }

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
            need(!is.null(app_object()$spatial_coords),
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

          # rename (eventual) axis labels
          colnames(coords)[colnames(coords) == 'imagecol'] <- 'spatial1'
          colnames(coords)[colnames(coords) == 'imagerow'] <- 'spatial2'
          xcol <- 'spatial1'
          ycol <- 'spatial2'

          # get final set of columns to pass to umap_ly
          final_cols <- c(xcol, ycol, color, label_col)

          ht <- config()$server$plots$dimplt$base_ht*input$spat_plot_scale
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

          final_cols <- unique(final_cols)
          plot_df <- coords[, final_cols, with=FALSE]

          # save spatial object
          spatial_obj$df <- list(data=plot_df,
                                 xcol=xcol,
                                 ycol=ycol,
                                 color=color,
                                 colors=cols,
                                 label_cols=label_col,
                                 split=split_var,
                                 alpha=alpha,
                                 marker_size=marker_size,
                                 source=source,
                                 num_traces=num_traces)

          p <- umap_ly(plot_df,
                       xcol=xcol,
                       ycol=ycol,
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

          trace_data <- plotly::plotly_build(p)$x$data
          spatial_obj$df$trace_names <- unlist(lapply(trace_data, function(x) unique(x$meta)))
          plot_labeled$spatial <- FALSE

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

      restyle_spatial_selection <- function(marker_opacity){
        if(is.null(spatial_obj$df$split)){
          color_values <- spatial_obj$df$data[[ spatial_obj$df$color ]]
          if(is.factor(color_values)){
            color_levels <- levels(droplevels(color_values))
          } else {
            color_levels <- unique(color_values)
          }

          opacity_list <- split(marker_opacity, f=color_values, drop=TRUE)
          opacity_list <- opacity_list[as.character(color_levels)]
          opacity_list <- opacity_list[!vapply(opacity_list, is.null, logical(1))]

          trace_match <- match(spatial_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        } else {
          split_var <- spatial_obj$df$split

          opacity_list <- split(marker_opacity,
                                f=list(spatial_obj$df$data[[ spatial_obj$df$color ]],
                                       spatial_obj$df$data[[ split_var ]]),
                                drop=TRUE)

          trace_match <- match(spatial_obj$df$trace_names, names(opacity_list))
          trace_idx <- which(!is.na(trace_match)) - 1
          opacity_list <- opacity_list[trace_match[!is.na(trace_match)]]
          trace_idx <- as.list(trace_idx)
        }

        if(length(opacity_list) == 0) return(invisible(NULL))

        restyle_args <- list(
          'marker.opacity' = I(unname(opacity_list))
        )

        spatialProxy %>%
          plotlyProxyInvoke('restyle', restyle_args, trace_idx)
      }

      observeEvent(show_selection(), {
        validate(
          need(!is.null(app_object()$rds) & args()$dimred != '',
               '')
        )
        validate(
          need(!is.null(spatial_obj$df), '')
        )

        sel_pts <- unique(unlist(all_selected()))

        validate(
          need(length(sel_pts) > 0, '')
        )

        marker_opacity <- rep(spatial_obj$df$alpha, nrow(spatial_obj$df$data))

        if(!plot_labeled$spatial){
          is_selected <- spatial_obj$df$data$barcode %in% sel_pts
          if(!any(is_selected)){
            showNotification(
              'No selected points found in current plot',
              type='warning'
            )
            return()
          }

          marker_opacity <- marker_opacity*0.05
          marker_opacity[which(is_selected)] <- 1
        } else {
          marker_opacity <- marker_opacity*1.95
        }

        restyle_spatial_selection(marker_opacity)
        plot_labeled$spatial <- !plot_labeled$spatial

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
        data_keys <- paste(data_df[[ spatial_obj$df$xcol ]],
                           data_df[[ spatial_obj$df$ycol ]])

        new <- data_df$barcode[which(data_keys %in% keys)]
        curr <- unique(unlist(all_selected()))

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
          ll <- list()
          if(length(selected_points$umap) > 0) ll$umap <- selected_points$umap
          if(length(selected_points$spatial) > 0) ll$spatial <- selected_points$spatial

          ll
        })
      )
    } # function
  ) # moduleServer
}
