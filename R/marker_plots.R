#' Marker plots module ui
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
markerPlotUI <- function(id, panel){
  ns <- NS(id)

  # set column width of menu title (col1) & input (col2) in fluidRow()
  col1 <- 6
  col2 <- 12 - col1

  if(panel == 'sidebar'){
    tagList(
      fluidRow(
        column(12, align='right',
          helpButtonUI(ns('mrkrplt_help'))
        ) # column
      ), # fluidRow

      div(id=ns('assay_menu'),
        fluidRow(
          column(col1, strong('Assay')),
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

      conditionalPanel(
        paste0('( input["', ns('markerplt_type'), '"] == "Feature Plot" & ',
               'input["', ns('ftrplt_type'), '"] == "Spatial" ) | ',
               '( input["', ns('markerplt_type'), '"] == "Coexpression Plot" & ',
               'input["', ns('coexplt_type'), '"] == "Spatial" )'),
        controlUI(ns('slice_ctrl'), label='Select slice')
      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('markerplt_type'), '"] == "Violin Plot"'),

        violinUI(ns('violin'), panel='sidebar')

      ), #conditionalPanel

      conditionalPanel(paste0('input["', ns('markerplt_type'), '"] == "DotPlot"'),

        dotPlotUI(ns('dotplot'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('markerplt_type'), '"] == "Feature Plot" & ',
               'input["', ns('ftrplt_type'), '"] == "UMAP"'),

        featurePlotUI(ns('featureplot'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('markerplt_type'), '"] == "Coexpression Plot" & ',
               'input["', ns('coexplt_type'), '"] == "UMAP"'),

        coexpressionPlotUI(ns('coexpression_plot'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('markerplt_type'), '"] == "Feature Plot" & ',
               'input["', ns('ftrplt_type'), '"] == "Spatial"'),

        spatialFeaturePlotUI(ns('spatial_featureplot'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('markerplt_type'), '"] == "Coexpression Plot" & ',
               'input["', ns('coexplt_type'), '"] == "Spatial"'),

        spatialCoexpressionPlotUI(ns('spatial_coexpression_plot'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('markerplt_type'), '"] == "Gene-gene Scatter"'),

        scatterPlotUI(ns('scatter'), panel='sidebar')

      ), # conditionalPanel

      conditionalPanel(paste0('input["', ns('markerplt_type'), '"] == "Line Plot"'),

        linePlotUI(ns('lineplt'), panel='sidebar')

      ), # conditionalPanel

      fluidRow(align='center',
        column(12,
          actionButton(ns('plt_do'), 'Refresh plot',
                       class='btn-primary',
                       style='margin-bottom: 10px;')
        ) # column
      ) # fluidRow
    ) # tagList
  } else if(panel == 'main'){
    tagList(
      tabsetPanel(type='tabs', id=ns('markerplt_type'),

        dotPlotUI(ns('dotplot'), panel='main'),

        violinUI(ns('violin'), panel='main'),

        tabPanel('Feature Plot',
          tabsetPanel(type='tabs', id=ns('ftrplt_type'),

            spatialFeaturePlotUI(ns('spatial_featureplot'), panel='main'),

            featurePlotUI(ns('featureplot'), panel='main')

          ) # tabsetPanel
        ), # tabPanel

        tabPanel('Coexpression Plot',
          tabsetPanel(type='tabs', id=ns('coexplt_type'),

            spatialCoexpressionPlotUI(ns('spatial_coexpression_plot'), panel='main'),

            coexpressionPlotUI(ns('coexpression_plot'), panel='main')

          ) # tabsetPanel

        ), # tabPanel

        scatterPlotUI(ns('scatter'), panel='main'),

        linePlotUI(ns('lineplt'), panel='main')

      ) # tabsetPanel
    ) # tagList
  }

} # markerPlotUI

#' Marker plots module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param filtered barcodes to filter object
#' @param genes_to_plot reactive list with genes in scratchpad
#' @param args reactive list with elements: 'assay' for selected assay,
#'        'dimred' for which dimension reduction to use and
#'        'grp_by' for grouping variable
#' @param gene_choices reactive list with all genes present in object
#' @param reload_global reactive to trigger reload
#' @param config reactive list with config settings
#'
#' @export
#'
markerPlotServer <- function(id, obj, filtered, genes_to_plot,
                             args, gene_choices, reload_global, config){
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

      obj_info <- reactiveValues(slot=NULL,
                                 filtered=NULL,
                                 metadata_levels=list(),
                                 gene_choices=NULL,
                                 slice_choices=NULL)

      observeEvent(app_object()$rds, {

        obj_type <- app_object()$obj_type
        if(obj_type == 'seurat'){

          shinyjs::show(id='assay_menu')

        } else if(obj_type == 'anndata'){
          shinyjs::hide(id='assay_menu')

        }
      })

      # read & save filtered object data
      observeEvent(filtered(), {
        # reset data on (re)load
        obj_info$slot <- NULL
        obj_info$filtered <- NULL
        obj_info$slice_choices <- NULL
        obj_info$gene_choices <- NULL
        obj_info$metadata_levels <- app_object()$metadata_levels

        obj_info$filtered <- filtered()

        if(app_object()$obj_type == 'seurat'){

        if(!any(grepl('Spatial', names(app_object()$rds@assays))) & !any(grepl('Xenium', names(app_object()$rds)))){
          hideTab(inputId='ftrplt_type', target='Spatial')
          hideTab(inputId='coexplt_type', target='Spatial')

          sel_plt <- input$markerplt_type
          # make sure something is selected
          updateTabsetPanel(session, inputId='markerplt_type',
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
                                     unique(app_object()$metadata$orig.ident))

          # if 'orig.ident' and slice choices don't match, show everything
          if(length(slice_choices) == 0) slice_choices <- unique(app_object()$spatial_coords$slice)

          obj_info$slice_choices <- slice_choices
        }

        # update assay menu
        assay_names <- names(app_object()$rds@assays)

        # prioritize SCT
        if('SCT' %in% assay_names) assay_names <- c('SCT', setdiff(assay_names, 'SCT'))

        # Here we check if all assays contain 'data' and/or 'scale.data' slots.
        # If not, the assay is dropped
        final_assays <- assay_names
        assay_list <- list()
        for(selected in assay_names){
          # get slot depending on assay selected
          # this can either be 'data' or 'scale.data'
          # - if 'scale.data' is present, this is used
          # TODO: move to config as, e.g. 'default_assay'
          if(inherits(app_object()$rds@assays[[ selected ]], 'Assay5')){
            slot_names <- names(app_object()$rds@assays[[ selected ]]@layers)
          } else {
            slot_names <- slotNames(app_object()$rds@assays[[ selected ]])
          }

          slot_names <- intersect(c('data', 'scale.data', 'counts'), slot_names)
          if(length(slot_names) == 0){
            final_assays <- setdiff(final_assays, selected)
          } else {
            assay_list[[ selected ]] <- slot_names
          }
        }

        # update with selection and exit
        updateSelectInput(session, 'assay',
                          choices=final_assays,
                          selected=final_assays[1])

        # show notification about dropped assays
        if(length(final_assays) < length(assay_names)){
          dropped <- setdiff(assay_names, final_assays)
          showNotification(
            paste('Some assays did not have "data", "scale.data" or "counts" slots and will be dropped:',
                  paste(dropped, collapse=', ')),
            type='warning'
          )
        }


        # save everything

        obj_info$assay_list <- assay_list
        obj_info$slot <- slot_names[1]

        } else if(app_object()$obj_type == 'anndata'){
          obj_info$slot <- 'none'

          if(!'spatial' %in% names(app_object()$rds$obsm)){
            hideTab(inputId='ftrplt_type', target='Spatial')
            hideTab(inputId='coexplt_type', target='Spatial')

            sel_plt <- input$markerplt_type
            # make sure something is selected
            updateTabsetPanel(session, inputId='markerplt_type',
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

            # NOTE: we only support a single slice in an anndata object
            obj_info$slice_choices <- 'slice'

          }

          updateSelectInput(session, 'assay',
                            choices=NULL,
                            selected=NULL)
        }

        showNotification(
          'Loaded marker plots ...'
        )
      })

      # update slot input when assay is updated
      observeEvent(input$assay, {
        req(app_object()$rds)

        slot_names <- obj_info$assay_list[[ input$assay ]]

        updateSelectInput(session, 'slot',
                          choices=slot_names,
                          selected=slot_names[1])

        # save everything
        obj_info$slot <- slot_names[1]
      })

      observeEvent(reload_global(), {
        updateSelectInput(session, 'assay',
                          selected=args()$assay)

      })


      ##################### Violin Plot ########################

      violinServer('violin',
                    app_object,
                    reactive({ obj_info$filtered }),
                    genes_to_plot,
                    reactive({ list(grp_by=args()$grp_by, assay=input$assay, slot=obj_info$slot) }),
                    gene_choices,
                    reload_global,
                    reactive({ input$plt_do }),
                    config)

      ##################### Dot plot ########################

      dotPlotServer('dotplot',
                    app_object,
                    reactive({ obj_info$filtered }),
                    genes_to_plot,
                    reactive({ list(grp_by=args()$grp_by, assay=input$assay, slot=obj_info$slot) }),
                    gene_choices,
                    reload_global,
                    reactive({ input$plt_do }),
                    config)

      ##################### Feature Plot ########################

      featurePlotServer('featureplot',
                        app_object,
                        reactive({ obj_info$filtered }),
                        genes_to_plot,
                        reactive({ list(assay=input$assay, slot=obj_info$slot, dimred=args()$dimred) }),
                        gene_choices,
                        reload_global,
                        reactive({ input$plt_do }),
                        config)

      ##################### Coexpression Plot ########################

      coexpressionPlotServer('coexpression_plot',
                             app_object,
                             reactive({ obj_info$filtered }),
                             genes_to_plot,
                             reactive({ list(assay=input$assay, slot=obj_info$slot, dimred=args()$dimred) }),
                             gene_choices,
                             reload_global,
                             reactive({ input$plt_do }),
                             config)

      #################### observer for slice select buttons ###############

      slice <- controlServer('slice_ctrl',
                             reactive({ list(all=obj_info$slice_choices) }),
                             'all',
                             reactive({ NULL }),
                             default=1)

      ##################### Spatial Feature Plot ###############

      spatialFeaturePlotServer('spatial_featureplot',
                               app_object,
                               reactive({ obj_info$filtered }),
                               genes_to_plot,
                               reactive({ list(assay=input$assay, slot=obj_info$slot, dimred=args()$dimred) }),
                               gene_choices,
                               reactive({ intersect(slice(), obj_info$slice_choices) }),
                               reload_global,
                               reactive({ input$plt_do }),
                               config)

      ##################### Spatial Coexpression Plot ########################

      spatialCoexpressionPlotServer('spatial_coexpression_plot',
                                    app_object,
                                    reactive({ obj_info$filtered }),
                                    genes_to_plot,
                                    reactive({ list(assay=input$assay, slot=obj_info$slot, dimred=args()$dimred) }),
                                    gene_choices,
                                    reactive({ intersect(slice(), obj_info$slice_choices) }),
                                    reload_global,
                                    reactive({ input$plt_do }),
                                    config)

      ##################### Gene-gene scatter ########################

      scatterPlotServer('scatter',
                        app_object,
                        reactive({ obj_info$filtered }),
                        genes_to_plot,
                        reactive({ list(grp_by=args()$grp_by, assay=input$assay, slot=obj_info$slot, assay_list=obj_info$assay_list) }),
                        gene_choices,
                        reload_global,
                        reactive({ input$plt_do }),
                        config)

      ##################### Line plot ########################

      linePlotServer('lineplt',
                     app_object,
                     reactive({ obj_info$filtered }),
                     genes_to_plot,
                     reactive({ list(grp_by=args()$grp_by, assay=input$assay, slot=obj_info$slot) }),
                     gene_choices,
                     reload_global,
                     reactive({ input$plt_do }),
                     config)

      #################### Help buttons ####################

      helpButtonServer('mrkrplt_help', size='l')

    } # function
  ) # moduleServer
} # markerPlotServer

# Internal function to return plot data
#
# - this returns a data frame with metadata, gene data
#   and reduction (if reduction=TRUE)
#
get_marker_plot_data <- function(g, app_object, filtered, args,
                                 reduction=FALSE, slice=NULL){
  obj_type <- app_object()$obj_type
  mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=TRUE)
  assay <- args()$assay
  selected_slot <- args()$slot

  # get spatial coords if slice is specified
  if(!is.null(slice)){
    idx <- app_object()$spatial_coords$rn %in% filtered & app_object()$spatial_coords$slice %in% slice
    coords <- app_object()$spatial_coords[idx,]

    for(sl in slice){
      idx <- coords$slice == sl
      coords$imagerow[idx] <- app_object()$imagerow_max[[ sl ]] - coords$imagerow[idx] + app_object()$imagerow_min[[ sl ]]
    }

    # make 'barcode' column from rownames
    coords$barcode <- coords$rn

    if(obj_type == 'seurat'){
      idx <- match(coords$rn, mdata$rn)
      mdata <- mdata[idx,]

      # drop rn column from metadata to prevent duplication with coords
      mdata <- mdata[,c("rn"):=NULL]
    }
  } else {
    idx <- mdata$rn %in% filtered
    mdata <- mdata[idx,]
  }

  df <- as.data.frame(mdata)

  # extract gene data
  if(obj_type == 'seurat'){

    if(inherits(app_object()$rds@assays[[ assay ]], 'Assay5')){
      all_g <- rownames(app_object()$rds@assays[[ assay ]]@features)
    } else {
      all_g <- rownames(app_object()$rds@assays[[ assay ]])
    }

    validate(
      need(all(g %in% all_g),
           paste0('Input genes (',
                  paste(setdiff(g, all_g), collapse=', '),
                  ') not found in "',
                  assay,
                  '" assay. Please remove or choose different assay and retry')
      )
    )
    if(inherits(app_object()$rds@assays[[ assay ]], 'Assay5')){
      ridx <- which(all_g %in% g)

      # get cell indices
      # - filter using coords if slice is specified
      if(is.null(slice)) cidx <- which(rownames(app_object()$rds@assays[[ assay ]]@cells) %in% filtered)
      else cidx <- which(rownames(app_object()$rds@assays[[ assay ]]@cells) %in% coords$rn)

      gdat <- app_object()$rds@assays[[ assay ]]@layers[[ selected_slot ]][ridx, cidx]

      if(length(ridx) == 1) gdat <- matrix(gdat, nrow=1, byrow=TRUE)
      else gdat <- as.matrix(gdat)

      rownames(gdat) <- all_g[ridx]

      gdat <- t(gdat)

      # order by gene
      gdat <- gdat[,g]
    } else {
      # get cell indices
      # - filter using coords if slice is specified
      if(is.null(slice)) cidx <- filtered
      else cidx <- coords$rn
      gdat <- slot(app_object()$rds@assays[[ assay ]],
                   selected_slot)[g, cidx]

      gdat <- as.matrix(gdat)

      if(length(g) > 1) gdat <- t(gdat)
    }

    if(reduction){
      dimred <- app_object()$rds@reductions[[ args()$dimred ]]@cell.embeddings[idx, ]
    }
  } else if(obj_type == 'anndata'){
    gdat <- app_object()$rds$X[idx, g]
    gdat <- as.matrix(gdat)

    if(reduction){
      dimred <- app_object()$rds$obsm[[ args()$dimred ]][idx,]

      label <- sub('X_', '', args()$dimred)
      colnames(dimred) <- paste0(label, 1:2)
    }
  }

  df <- cbind(df, gdat)
  colnames(df)[(ncol(df) - length(g) + 1):ncol(df)] <- g


  if(reduction){
    df <- cbind(as.data.frame(dimred), df)
  }

  if(!is.null(slice)) df <- cbind(coords, df)

  df
}


