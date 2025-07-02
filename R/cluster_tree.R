#' Cluster tree module UI
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#'
#' @export
#'
clustreeUI <- function(id, panel){
  ns <- NS(id)

  if(panel == 'sidebar'){
    tagList(
      br(),

      div(id=ns('assay_menu'),
        fluidRow(
          column(6,
            strong('Assay')
          ), # column
          column(6,
            selectInput(ns('clust_assay'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL)
          ) # column
        ) # fluidRow
      ), # div

      conditionalPanel(paste0('input["', ns('clustree_type'), '"] == "Single"'),

        fluidRow(
          column(12, strong('Plot options')),
        ),  # fluidRow

        fluidRow(
          column(5, 'Clustering column'),
          column(7,
            selectInput(ns('clust_column'),
                        label=NULL,
                        choices=NULL,
                        selected=NULL
            ) # selectInput
          ) # column
        ), # fluidRow

        fluidRow(align='center',
          column(12,
            actionButton(ns('plt_single_do'), 'Refresh',
                         icon=icon('refresh'),
                         class='btn-primary',
                         style='margin-bottom: 10px;')
          ) # column
        ), # fluidRow

        fluidRow(
          column(5, 'Tree direction'),
          column(7,
            selectInput(ns('clust_tree_direction'),
                        label=NULL,
                        choices=c('rightwards', 'downwards',
                                  'upwards', 'leftwards')
            ) # selectInput
          ) # column
        ), # fluidRow

        fluidRow(
          column(5, 'Text scale'),
          column(7,
            numericInput(ns('single_text_scale'),
                         label=NULL,
                         value=1.5)
          ) # column
        ) # fluidRow
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('clustree_type'), '"] == "Compare resolutions (Tree)" | ',
               'input["', ns('clustree_type'), '"] == "Compare resolutions (Overlay)"'),

        fluidRow(
          column(12, strong('Plot options'))
        ),  # fluidRow

        fluidRow(style='margin-bottom: 10px;',
          column(12, 'Clustering resolutions'),
        ),

        fluidRow(
          column(12,
            selectizeInput(ns('clust_col_tree_current'),
                           label=NULL,
                           choices=NULL,
                           multiple=TRUE)
          ) # column
        ), # fluidRow

        fluidRow(
          align='center',
          style='margin-bottom: 20px;',
          column(12,
            splitLayout(cellWidths=c('50%', '50%'),
                actionButton(ns('clust_col_tree_none'),
                             label='none',
                             icon=shiny::icon('minus'),
                             class='select-buttons'),
                actionButton(ns('clust_col_tree_all'),
                             label='all',
                             icon=shiny::icon('check'),
                             class='select-buttons')
            ) # splitLayout
          ) # column
        ), # fluidRow

        fluidRow(
          column(6, 'Cluster column prefix'),
          column(6,
            selectizeInput(ns('clust_col_prefix'),
                           label=NULL,
                           choices=NULL,
                           selected=NULL,
                           options=list(create=TRUE)
            ) # selectizeInput
          ) # column
        ), # fluidRow

        fluidRow(
          column(6, 'Replace with'),
          column(6,
            textInput(ns('clust_col_rep'),
                      label=NULL,
                      value='res.'
            ) # textInput
          ) # column
        ), # fluidRow

        fluidRow(
          column(6, 'Min incoming node proportion'),
          column(6,
            numericInput(ns('min_in_prop'),
                         label=NULL,
                         value=0.1
            ) # textInput
          ) # column
        ), # fluidRow

        conditionalPanel(
          paste0('input["', ns('clustree_type'), '"] == "Compare resolutions (Overlay)"'),

          fluidRow(
            column(12, strong('Overlay options'))
          ),  # fluidRow

          fluidRow(
            column(6, 'Dimension reduction'),
            column(6,
              selectInput(ns('clust_tree_dimred'),
                          label=NULL,
                          choices=NULL,
                          selected=NULL)
            ) # column
          ), # fluidRow

          fluidRow(
            column(6, 'Label nodes?'),
            column(6,
              selectInput(ns('clust_tree_label'),
                          label=NULL,
                          choices=c('no', 'yes')
              ) # selectInput
            ) # column
          ), # fluidRow

          fluidRow(
            column(6, 'Side view'),
            column(6,
              selectInput(ns('clust_tree_side'),
                          label=NULL,
                          choices=c('no', 'x-side', 'y-side')
              ) # selectInput
            ) # column
          ) # fluidRow

        ) # conditionalPanel

      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('clustree_type'), '"] == "Compare resolutions (Tree)"'),
        fluidRow(align='center',
          column(12,
            actionButton(ns('plt_tree_do'), 'Refresh plot',
                         class='btn-primary',
                         style='margin-bottom: 10px;')
          ) # column
        ) # fluidRow
      ), # conditionalPanel

      conditionalPanel(
        paste0('input["', ns('clustree_type'), '"] == "Compare resolutions (Overlay)"'),
        fluidRow(align='center',
          column(12,
            actionButton(ns('plt_overlay_do'), 'Refresh plot',
                         class='btn-primary',
                         style='margin-bottom: 10px;')
          ) # column
        ) # fluidRow
      ) # conditionalPanel

    ) # tagList
  } else if(panel == 'main'){
    tagList(
      tabsetPanel(type='tabs', id=ns('clustree_type'),
        tabPanel('Single',
          fluidRow(
            column(11, align='right',
              downloadPlotUI(ns('clustree_single_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('clustree_single_help'))
            ) # column
          ), # fluidRow

          actionButton(ns('single_tree_do'), 'Generate plot',
                       class='btn-primary'),
          withSpinner(
            plotOutput(ns('clustree_single'), height='700px')
          ) # withSpinner
        ), # tabPanel
        tabPanel('Compare resolutions (Tree)',
          fluidRow(
            column(11, align='right',
              downloadPlotUI(ns('clustree_tree_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('clustree_help'))
            ) # column
          ), # fluidRow

          actionButton(ns('clustree_tree_do'), 'Generate plot',
                       class='btn-primary'),
          span('Warning: Computationally intensive\n',
               style='font-style: italic; color: red'),
          withSpinner(
            plotOutput(ns('clustree_tree'), height='700px')
          ) # withSpinner
        ), # tabPanel
        tabPanel('Compare resolutions (Overlay)',
          fluidRow(
            column(11, align='right',
              downloadPlotUI(ns('clustree_overlay_dload'))
            ), # column
            column(1, align='left',
              helpButtonUI(ns('clustree_overlay_help'))
            ) # column
          ), # fluidRow

          actionButton(ns('clustree_overlay_do'), 'Generate plot',
                       class='btn-primary'),
          span('Warning: Computationally intensive\n',
               style='font-style: italic; color: red'),
          withSpinner(
            plotOutput(ns('clustree_overlay'), height='700px')
          ) # withSpinner
        ) # tabPanel
      )
    )
  }
} # clustreeUI


#' Cluster tree module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param filtered cell barcodes for filtering object
#' @param args reactive list with global args, 'grp_by' for grouping variable
#'        and 'dimred' for which dimension reduction to use
#' @param reload_global reactive to reload module
#' @param config reactive list with config settings
#'
#' @export
#'
clustreeServer <- function(id, obj, filtered, args, reload_global, config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        list(
          rds=obj$rds,
          obj_type=obj$obj_type,
          metadata=obj$metadata,
          grouping_vars=obj$grouping_vars
        )
      })

      clust_tree_obj <- reactiveValues(single=NULL,
                                       tree=NULL,
                                       overlay=NULL,
                                       args=NULL)

      obj_info <- reactiveValues(slot=NULL,
                                 filtered=NULL,
                                 cluster_columns=NULL,
                                 var_genes=list())

      observeEvent(app_object()$rds, {
        obj_info$slot <- NULL
        obj_info$filtered <- NULL
        obj_info$var_genes <- list()
        obj_info$cluster_columns <- NULL

        if(app_object()$obj_type == 'seurat'){

          shinyjs::show(id='assay_menu')

        } else if(app_object()$obj_type == 'anndata'){
          shinyjs::hide(id='assay_menu')

        }
      })

      # read & save filtered object data
      observeEvent(filtered(), {
        # reset data on (re)load
        obj_info$slot <- NULL
        obj_info$filtered <- NULL

        ## reset clustree objects
        obj_info$cluster_columns <- NULL
        clust_tree_obj$single <- NULL
        clust_tree_obj$tree <- NULL
        clust_tree_obj$overlay <- NULL
        clust_tree_obj$args <- NULL

        obj_type <- app_object()$obj_type

        # get current barcodes & save
        obj_info$filtered <- filtered()

        meta_cols <- colnames(app_object()$metadata)

        if(obj_type == 'seurat'){

          # update assay menus
          assay_names <- names(app_object()$rds@assays)

          if('SCT' %in% assay_names) selected <- 'SCT'
          else selected <- assay_names[1]
          updateSelectInput(session, 'clust_assay',
                            choices=assay_names,
                            selected=selected)

          # get names of reductions
          dr_choices <- names(app_object()$rds@reductions)

        } else if(obj_type == 'anndata'){

          updateSelectInput(session, 'clust_assay',
                            choices='assay',
                            selected='assay')

          obj_info$slot <- 'none'

          # get names of reductions
          dr_choices <- names(app_object()$rds$obsm)
        }

        col_choices <- app_object()$grouping_vars
        if(args()$grp_by != '' & args()$grp_by %in% col_choices) clust_sel <- args()$grp_by
        else clust_sel <- col_choices[1]
        updateSelectInput(session, 'clust_column',
                          choices=col_choices,
                          selected=clust_sel)

        # pick a umap reduction by default (if available)
        umap_idx <- grep('umap', dr_choices)

        if(length(umap_idx) >= 1) selected <- dr_choices[ umap_idx[1] ]
        else selected <- dr_choices[1]
        updateSelectInput(session, 'clust_tree_dimred',
                          choices=dr_choices,
                          selected=selected)

        # get clust column regex
        clust_regex <- config()$server$grouping_column$regex

        # find matches
        clust_idx <- lapply(clust_regex, function(x) grep(x, meta_cols))
        names(clust_idx) <- clust_regex

        # if matches are non-unique, show warning, don't do anything
        if(length(unlist(clust_idx)) != length(unique(unlist(clust_idx)))){
          showNotification(
            'Warning: Cluster column prefixes return non-unique matches',
            type='warning'
          )
        }

        # get clust_col_prefix from metadata columns
        tmp_prefix <- unique(
                        unlist(
                          lapply(clust_regex,
                            function(x){
                              idx <- clust_idx[[x]]

                              # only keep if multiple resolutions found
                              if(length(idx) >= 2){
                                # seurat object
                                if(regexpr('res', x) > 0){
                                  # remove trailing number, e.g. '1.2'
                                  tmp_prefix <- unique(
                                                  sub('(\\d|\\.)+', '',
                                                      meta_cols[idx])
                                                )

                                  # add back trailing '.'
                                  tmp_prefix <- paste0(tmp_prefix, '.')
                                } else {
                                  # anndata object
                                  tmp_prefix <- x
                                }
                                tmp_prefix
                              }
                            }
                          )
                        )
                      )

        # check the number of res columns matching each tmp_prefix
        col_idx <- unlist(lapply(tmp_prefix, function(x) length(grep(x, meta_cols)) >= 2))

        # if fewer than 2 cluster columns for any prefix, hide 'compare resolutions' tabs
        if(!any(col_idx)){
            hideTab(inputId = 'clustree_type',
                    target='Compare resolutions (Tree)')
            hideTab(inputId = 'clustree_type',
                    target='Compare resolutions (Overlay)')
            showTab(inputId = 'clustree_type',
                    target = 'Single', select=TRUE)

          updateSelectInput(session, 'clust_col_prefix',
                            choices='',
                            selected='')

          updateSelectizeInput(session,
                               'clust_col_tree_current',
                               choices='',
                               selected='')
        } else {
          showTab(inputId = 'clustree_type',
                  target='Compare resolutions (Tree)')
          showTab(inputId = 'clustree_type',
                  target='Compare resolutions (Overlay)')

          tmp_prefix <- tmp_prefix[which(col_idx)]

          if(obj_type == 'seurat'){

            # update selection based on selected assay
            sel_idx <- grep(input$clust_assay, tmp_prefix)
            if(length(sel_idx) == 0) sel_idx <- 1
            updateSelectInput(session, 'clust_col_prefix',
                              choices=tmp_prefix,
                              selected=tmp_prefix[sel_idx])
            sel_prefix <- tmp_prefix[sel_idx]

          } else if(obj_type == 'anndata'){

            updateSelectInput(session, 'clust_col_prefix',
                              choices=tmp_prefix,
                              selected=tmp_prefix[1])

            sel_prefix <- tmp_prefix[1]
          }

          # update cluster column menu
          col_idx <- unique(unlist(lapply(sel_prefix, function(x) grep(x, meta_cols))))
          col.choices <- meta_cols[col_idx]
          col_names <- col.choices
          for(pre in tmp_prefix){
            col_names <- sub(pre, '', col_names)
          }
          names(col.choices) <- col_names

          # save cluster column choices & update 'detected clusterings' menus
          obj_info$cluster_columns <- col.choices
          updateSelectizeInput(session,
                               'clust_col_tree_current',
                               choices=col.choices,
                               selected=col.choices)
        }

        showNotification(
          'Loaded cluster tree ...'
        )
      })

      observeEvent(reload_global(), {
        updateSelectInput(session, 'clust_column',
                          selected=args()$grp_by)

        updateSelectInput(session, 'clust_tree_dimred',
                          selected=args()$dimred)
      })

      observeEvent(input$clust_col_prefix %>% debounce(1000), {
        validate(
          need(!is.null(obj_info$filtered), '')
        )

        if(app_object()$obj_type == 'seurat'){
          meta_cols <- colnames(app_object()$rds@meta.data)
        } else if(app_object()$obj_type == 'anndata'){
          meta_cols <- colnames(app_object()$rds$obs)
        }

        col_idx <- grep(input$clust_col_prefix, meta_cols)
        col.choices <- meta_cols[col_idx]
        names(col.choices) <- sub(input$clust_col_prefix,
                                  '', col.choices)
        obj_info$cluster_columns <- col.choices

        updateSelectizeInput(session,
                             'clust_col_tree_current',
                             choices=col.choices,
                             selected=col.choices)
      })

      observeEvent(input$clust_col_tree_none, {
        updateSelectizeInput(session,
                             'clust_col_tree_current',
                             selected='')
      })

      observeEvent(input$clust_col_tree_all, {
        if(!is.null(obj_info$cluster_columns)){
          updateSelectizeInput(session,
                               'clust_col_tree_current',
                               choices=obj_info$cluster_columns,
                               selected=obj_info$cluster_columns)
        }
      })

      ######################### Single view #########################

      observeEvent(c(input$single_tree_do,
                     input$plt_single_do), {

        mdata <- app_object()$metadata
        validate(
          need(!is.null(mdata), '')
        )

        validate(
          need(input$clust_column != '' & input$clust_column %in% colnames(mdata),
               'Please select clustering column')
        )

        obj_type <- app_object()$obj_type
        bc <- obj_info$filtered

        if(obj_type == 'seurat'){
          obj <- subset(app_object()$rds, cells=bc)

          # remove 'ident' column from meta.data since it
          # overrides Idents() calls
          if('ident' %in% colnames(mdata)){
            obj@meta.data$ident <- NULL
          }

          # set Idents
          Idents(obj) <- input$clust_column
          mdata <- obj@meta.data

        } else if(obj_type == 'anndata'){

          idx <- rownames(mdata) %in% bc
          obj <- app_object()$rds[idx,]
          mdata <- mdata[idx,]
        }

        # get most variable genes
        if(input$clust_assay %in% names(obj_info$var_genes)){
          var_genes <- obj_info$var_genes[[ input$clust_assay ]]
        } else {
          if(obj_type == 'anndata'){
            if(nrow(mdata) > 50000 & !'highly_variable' %in% colnames(app_object()$rds$var)){
              showNotification(
                paste('Warning: Most variable genes not found in object.',
                      'Calculating this can be *very* expensive for large datasets'),
                type='warning'
              )
            }
          } else if(obj_type == 'seurat'){
            obj_class <- class(obj@assays[[ input$clust_assay ]])
            if(inherits(obj@assays[[ input$clust_assay ]], 'Assay5')){
              showNotification(
                'Warning: Seurat v5 assay not supported. Please select different assay and retry',
                type='error',
                duration=15
              )
            }

            validate(
              need(!inherits(obj@assays[[ input$clust_assay ]], 'Assay5'), '')
            )
          }
          var_genes <- get_var_genes(obj, obj_type,
                                     assay_name=input$clust_assay)

          max_var_genes <- config()$server$max_var_genes
          if(length(var_genes) > max_var_genes) var_genes <- var_genes[1:max_var_genes]
          obj_info$var_genes[[ input$clust_assay ]] <- var_genes
        }

        if(obj_type == 'seurat'){
          if(is.null(Tool(obj, slot='BuildClusterTree'))){
            withProgress({
              obj <- tryCatch({
                       BuildClusterTree(obj, assay=input$clust_assay,
                                        features=var_genes)
                     },
                     error=function(e){ e })

              validate(
                need(inherits(obj, 'Seurat'), 'Error building cluster tree. Please try again with different settings')
              )
            }, message='Building cluster tree', value=0.5)
          }

          clust_tree_obj$single <- Tool(obj, slot='BuildClusterTree')
        } else if(obj_type == 'anndata'){

          withProgress(
            {
              data.tree <- BuildClusterTree2(obj,
                                             features=var_genes,
                                             clust_column=input$clust_column)
              clust_tree_obj$single <- data.tree
            }, message='Building cluster tree', value=0.5)
        }

        num_leaves <- length(clust_tree_obj$single$tip.label)
        base_ht <- 600
        if(num_leaves > 20) tree_ht <- round(base_ht*(num_leaves/20))
        else tree_ht <- base_ht

        # only allow height to go up to 2*tree_ht
        if(tree_ht > 3*base_ht) tree_ht <- 3*base_ht

        clust_tree_obj$args <- list(tree_ht=tree_ht)

      })

      ################## Compare resolutions - Tree ###############

      observeEvent(c(input$clustree_tree_do, input$plt_tree_do), {
        validate(
          need(!is.null(app_object()$metadata), '')
        )

        if(length(input$clust_col_tree_current) < 2){
          showNotification(
            'Fewer than 2 clusterings selected. Please choose at least two or adjust cluster column prefix and retry',
            type='error'
          )
        }

        validate(
          need(length(input$clust_col_tree_current) >= 2, '')
        )

        # only keep selected columns in metadata
        mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=TRUE)
        idx <- mdata$rn %in% obj_info$filtered
        mdata <- mdata[idx, ]
        mdata <- as.data.frame(mdata)

        mdata <- mdata[,colnames(mdata) %in% input$clust_col_tree_current]

        # remove column prefix and replace with clust_col_rep
        rep <- input$clust_col_rep
        if(rep == ''){
          showNotification(
              '"Replace with" cannot be empty. Using "X" instead'
          )
          rep <- 'X'
        } else {
          tmp <- sub(input$clust_col_prefix, '', colnames(mdata))
          validate(
            need(sum(is.na(as.numeric(tmp))) == 0,
                 'Column names could not be converted to a number after removing "Cluster column prefix". Please retry')
          )
        }
        colnames(mdata) <- sub(input$clust_col_prefix,
                               rep, colnames(mdata))

        # check min in_prop
        if(is.na(input$min_in_prop)){
          showNotification(
            'Minimum incoming node proportion must be > 0. Setting to 0'
          )
          min_in_prop <- 0

        } else if(input$min_in_prop < 0){
          showNotification(
            'Minimum incoming node proportion must be > 0. Setting to 0'
          )
          min_in_prop <- 0
        } else {
          min_in_prop <- input$min_in_prop
        }

        withProgress(
          {

            p <- clustree(mdata,
                          prefix=rep,
                          prop_filter=min_in_prop,
                          node_text_size=3,
                          node_alpha=0.9)
          },
          message='Generating cluster tree'
        )
        clust_tree_obj$tree <- p
      }, ignoreNULL=FALSE)

      clust_tree <- eventReactive(clust_tree_obj$tree, {
        validate(
          need(length(input$clust_col_tree_current) >= 2,
               'Fewer than 2 clusterings selected. Please choose at least two or adjust cluster column prefix and retry')
        )

        p <- clust_tree_obj$tree

        p + theme(legend.title=element_text(size=15, face='bold'),
                  legend.text=element_text(size=12))
      })

      ################## Compare resolutions - Overlay ###############

      observeEvent(input$clust_assay, {
        validate(
          need(!is.null(obj_info$filtered), '')
        )

        if(app_object()$obj_type == 'seurat'){
          dr_choices_all <- names(app_object()$rds@reductions)
        } else if(app_object()$obj_type == 'anndata'){
          dr_choices_all <- names(app_object()$rds$obsm)
        }
        dr_choices <- dr_choices_all[grep(tolower(input$clust_assay), dr_choices_all)]

        if(length(dr_choices) == 0) dr_choices <- dr_choices_all
        names(dr_choices) <- sub('X_', '', dr_choices)

        # pick a umap reduction by default (if available)
        umap_idx <- grep('umap', dr_choices)

        if(length(umap_idx) >= 1) selected <- dr_choices[ umap_idx[1] ]
        else selected <- dr_choices[1]
        updateSelectInput(session, 'clust_tree_dimred',
                          choices=dr_choices,
                          selected=selected)

      })

      observeEvent(c(input$clustree_overlay_do, input$plt_overlay_do), {
        validate(
          need(!is.null(obj_info$filtered), '')
        )

        if(length(input$clust_col_tree_current) < 2){
          showNotification(
            'Fewer than 2 clusterings selected. Please choose at least two or adjust cluster column prefix and retry',
            type='error'
          )
        }
        validate(
          need(length(input$clust_col_tree_current) >= 2, '')
        )

        # subset metadata to keep current barcodes
        mdata <- data.table::as.data.table(app_object()$metadata, keep.rownames=TRUE)
        idx <- mdata$rn %in% obj_info$filtered
        mdata <- mdata[idx, ]
        mdata <- as.data.frame(mdata)

        # only keep selected columns in metadata
        mdata <- mdata[,colnames(mdata) %in% input$clust_col_tree_current]

        # remove column prefix and replace with clust_col_rep
        rep <- input$clust_col_rep
        if(rep == ''){
          showNotification(
              '"Replace with" cannot be empty. Using "X" instead'
          )
          rep <- 'X'
        } else {
          tmp <- sub(input$clust_col_prefix, '', colnames(mdata))
          validate(
            need(sum(is.na(as.numeric(tmp))) == 0,
                 'Column names could not be converted to a number after removing "Cluster column prefix". Please retry')
          )
        }
        colnames(mdata) <- sub(input$clust_col_prefix,
                               rep, colnames(mdata))

        # check min in_prop
        if(is.na(input$min_in_prop)){
          showNotification(
            'Minimum incoming node proportion must be > 0. Setting to 0'
          )
          min_in_prop <- 0

        } else if(input$min_in_prop < 0){
          showNotification(
            'Minimum incoming node proportion must be > 0. Setting to 0'
          )
          min_in_prop <- 0
        } else {
          min_in_prop <- input$min_in_prop
        }

        red_dim <- input$clust_tree_dimred
        if(input$clust_tree_label == 'yes') label_nodes <- TRUE
        else label_nodes <- FALSE

        if(app_object()$obj_type == 'seurat'){
          df <- app_object()$rds@reductions[[ red_dim ]]@cell.embeddings[idx, ]
        } else if(app_object()$obj_type == 'anndata'){
          df <- app_object()$rds$obsm[[ red_dim ]][idx,]
        }

        label <- sub('X_', '', red_dim)
        colnames(df) <- paste0(label,  1:ncol(df))

        mdata <- cbind(mdata, df[, 1:2])
        withProgress(
          {
        #clust_tree_obj$overlay <- future_promise({
        p_list <- clustree_overlay(x=mdata,
                                   prefix=rep,
                                   prop_filter=min_in_prop,
                                   x_value=colnames(df)[1],
                                   y_value=colnames(df)[2],
                                   label_nodes=label_nodes,
                                   plot_sides=TRUE)
        #  })
          },
          message='Generating cluster tree - overlay'
        )
        clust_tree_obj$overlay <- p_list
      })

      clust_overlay <- eventReactive(clust_tree_obj$overlay, {
        validate(
          need(length(input$clust_col_tree_current) >= 2,
               'Fewer than 2 clusterings selected. Please choose at least two or adjust cluster column prefix and retry')
        )

        p_list <- clust_tree_obj$overlay
        if(input$clust_tree_side == 'no'){
          p <- p_list$overlay
        } else if(input$clust_tree_side == 'x-side'){
          p <- p_list$x_side
        } else if(input$clust_tree_side == 'y-side'){
          p <- p_list$y_side
        }

        p <- p + guides(colour='none') +
          theme(axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=15, face='bold'),
                axis.title.y=element_text(size=15, face='bold'),
                legend.title=element_text(size=15, face='bold'),
                legend.text=element_text(size=12))
        p
      })

      output$clustree_tree <- renderPlot({
        validate(
          need(!is.null(app_object()$rds), '')
        )

        isolate({
          l <- length(input$clust_col_tree_current)
        })
        if(l >= 2){

          p <- clust_tree()

          p
        } else {
          showNotification(
            'Fewer than 2 clusterings found in object. Need at least 2 to compare',
            type='error'
          )
          NULL
        }
      })

      output$clustree_overlay <- renderPlot({
        validate(
          need(!is.null(app_object()$rds), '')
        )

        isolate({
          l <- length(input$clust_col_tree_current)
        })

        if(l >= 2){

          p <- clust_overlay()

          p
        } else {
          NULL
        }
      })

      get_clustree_single <- reactive({
        clust_tree_obj$single
      })

      output$clustree_single <- renderPlot(
        height=eventReactive(c(get_clustree_single(),
                               input$single_tree_do,
                               input$plt_single_do), {
          validate(
            need(!is.null(clust_tree_obj$args), '')
          )
          clust_tree_obj$args$tree_ht
        }),
        {
          p <- get_clustree_single()
          if(!any(p$edge.length > 0)) use.edge.length <- FALSE
          else use.edge.length <- TRUE

          if(is.na(input$single_text_scale)) cex <- 1
          else cex <- input$single_text_scale

          p <- ape::plot.phylo(p,
                               direction=input$clust_tree_direction,
                               use.edge.length=use.edge.length,
                               cex=cex)
          p

      })

      helpButtonServer('clustree_help', size='l')
      helpButtonServer('clustree_single_help', size='l')
      helpButtonServer('clustree_overlay_help', size='l')

      observeEvent(input$clust_tree_direction, {
        downloadPlotServer('clustree_single_dload',
                           get_clustree_single,
                           'clustree_single',
                           direction=input$clust_tree_direction)
      })

      downloadPlotServer('clustree_tree_dload', clust_tree,
                         'clustree_tree')
      downloadPlotServer('clustree_overlay_dload', clust_overlay,
                         'clustree_overlay')

    }
  ) # moduleServer
} # clustreeServer
