#' Generalized marker tables module UI
#'
#' @param id Input id
#' @param panel string, can be 'sidebar' or 'main'
#' @param type string, used to define type of table
#' @param label string, label for table
#'
#' @export
#'
markerTableGeneralUI <- function(id, panel, type, label=NULL){
  ns <- NS(id)

  if(panel == 'sidebar'){
    tagList(
      div(id=ns('marker_type_menu'),
        fluidRow(
          column(6, 'Marker type'),
          column(6,
            selectInput(ns('marker_type'), label=NULL,
                        choices=c('unique', 'all'),
                        selected='unique')
          ) # column
        ) # fluidRow
      ), # div

      bsCollapse(
        bsCollapsePanel(span(icon('filter'), 'filter by cluster'),
          value='clusters',

          fluidRow(
            column(12,
              selectizeInput(ns('clusters'), label=NULL,
                             choices=NULL,
                             selected=NULL,
                             multiple=TRUE)
            ) # column
          ), # fluidRow

          fluidRow(
            align='center',
            style='margin-bottom: 20px;',
            column(12,
              splitLayout(cellWidths=c('50%', '50%'),
                  actionButton(ns('clust_none'),
                               label='none',
                               icon=shiny::icon('minus'),
                               class='select-buttons'),
                  actionButton(ns('clust_all'),
                               label='all',
                               icon=shiny::icon('check'),
                               class='select-buttons')
              ) # splitLayout
            ) # column
          ) # fluidRow

        ) # bsCollapsePanel
      ), # bsCollapse

      div(id=ns('assay_menu'),
        bsCollapse(
          bsCollapsePanel(span(icon('filter'), 'filter by assay'),
            value='assay',
            controlUI(ns('assay'), label='')
          )
        )
      ), # div

      div(id=ns('resolution_menu'),
        bsCollapse(
          bsCollapsePanel(span(icon('filter'), 'filter by resolution'),
            value='resolution',
            controlUI(ns('resolution'), label='')
          )
        )
      ), # div

      div(id=ns('comparison_menu'),
        bsCollapse(
          bsCollapsePanel(span(icon('filter'), 'filter by comparison'),
            value='comparison',
            controlUI(ns('comparison'), label='')
          )
        )
      ), # div

      div(id=ns('group_menu'),
        bsCollapse(
          bsCollapsePanel(span(icon('filter'), 'filter by group'),
            value='group',
            controlUI(ns('group'), label='')
          )
        )
      ), # div

      div(id=ns('samp_menu'),
        bsCollapse(
          bsCollapsePanel(span(icon('filter'), 'filter columns'),
            value='column',

            fluidRow(
              column(6, 'Sample groups shown'),
              column(6,
                selectizeInput(ns('samps'), label=NULL,
                               choices=NULL,
                               selected=NULL,
                               multiple=TRUE)
              ) # column
            ), # fluidRow

            fluidRow(
              align='center',
              style='margin-bottom: 20px;',
              column(6, span()),
              column(6,
                splitLayout(cellWidths=c('50%', '50%'),
                    actionButton(ns('nosamps'),
                                 label='none',
                                 icon=shiny::icon('minus'),
                                 class='select-buttons'),
                    actionButton(ns('allsamps'),
                                 label='all',
                                 icon=shiny::icon('check'),
                                 class='select-buttons')
                ) # splitLayout
              ) # column
            ) # fluidRow
          ) # bsCollapsePanel
        ) # bsCollapse
      ), # div

      fluidRow(align='center',
        column(12,
          actionButton(ns('tbl_do'), 'Reload',
                       class='btn-primary',
                       style='margin-bottom: 10px;')
        ) # column
      ) # fluidRow


    ) # tagList
  } else if(panel == 'selection'){
    tagList(
      fluidRow(
        align='center',
        style='margin-bottom: 10px;',
        column(6,
          actionButton(ns('add_selected_markers'),
                      'Add to scratchpad')
        ), # column

        column(6,
          actionButton(ns('reset_markers'),
                       'Clear selection',
                       class='btn-primary')
        ) # column
      ), # fluidRow

      fluidRow(
        column(12,
          textOutput(ns('selected_genes'))
        ) # column
      ) # fluidRow

    ) # tagList
  } else if(panel == 'main'){
    if(is.null(label)) label <- type

    tabPanel(title=label, value=type,
      fluidRow(
        column(11, align='right',
          downloadFileUI(ns('tbl_dload'))
        ), # column
        column(1,
          align='left',
          helpButtonUI(ns(paste0(type, '_help')))
        ) #column
      ), # fluidRow

     withSpinner(
       DTOutput(ns('marker_tbl'))
     ) # withSpinner
    ) # tabPanel
  }
} # ui


#' Generalize marker table module server
#'
#' @param id Input id
#' @param obj reactive list with marker tables
#' @param type string, can be 'allmarkers', 'consmarkers' or 'demarkers'
#' @param genes_to_plot reactive list of genes to plot
#' @param reset_genes reactive to trigger gene selection reset
#' @param global_args reactive list with global settings
#' @param args reactive list with 'max_padj', 'max_lfc'
#' @param reload_global reactive to trigger global args reload
#' @param config reactive list with config settings
#'
#' @export
#'
markerTableGeneralServer <- function(id, obj, type,
                                     genes_to_plot, reset_genes,
                                     global_args,
                                     args,
                                     reload_global,
                                     config){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        list(
          markers=obj()$markers
        )
      })

      flags <- reactiveValues(data_loaded=0)

      marker_info <- reactiveValues(current_tbl=NULL,
                                    gene_column=NULL,
                                    padj=NULL,
                                    lfc=NULL,
                                    filter_cols=NULL,
                                    clicked_genes=NULL,
                                    filters=list(),
                                    all_res=NULL,
                                    all_grps=NULL) # only used for consmarkers

      ################## Load data ####################

      observeEvent(app_object(), {
        # reset before loading new data
        marker_info$current_tbl <- NULL
        marker_info$clicked_genes <- NULL
        marker_info$filters <- list()
        marker_info$all_res <- NULL
        marker_info$all_grps <- NULL
        marker_info$gene_column <- NULL
        marker_info$padj <- NULL
        marker_info$lfc <- NULL

        filters$assay <- NULL
        filters$comparison <- NULL
        filters$group <- NULL
        filters$resolution <- NULL

        validate(
          need(!is.null(app_object()$markers), '')
        )

        df <- app_object()$markers

        # get columns
        all_cols <- c('gene_column', 'padj', 'lfc')
        for(cl in all_cols){
          idx <- which(colnames(df) %in% config()$server$markers[[ cl ]])
          if(length(idx) > 0) marker_info[[ cl ]] <- colnames(df)[idx[1]]
        }

        # get current filter columns
        marker_info$filter_cols <- intersect(colnames(df),
                                             config()$server$markers$filter_cols)

        for(col in config()$server$markers$filter_cols){
          if(col %in% colnames(df)){
            lvls <- unique(df[[ col ]])

            # updt marker info & filters
            marker_info$filters[[ col ]] <- lvls
            filters[[ col ]] <- lvls

            shinyjs::show(id=paste0(col, '_menu'))
          } else {
            shinyjs::hide(id=paste0(col, '_menu'))
          }
        }


        if(type == 'consmarkers'){
          # calculate conserved markers sample groups
          # each group should have 5 columns:
          # *_p_val, *_avg_log2FC, *_pct.1, *_pct.2, *_p_val_adj
          # NOTE: using '_pct.1' as search string to avoid ambiguity with p_val/p_val_adj columns
          # NOTE: this is seurat-specific
          idx <- grep('_pct\\.1', colnames(df))
          samps <- colnames(df)[idx]
          samps <- sub('_pct\\.1', '', samps)
          updateSelectizeInput(session, 'samps',
                               choices=samps,
                               selected=samps)
          marker_info$all_grps <- samps

          shinyjs::show(id='samp_menu')
        } else {

          updateSelectizeInput(session, 'samps',
                               choices='',
                               selected='')

          shinyjs::hide(id='samp_menu')
        }

        flags$data_loaded <- flags$data_loaded + 1
      })

      observeEvent(reload_global(), {
        for(name in names(global_args())){
          tmp <- global_args()[[ name ]]
          if(!setequal(tmp, filters[[ name ]])){
            filters[[ name ]] <- tmp
          }
        }
      })

      ################  Filters ################

      filters <- reactiveValues(assay=NULL,
                                comparison=NULL,
                                group=NULL,
                                resolution=NULL)

      assays <- controlServer('assay',
                  reactive({ app_object()$markers }),
                  'assay',
                  reactive({ filters[[ 'assay' ]] })
                )

      comparisons <- controlServer('comparison',
                       reactive({ app_object()$markers }),
                       'comparison',
                       reactive({ filters[[ 'comparison' ]] })
                     )

      groups <- controlServer('group',
                  reactive({ app_object()$markers }),
                  'group',
                  reactive({ filters[[ 'group' ]] })
                )

      resolutions <- controlServer('resolution',
                       reactive({ app_object()$markers }),
                       'resolution',
                       reactive({ filters[[ 'resolution' ]] })
                     )

      # NOTE: ignoreNULL=FALSE forces trigger even if returned value=''
      observeEvent(comparisons(), {
        filters$comparison <- comparisons()
      }, ignoreNULL=FALSE)

      observeEvent(groups(), {
        filters$group <- groups()
      }, ignoreNULL=FALSE)

      observeEvent(assays(), {
        filters$assay <- assays()
      }, ignoreNULL=FALSE)

      observeEvent(resolutions(), {
        filters$resolution <- resolutions()
      }, ignoreNULL=FALSE)

      # function to update clusters menu based on input df
      # NOTE: this also depends on filters
      update_clusters <- function(df){
        cols <- config()$server$markers$filter_cols

        if(!any(cols %in% colnames(df))){
          clusters <- unique(df[['cluster']])
        } else {
          cols_to_filt <- intersect(cols, colnames(df))

          idx <- rep(TRUE, nrow(df))
          for(cl in cols_to_filt){
            tmp <- factor(df[[ cl ]]) %in% filters[[ cl ]]
            idx <- idx & tmp
          }
          df <- df[idx,]

          clusters <- unique(df[['cluster']])
        }

        clusters <- clusters[order(clusters)]
        updateSelectizeInput(session, 'clusters',
                             choices=clusters,
                             selected=clusters)
      }

      # update clusters when any filters change
      observeEvent(c(flags$data_loaded, # this flag makes sure clusters are updated on load even if no filters are present
                     filters$assay,
                     filters$resolution,
                     filters$comparison,
                     filters$group,
                     input$clust_all), {
        validate(
          need(!is.null(app_object()$markers), '')
        )

        for(col in marker_info$filter_cols){
          if(col %in% colnames(df)){
            validate(
              need(!is.null(filters[[ col ]]), '')
            )
          }
        }

        update_clusters(app_object()$markers)

      }) # observeEvent

      # cluster selection controls
      observeEvent(input$clust_none, {
        updateSelectizeInput(session, 'clusters',
                             selected='')
      }) # observeEvent

      # sample group selection controls
      # NOTE: this only affects *columns* shown, not rows
      observeEvent(input$nosamps, {
        updateSelectizeInput(session, 'samps',
                             selected='')
      })

      observeEvent(input$allsamps, {
        updateSelectizeInput(session, 'samps',
                             selected=marker_info$all_grps)
      })

      #################### Marker table ####################

      get_marker_tbl <- eventReactive(c(input$tbl_do,
                                        flags$data_loaded), {

        validate(
          need(!is.null(app_object()$markers),
               'Markers table not found')
        )

        validate(
          need(input$clusters != '',
               'No clusters selected')
        )

        df <- app_object()$markers

        # apply padj & lfc filters
        if(!is.na(args()$max_padj) & args()$max_padj < 1){
          if(!is.null(marker_info$padj )){
            df <- df[df[[ marker_info$padj ]] < args()$max_padj, ]
          } else {
            showNotification(
              paste('Warning: No padj columns detected in marker table.',
                    'Filtering by padj is disabled'),
              type='warning'
            )
          }
        }

        if(!is.na(args()$min_lfc) & args()$min_lfc != 0){
          if(!is.null(marker_info$lfc)){
            df <- df[abs(df[[ marker_info$lfc ]]) >= args()$min_lfc, ]
          } else {
            showNotification(
              paste('Warning: No LFC columns detected in marker table.',
                    'Filtering by LFC is disabled'),
              type='warning'
            )
          }
        }

        # drop 'nclusters' if present and add new one based on
        # filtered table
        # NOTE: only add this for allmarkers/consmarkers
        if('nclusters' %in% colnames(df)){
          df <- df[, setdiff(colnames(df), 'nclusters')]

          gc <- marker_info$gene_column
          tmp <- df[[ gc ]]
          for(fc in marker_info$filter_cols){
            tmp <- paste(tmp, df[[ fc ]])
          }
          cluster_counts <- table(tmp)
          df$nclusters <- as.numeric(cluster_counts[tmp])
        }

        # apply filters
        for(col in marker_info$filter_cols){
          if(col %in% colnames(df)){
            df <- df[df[[ col ]] %in% filters[[ col ]], ]
          }
        }

        if(input$marker_type == 'unique' & 'nclusters' %in% colnames(df)){
          df <- df[df[['nclusters']] == 1, ]
        }

        # filter to keep clusters
        df <- df[df[['cluster']] %in% input$clusters, ]
        validate(
            need(nrow(df) > 0,
                 'No markers remain after filtering. Consider adjusting criteria')
        )

        # arrange columns
        cols_to_filt <- intersect(colnames(df), config()$server$markers$filter_cols)

        if(!is.null(marker_info$gene_column))
          col_order <- marker_info$gene_column

        col_order <- c(col_order, 'cluster')

        if('nclusters' %in% colnames(df))
          col_order <- c(col_order, 'nclusters')

        # add remaining columns
        col_order <- c(col_order,
                       cols_to_filt,
                       marker_info$lfc, marker_info$padj)

        cols.to.drop <- config()$server$markers$cols.to.drop

        # only show selected sample columns
        if(type == 'consmarkers'){
          # drop sample columns that are not chosen
          samps_to_drop <- setdiff(marker_info$all_grps,
                                   input$samps)

          if(length(samps_to_drop) > 0){
            drop.idx <- unlist(lapply(samps_to_drop, function(x){
                          grep(paste0('^',x), colnames(df))
                        }))
            cols.to.drop <- unique(c(cols.to.drop,
                                     colnames(df)[drop.idx]))
          }

        }

        df <- df[, c(col_order, setdiff(colnames(df), col_order))]

        # save current data frame to reactive
        marker_info$current_tbl <- df

        df <- df %>% select(-any_of(cols.to.drop))

        df

      })

      output$marker_tbl <- renderDT({
          validate(
            need(!is.null(app_object()$markers), 'Loading ...')
          )
          df <- get_marker_tbl()

          format_cols <- config()$server$markers$format_significant
          matches <- lapply(format_cols$column_regex, function(x){
                          grep(x, colnames(df))
                      })
          which_cols <- colnames(df)[ unlist(matches) ]

          # also add any additional columns with 'double' values
          dbl_cols <- which(unlist(lapply(df, is.double)))
          which_cols <- unique(c(which_cols, colnames(df)[dbl_cols]))

          # remove 'cluster', 'nclusters', 'resolution'
          which_cols <- setdiff(which_cols, c('cluster', 'nclusters', 'resolution'))

          if(type == 'consmarkers') samps_to_keep <- input$samps
          else samps_to_keep <- NULL

          if(length(samps_to_keep) > 0){
            # get remaining sample column indices
            samp.idx <- lapply(samps_to_keep, function(x){
                          grep(paste0('^',x), colnames(df))
                        })

            # make sure no other cols are to the right of sample cols
            # NOTE: here we reorder columns, putting the sample columns
            #       to the right-most position
            df <- df[, c(setdiff(1:ncol(df), unique(unlist(samp.idx))), 
                         unique(unlist(samp.idx)))]

            # get remaining sample column indices
            # NOTE: doing this again since we just reordered cols
            samp.idx <- lapply(samps_to_keep, function(x){
                          grep(paste0('^',x), colnames(df))
                        })

            # get sample col suffixes (assume same for each sample)
            samp.suffix <- sub(paste0(samps_to_keep[1], '_'), '',
                               colnames(df)[samp.idx[[1]]])

            # get non-sample column names
            nonsamp.idx <- setdiff(1:ncol(df), unique(unlist(samp.idx)))
            nonsamp.names <- colnames(df)[nonsamp.idx]

            # build container for table
            sketch <- htmltools::withTags(table(
              class = 'display',
              tags$thead(
                tags$tr(
                  lapply(nonsamp.names,
                         function(x) tags$th(rowspan=2, x)),
                  lapply(samps_to_keep,
                         function(x) tags$th(class='dt-center',
                                        colspan=length(samp.suffix), x))
                ),
                tags$tr(
                  lapply(rep(samp.suffix, length(samps_to_keep)), tags$th)
                )
              )
            ))

            border_cols <- colnames(df)[
                                unlist(lapply(samp.idx, function(x) x[1]))
                           ]

            df <- df %>%
                datatable(rownames=FALSE,
                          container=sketch) %>%
                formatStyle(columns=border_cols,
                          'border-left'='solid 1px')
          } else {
            if(!is.null(marker_info$gene_column)){
              df <- df %>% relocate(any_of(marker_info$gene_column))
            }

            df <- df %>%
                datatable(rownames=FALSE)
          }

          df %>%
            formatSignif(columns=which_cols, digits=format_cols$digits) %>%
            formatStyle(columns=which_cols, 'white-space'='nowrap')
      })

      # create proxy for all markers table
      markers_proxy <- dataTableProxy('marker_tbl')

      #################### Table selection ####################

      observeEvent(input$reset_markers, {
        markers_proxy %>% selectRows(NULL)
      })

      # function to get clicked genes from marker table
      get_clicked_genes <- function(){
        tbl <- marker_info$current_tbl
        sel <- input$marker_tbl_rows_selected

        gene_col <- marker_info$gene_column
        validate(
          need(!is.null(gene_col), '')
        )

        # handle NAs in symbol
        s <- tbl[[gene_col]]
        s[is.na(s)] <- tbl$gene[is.na(s)]

        if(is.null(sel)){
          validate(
            need(!is.null(sel), '')
          )
        }

        s[sel]
      }


      observeEvent(input$add_selected_markers, {
        sel <- input$marker_tbl_rows_selected
        if(is.null(sel)){
          showNotification(
            'Cannot add genes, no rows selected', type='warning'
          )
        }

        curr_genes <- genes_to_plot()
        selected_genes <- get_clicked_genes()

        if(all(selected_genes %in% curr_genes)){
          showNotification(
            'Selected genes already present in scratchpad, skipping', type='warning'
          )
        } else {
          g.new <- setdiff(selected_genes, curr_genes)
          if(length(g.new) > 0){
            n <- length(g.new)
            txt <- ifelse(n == 1, 'gene', 'genes')
            showNotification(
              paste('Adding', length(g.new), 'new', txt,
                    'to scratchpad'),
              type='warning'
            )
          }

          if(is.null(curr_genes)) selected <- selected_genes
          else selected <- unique(c(curr_genes, selected_genes))

          marker_info$clicked_genes <- selected
        }

      }) # observeEvent

      observeEvent(reset_genes(), {
        marker_info$clicked_genes <- NULL

        markers_proxy %>% selectRows(NULL)

      })

      output$selected_genes <- renderText({
        g <- get_clicked_genes()
        paste(length(g), 'genes selected:\n',
              paste(g, collapse=', '))
      })

      helpButtonServer(paste0(type, '_help'), size='l')
      downloadFileServer('tbl_dload', get_marker_tbl, type)

      return(
        reactive({
          list(
            filters=marker_info$filters, # only return the full levels
            g=marker_info$clicked_genes
          )
        })
      )
    } # function
  ) # moduleServer
} # server
