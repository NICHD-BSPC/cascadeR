#' Filter settings module ui
#'
#' @param id Input id
#'
#' @export
#'
subsetUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
       column(12, align='right',
        helpButtonUI(ns('subset_help'))
      ) # column
    ), # fluidRow

    tags$div(
      uiOutput(ns('filter_summary')),
      class='div-filter-list'
    ),

    wellPanel(
      bsCollapse(id=ns('filter_panel'),
        open='Add/edit filter',
        bsCollapsePanel('Add/edit filter',

          fluidRow(
            column(6, 'Type of filter'),
            column(6,
              selectInput(ns('filter_type'),
                          label=NULL,
                          choices=c('metadata', 'gene', 'selection'),
                          selected='')
            ) # column
          ), # fluidRow

          conditionalPanel(
            paste0('input["', ns('filter_type'), '"] == "metadata"'),
            fluidRow(
              column(6, 'Choose variable'),
              column(6,
                selectInput(ns('filter_var'),
                            label=NULL,
                            choices=NULL,
                            selected=NULL)
              ) # column
            ), # fluidRow

            uiOutput(ns('filter_var_menu')),

          ), # conditionalPanel

          conditionalPanel(
            paste0('input["', ns('filter_type'), '"] == "gene"'),
            fluidRow(
              column(6, 'Choose gene'),
              column(6,
                selectInput(ns('filter_gene'),
                            label=NULL,
                            choices=NULL,
                            selected=NULL)
              ) # column
            ), # fluidRow

            wellPanel(
              uiOutput(ns('gene_hist')),
              fluidRow(
                column(12, align='right',
                  sliderInput(ns('filter_gene_range'),
                              label=NULL,
                              min=0,
                              max=Inf,
                              step=0.1,
                              value=c(0, 0),
                              round=-2,
                              width='90%')
                ) # column
              ) # fluidRow
            ) # wellPanel

          ), # conditionalPanel

          conditionalPanel(
            paste0('input["', ns('filter_type'), '"] == "selection"'),
            fluidRow(
              column(6, 'Choose plot'),
              column(6,
                selectInput(ns('select_var'),
                            label=NULL,
                            choices=c('umap', 'spatial'))
              ) # column
            ), # fluidRow

            uiOutput(ns('select_var_menu')),
            br()

          ), # conditionalPanel

          fluidRow(
            column(12,
              align='center',
              actionButton(ns('add_filter_do'),
                           label='Add filter'
              ) # actionButton
            ) # column
          ) # fluidRow

        ) # bsCollapsePanel
      ), # bsCollapse

      fluidRow(
        column(6, align='right',
          actionButton(ns('save_filter'), 'Save filter set')
        ), # column

        column(6, align='left',
          actionButton(ns('load_filter'), 'Load filter set')
        ) # column
      ), # fluidRow

      br(),

      fluidRow(
        column(6, align='right',
          actionButton(ns('apply_filter'), 'Apply',
                       class='btn-primary')
        ), # column

        column(6, align='left',
          actionButton(ns('reset_filter_levels'), 'Reset',
                       class='btn-primary')
        ) # column
      ) # fluidRow
    ) # wellPanel

  ) # tagList
}

#' Filter settings module server
#'
#' @param id Input id
#' @param obj Cascade app object
#' @param args reactive list with 'assay' with name of selected assay
#' @param metadata_args reactive list with 'factor_levels' that has levels of
#'        categorical metadata & 'numeric_dist' that has distributions of numeric
#'        metadata
#' @param gene_choices reactive list with all genes present in object
#'
#' @export
#'
subsetServer <- function(id, obj, args, metadata_args, gene_choices){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      app_object <- reactive({
        validate(
          need(!is.null(obj$rds), '')
        )

        list(
          rds=obj$rds,
          obj_type=obj$obj_type,
          metadata=obj$metadata
        )
      })

      selected_points <- reactive({
        obj$selected_points
      })

      # reactive values to keep track of stuff
      filter_list <- reactiveValues(metadata=list(),
                                    gene=list(),
                                    selection=list(),
                                    order=c())

      cell_info <- reactiveValues(before=NULL,
                                  after=NULL)
      obj_details <- reactiveValues(assay=NULL, slot=NULL)
      all_genes <- reactiveValues(choices=NULL)
      filter_levels <- reactiveValues(
                          metadata=list(full=list(),
                                        current=list(),
                                        dist=list(full=list(), current=list())),
                          gene=list(full=list(),
                                    current=list(),
                                    dist=list(full=list(),
                                              current=list())),
                          selection=list()
                       )
      saved_filters <- reactiveValues(all=list())
      data_loaded <- reactiveValues(flag=0)

      reset_data <- function(){

        # reset reactive values
        filter_list$order <- c()
        filter_list$metadata <- list()
        filter_list$gene <- list()
        filter_list$selection <- list()
        saved_filters$all <- list() # TODO: load from disk/args

        filter_levels$metadata$full <- list()
        filter_levels$metadata$current <- list()
        filter_levels$metadata$dist$full <- list()
        filter_levels$metadata$dist$current <- list()
        filter_levels$gene$full <- list()
        filter_levels$gene$current <- list()
        filter_levels$gene$dist$full <- list()
        filter_levels$gene$dist$current <- list()
        filter_levels$selection <- list()

        all_genes$choices <- NULL
        cell_info$before <- NULL
        cell_info$after <- NULL
        obj_details$assay <- NULL
        obj_details$slot <- NULL

      }

      observeEvent(c(app_object()$rds, args()), {
        reset_data()

        validate(
          need(!is.null(app_object()$rds), '')
        )
        obj_type <- app_object()$obj_type

        mdata <- app_object()$metadata
        meta_cols <- colnames(mdata)

        # get cell barcodes and save
        cell_info$before <- rownames(mdata)
        cell_info$after <- cell_info$before

        # get available assay names & select
        # TODO: add to menu
        if(obj_type == 'seurat'){
          obj_details$assay <- args()$assay
          obj_assay_names <- names(app_object()$rds@assays)

          # get slot depending on assay selected
          # this can either be 'data' or 'scale.data'
          # - if 'scale.data' is present, this is used
          # TODO: move to config as, e.g. 'default_assay'
          if(inherits(app_object()$rds@assays[[ args()$assay ]], 'Assay5')){
            slot_names <- names(app_object()$rds@assays[[ args()$assay ]]@layers)
          } else {
            slot_names <- slotNames(app_object()$rds@assays[[ args()$assay ]])
          }

          if('scale.data' %in% slot_names){
            slot <- 'scale.data'
          } else {
            slot <- 'data'
          }

          obj_details$slot <- slot

        } else if(obj_type == 'anndata'){
          obj_details$assay <- 'X'

          obj_details$slot <- 'none'
        }

        filter_levels$metadata$full <- metadata_args()$factor_levels
        filter_levels$metadata$current <- metadata_args()$factor_levels
        filter_levels$metadata$dist$full <- metadata_args()$numeric_dist
        filter_levels$metadata$dist$current <- lapply(metadata_args()$numeric_dist,
                                                 function(x) get_limits(x$mids))

        updateSelectInput(session, 'filter_var',
                          choices=c('none',
                                    c(names(filter_levels$metadata$full),
                                      names(filter_levels$metadata$dist$full))))

        # update selection var menu
        if(obj_type == 'seurat'){
          if(any(grepl('Spatial', names(app_object()$rds))) |
             any(grepl('Xenium', names(app_object()$rds)))){
            choices=c('spatial', 'umap')
          } else {
            choices=c('umap')
          }
        } else if(obj_type == 'anndata'){
          if('spatial' %in% names(app_object()$rds$obsm)){
            choices <- c('spatial', 'umap')
          } else {
            choices <- c('umap')
          }
        }
        updateSelectInput(session, 'select_var',
                          choices=choices)
        data_loaded$flag <- data_loaded$flag + 1

        showNotification(
          'Loaded filter module ...'
        )
      })

      observeEvent(gene_choices(), {
        # save gene names
        all_genes$choices <- gene_choices()

        updateSelectizeInput(session, 'filter_gene',
                             choices=c('none', all_genes$choices),
                             selected='none',
                             server=TRUE)
      })

      #################### Gene filters ####################

      observeEvent(input$filter_gene, {
        obj_type <- app_object()$obj_type

        if(input$filter_gene == '' | input$filter_gene == 'none'){
          full <- c(0, Inf)
          current <- full
        } else {

          # if selected gene does not exist in filter_levels, compute
          # quantiles and add
          if(!input$filter_gene %in% names(filter_levels$gene$full)){

            # get data for gene from all cells
            if(obj_type == 'seurat'){
              if(inherits(app_object()$rds@assays[[ obj_details$assay ]], 'Assay5')){
                all_g <- rownames(app_object()$rds@assays[[ obj_details$assay ]]@features)
                ridx <- which(all_g %in% input$filter_gene)
                if(length(ridx) == 0){
                  showNotification(
                    paste0('Gene ', input$filter_gene, ' not found in "', obj_details$assay, '" assay. ',
                           'Please choose different assay in "Global settings" and retry'),
                    type='error'
                  )

                  validate(
                    need(input$filter_gene %in% all_g, '')
                  )
                }
                cidx <- which(rownames(app_object()$rds@assays[[ obj_details$assay ]]@cells) %in% cell_info$after)
                g <- app_object()$rds@assays[[ obj_details$assay ]]@layers[[ obj_details$slot ]][ridx,]
                gc <- g[cidx]
              } else {
                all_g <- rownames(app_object()$rds@assays[[ obj_details$assay ]])
                if(!input$filter_gene %in% all_g){
                  showNotification(
                    paste0('Gene ', input$filter_gene, ' not found in "', obj_details$assay, '" assay. ',
                           'Please choose different assay in "Global settings" and retry'),
                    type='error'
                  )

                  validate(
                    need(input$filter_gene %in% all_g, '')
                  )
                }
                g <- slot(app_object()$rds@assays[[ obj_details$assay ]], 'data')[input$filter_gene, ]
                # get data for gene from current selection
                gc <- g[cell_info$after]
              }
            } else if(obj_type == 'anndata'){
              g <- app_object()$rds[[ obj_details$assay ]][,input$filter_gene]

              # get data for gene from current selection
              gc <- g[cell_info$after]
            }

            full <- get_limits(g)

            # get data for gene from current selection
            current <- get_limits(gc)

            # get distributions
            predist <- hist(g, breaks=20, plot=FALSE)
            postdist <- hist(gc, breaks=predist$breaks, plot=FALSE)
            filter_levels$gene$dist$full[[ input$filter_gene ]] <- data.frame(mids=postdist$mids, counts=postdist$counts)
            filter_levels$gene$dist$current[[ input$filter_gene ]] <- filter_levels$gene$dist$full[[ input$filter_gene ]]

            # if this is a new gene, then add to filter_levels
            filter_levels$gene$full[[ input$filter_gene ]] <- full
            filter_levels$gene$current[[ input$filter_gene ]] <- current
          } else {
            # for pre-existing genes, get values from filter_levels
            full <- filter_levels$gene$full[[ input$filter_gene ]]
            current <- filter_levels$gene$current[[ input$filter_gene ]]
          }

        }

        updateSliderInput(session, 'filter_gene_range',
                    min=min(full),
                    max=max(full),
                    value=c(min(current), max(current))
        )

      })

      output$gene_hist <- renderUI({

        if(input$filter_gene == '' | input$filter_gene == 'none'){
          NULL
        } else {
          output$plot1 <- renderPlot({

          validate(
            need(input$filter_gene %in% names(filter_levels$gene$dist$current), '')
          )

          df <- filter_levels$gene$dist$current[[ input$filter_gene ]]

          p <- ggplot(df, aes(x=.data$mids, y=.data$counts)) +
            geom_bar(stat='identity') +
            xlab('') + ylab('') +
            theme_bw() +
            theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  axis.title.x=element_blank())
          p

          })

          plotOutput(ns('plot1'), height='100px')

        }

      })

      # observer to update slider when filter levels change
      # even if selected gene is same
      observeEvent(filter_levels$gene, {
        req(input$filter_gene)

        current <- filter_levels$gene$current[[ input$filter_gene ]]

        if(!is.null(current)){
          updateSliderInput(session, 'filter_gene_range',
                            value=c(min(current), max(current)))
        }
      })

      ####################### Selection filters ########################

      output$select_var_menu <- renderUI({
        validate(
          need(input$select_var != 'none', '')
        )
        np <- length(selected_points()[[ input$select_var ]])
        tagList(
          paste(np, 'cells'),
        )
      })

      ####################### Save filters ########################

      observeEvent(input$save_filter, {
        if(length(filter_list$order) == 0){
          showNotification(
            'No filters added! Must add at least one filter before saving.',
            type='error'
          )

          validate(
            need(length(filter_list$order) > 0, '')
          )
        }

        save_num <- length(saved_filters$all)
        showModal(
          modalDialog(
            title='Save filter',
            tagList(
              fluidRow(
                column(6, 'Filter name'),
                column(6,
                  textInput(ns('save_filter_name'),
                            label=NULL,
                            value=paste0('filter', (save_num + 1))
                  ) # textInput
                ) # column
              ), # fluidRow
              tags$i('Hint: use a short meaningful name here')
            ), # tagList
            footer=tagList(
              actionButton(ns('save_filter_do'), label='OK'),
              modalButton('Cancel')
            ),
            easyClose=TRUE
          )
        )
      })

      observeEvent(input$save_filter_do, {
        fname <- input$save_filter_name
        if(!fname %in% names(saved_filters$all)){
          m <- intersect(names(filter_levels$metadata$current),
                         filter_list$order)
          mn <- intersect(names(filter_levels$metadata$dist$current),
                         filter_list$order)
          g <- intersect(names(filter_levels$gene$current),
                         filter_list$order)
          s <- intersect(names(filter_levels$selection),
                         filter_list$order)
          fl <- list(metadata=c(filter_levels$metadata$current[ m ],
                                filter_levels$metadata$dist$current[ mn ]),
                     gene=filter_levels$gene$current[ g ],
                     selection=filter_levels$selection[ s ],
                     order=filter_list$order)
          saved_filters$all[[ fname ]] <- fl

          removeModal()
        } else {
          showNotification(
            'Filter name already exists!',
            type='error'
          )
        }
      })

      ####################### Load filter ########################

      observeEvent(input$load_filter, {
        fnames <- names(saved_filters$all)
        showModal(
          modalDialog(
            title='Load filter',
            tagList(
              fluidRow(
                column(6, 'Choose filter'),
                column(6,
                  selectInput(ns('load_filter_name'),
                              label=NULL,
                              choices=c('choose one', fnames)
                  ) # selectInput
                ) # column
              ) # fluidRow
            ), # tagList
            footer=tagList(
              actionButton(ns('load_filter_do'), label='OK'),
              modalButton('Cancel')
            ),
            easyClose=TRUE
          )
        )
      })

      # check for current filters and show warning before overwriting
      observeEvent(input$load_filter_do, {
        if(length(filter_list$order) > 0){
          showModal(
            modalDialog(
              title=NULL,
              tags$b('Warning: This will overwrite current filter settings. Are you sure you want to proceed?'),
              footer=tagList(
                actionButton(ns('load_now'), 'OK'),
                modalButton('Cancel')
              ),
              easyClose=TRUE
            ) # modalDialog
          ) # showModal
        } else {
          load_filter(input$load_filter_name)

          showModal(
            modalDialog(
              'Make sure to click "Apply" in Filters menu to apply loaded filters!',
              footer=modalButton('OK'),
              easyClose=TRUE
            )
          )

        }
      }) # observeEvent

      observeEvent(input$load_now, {
        load_filter(input$load_filter_name)

        showModal(
          modalDialog(
            'Make sure to click "Apply" in Filters menu to apply loaded filters!',
            footer=modalButton('OK'),
            easyClose=TRUE
          )
        )

      })

      # function to load filter
      load_filter <- function(fname){
        # TODO: add check for current filters
        fl <- saved_filters$all[[ fname ]]
        filter_list$metadata <- list()
        filter_list$gene <- list()
        filter_list$selection <- list()
        filter_list$order <- fl$order
        obj_type <- app_object()$obj_type

        # rebuild filter lists from loaded filter
        # and update current filter_levels
        fm <- list()
        fg <- list()
        fs <- list()
        for(key in fl$order){
          if(key %in% names(fl$metadata)){
            mcol <- fl$metadata[[ key ]]
            if(key %in% names(filter_levels$metadata$full)){
              full <- filter_levels$metadata$full[[ key ]]
              dropped <- setdiff(full, mcol)
              f <- c(key, dropped)
              filter_levels$metadata$current[[ key ]] <- mcol
            } else if(key %in% names(filter_levels$metadata$dist$full)){
              mcol <- get_limits(mcol)
              f <- c(key, mcol)
              filter_levels$metadata$dist$current[[ key ]] <- mcol
            }
            fm[[ key ]] <- f
          } else if(key %in% names(fl$gene)){

            full <- filter_levels$gene$full[[ key ]]
            gcol <- fl$gene[[ key ]]
            gcol <- get_limits(gcol)

            f <- c(key, gcol)

            fg[[ key ]] <- f
            filter_levels$gene$current[[ key ]] <- gcol

            # get gene data
            if(obj_type == 'seurat'){
              if(inherits(app_object()$rds@assays[[ obj_details$assay ]], 'Assay5')){
                all_g <- rownames(app_object()$rds@assays[[ obj_details$assay ]]@features)
                ridx <- which(all_g %in% input$filter_gene)
                g <- app_object()$rds@assays[[ obj_details$assay ]]@layers[[ obj_details$slot ]][ridx,]
              } else {
                g <- slot(app_object()$rds@assays[[ obj_details$assay ]], 'data')[key, ]
              }
            } else if(obj_type == 'anndata'){
              g <- app_object()$rds[[ obj_details$assay ]][, key]
            }

            # save distribution
            predist <- hist(g, breaks=20, plot=FALSE)
            postdist <- hist(g[g > gcol[1] & g < gcol[2]],
                             breaks=predist$breaks,
                             plot=FALSE)
            filter_levels$gene$dist$full[[ key ]] <- data.frame(mids=predist$mids, counts=predist$counts)
            filter_levels$gene$dist$current[[ key ]] <- data.frame(mids=postdist$mids, counts=postdist$counts)

          } else if(key %in% names(fl$selection)){
            fs[[ key ]] <- key
            filter_levels$selection[[ key ]] <- fl$selection[[ key ]]
          }
        }

        filter_list$metadata <- fm
        filter_list$gene <- fg
        filter_list$selection <- fs
        filter_list$order <- fl$order

      }

      #################### Metadata filters ####################

      filter_var_ui <- eventReactive(c(input$filter_var,
                                       filter_levels$metadata$dist), {

        if(input$filter_var == '' | input$filter_var == 'none'){
          NULL
        } else if(input$filter_var %in% names(filter_levels$metadata$full)){
          full <- filter_levels$metadata$full[[ input$filter_var ]]
          current <- filter_levels$metadata$current[[ input$filter_var ]]

          # build different uis for factor/character or numeric variables
          lvls <- unique(full)
          selected <- unique(current)

          tagList(
            selectizeInput(ns('filter_var_levels'),
                           label=NULL,
                           choices=lvls,
                           selected=selected,
                           multiple=TRUE),

            fluidRow(
              align='center',
              #style='margin-bottom: 20px;',
              column(12,
                splitLayout(cellWidths=c('50%', '50%'),
                    actionButton(ns('filter_none'),
                                 label='none',
                                 icon=shiny::icon('minus'),
                                 class='select-buttons'),
                    actionButton(ns('filter_all'),
                                 label='all',
                                 icon=shiny::icon('check'),
                                 class='select-buttons')
                ) # splitLayout
              ) # column
            ) # fluidRow
          ) # tagList

        } else {

          full_df <- filter_levels$metadata$dist$full[[ input$filter_var ]]
          current <- filter_levels$metadata$dist$current[[ input$filter_var ]]

          p <- ggplot(full_df, aes(x=.data$mids, y=.data$counts)) +
            geom_bar(stat='identity') +
            xlab('') + ylab('') +
            theme_bw() +
            theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank())

          output$plot2 <- renderPlot({ p })
          tagList(
            plotOutput(ns('plot2'), height='100px'),

            fluidRow(
              column(12, align='right',
                sliderInput(ns('filter_var_range'),
                            label=NULL,
                            min=min(full_df$mids),
                            max=max(full_df$mids),
                            value=c(min(current), max(current)),
                            round=-2,
                            width='90%')
              ) # column
            ) # fluidRow
          )

        }

      })

      # observer to update slider/selectInput when filter levels change
      # even if selected variable is same
      observeEvent(filter_levels$metadata, {
        req(input$filter_var)

        if(input$filter_var %in% names(filter_levels$metadata$full)){
          full <- filter_levels$metadata$full[[ input$filter_var ]]
          current <- filter_levels$metadata$current[[ input$filter_var ]]

          if(!is.null(current)){
            updateSelectizeInput(session, 'filter_var_levels',
                                 choices=full,
                                 selected=current,
                                 server=TRUE)
          }
        } else if(input$filter_var %in% names(filter_levels$metadata$dist$full)){
          current <- filter_levels$metadata$dist$current[[ input$filter_var ]]
          updateSliderInput(session, 'filter_var_range',
                            value=c(min(current), max(current)))
        }
      })

      output$filter_var_menu <- renderUI({
        tl <- filter_var_ui()
        if(!is.null(tl)){
          wellPanel(
            tl
          )
        }
      })

      # reset filter levels
      observeEvent(input$reset_filter_levels, {
        filter_levels$metadata$current <- filter_levels$metadata$full
        filter_levels$metadata$dist$current <- lapply(filter_levels$metadata$dist$full,
                                                 function(x) get_limits(x$mids))

        if(input$filter_var %in% names(filter_levels$metadata$full)){
          lvls <- filter_levels$metadata$full[[ input$filter_var ]]
          updateSelectizeInput(session, 'filter_var_levels',
                               choices=lvls,
                               selected=lvls,
                               server=TRUE)
        } else if(input$filter_var %in% names(filter_levels$metadata$dist$full)){
          full <- filter_levels$metadata$dist$full[[ input$filter_var ]]
          updateSliderInput(session, 'filter_var_range',
                            value=c(min(full$mids), max(full$mids)))
        }

        filter_levels$gene$current <- filter_levels$gene$full
        lvls <- filter_levels$gene$full[[ input$filter_gene ]]
        lvls <- unname(lvls)
        updateSliderInput(session, 'filter_gene_range',
                          value=c(lvls[1], lvls[length(lvls)]))

        cell_info$after <- cell_info$before

        filter_list$gene <- list()
        filter_list$metadata <- list()
        filter_list$selection <- list()
        filter_levels$selection <- NULL
        filter_list$order <- c()

        filter_levels$gene$dist$current <- filter_levels$gene$dist$full
      })

      # select all variable levels
      observeEvent(input$filter_all, {
        selected <- filter_levels$metadata$full[[ input$filter_var ]]
        updateSelectizeInput(session, 'filter_var_levels',
                             choices=selected,
                             selected=selected,
                             server=TRUE)
      })

      # select no variable levels
      observeEvent(input$filter_none, {
        selected <- filter_levels$metadata$full[[ input$filter_var ]]
        updateSelectizeInput(session, 'filter_var_levels',
                             choices=selected,
                             selected='',
                             server=TRUE)
      })

      ######################### Add filter #########################

      observeEvent(input$add_filter_do, {
        if(input$filter_type == 'metadata'){
          key <- input$filter_var

          # if factor/character variable, save remaining levels
          if(key %in% names(filter_levels$metadata$full)){
            full_col <- filter_levels$metadata$full[[ key ]]
            col <- input$filter_var_levels
            if(is.null(col)){
              showNotification(
                'Must select at least one level!',
                type='warning'
              )
              validate(
                need(!is.null(col) | length(col) > 0,
                     'Must select at least one level to plot')
              )
            }
            dropped <- setdiff(full_col, col)
            f <- c(key, dropped)
            filter_levels$metadata$current[[ key ]] <- col

          } else {
            # if numeric variable, save current/selected range
            col <- input$filter_var_range
            f <- c(key, col)
            filter_levels$metadata$dist$current[[ key ]] <- col

          }

          # if filter is new, add to filter_list
          if(!key %in% names(filter_list$metadata)){

            filter_list$metadata[[ key ]] <- f
          } else {
            new <- paste(f, collapse='|')
            curr <- paste(filter_list$metadata[[ key ]], collapse='|')
            if(new == curr){
              showNotification(
                'Specified filter already exists. Skipping',
                type='warning'
              )
            } else {
              filter_list$metadata[[ key ]] <- f
            }
          }
        } else if(input$filter_type == 'gene'){
          # this is identical to metadata numeric variable

          key <- input$filter_gene
          full_col <- filter_levels$gene$full[[ key ]]

          col <- input$filter_gene_range
          f <- c(key, col)

          # if filter is new, add to filter_list
          if(!key %in% names(filter_list$gene)){
            filter_levels$gene$current[[ key ]] <- col

            filter_list$gene[[ key ]] <- f

          } else {
            new <- paste(f, collapse='|')
            curr <- paste(filter_list$gene[[ key ]], collapse='|')
            if(new == curr){
              showNotification(
                'Specified filter already exists. Skipping',
                type='warning'
              )
            } else {
              filter_list$gene[[ key ]] <- f
            }
          }
        } else if(input$filter_type == 'selection'){
          # current number of selections
          nsel <- length(filter_list$selection)
          key <- paste(input$select_var, 'selection', nsel + 1)

          # if filter is new, add to filter_list
          if(!key %in% filter_list$selection){
            bc <- selected_points()[[ input$select_var ]]
            filter_levels$selection[[ key ]] <- bc

            filter_list$selection[[ key ]] <- c(key, paste(length(bc), 'cells'))

          } else {
            showNotification(
              'Specified filter already exists. Skipping',
              type='warning'
            )
          }
        }

        # add key to filter_list order
        if(!key %in% filter_list$order)
          filter_list$order <- c(filter_list$order, key)
      })

      #################### Apply current filters ####################

      apply_filters <- eventReactive(c(data_loaded$flag,
                                       all_genes$choices,
                                       input$reset_filter_levels,
                                       input$apply_filter), {

        validate(
          need(!is.null(app_object()$rds), 'Waiting for selection')
        )

        validate(
          need(all(c(names(filter_list$metadata), names(filter_list$gene), names(filter_list$selection)) %in% filter_list$order), '')
        )

        obj <- app_object()$rds
        obj_type <- app_object()$obj_type

        mdata <- app_object()$metadata
        mdata <- data.table::as.data.table(mdata, keep.rownames=T)
        mdata_orig <- mdata

        if(length(filter_list$order) > 0){
          for(col in filter_list$order){

            idx <- 1:nrow(mdata)
            if(col %in% names(filter_list$metadata)){
              mcol <- mdata[[ col ]]

              if(col %in% names(filter_levels$metadata$current)){
                # apply metadata filters
                tmp <- filter_levels$metadata$current[[ col ]]

                validate(
                  need(!is.null(tmp),
                       'Must select at least one level to plot')
                )
                #if(is.character(tmp) | is.factor(tmp)){
                tmp_idx <- mcol %in% tmp
              } else if(col %in% names(filter_levels$metadata$dist$current)){
                tmp <- filter_levels$metadata$dist$current[[ col ]]
                tmp_idx <- mcol >= min(tmp) & mcol <= max(tmp)
              }
              idx <- intersect(idx, which(tmp_idx))
            } else if(col %in% names(filter_list$gene)){
              tmp <- filter_levels$gene$current[[ col ]]
              validate(
                need(!is.null(tmp),
                     'Must select at least one level to plot')
              )

              # get gene data
              if(obj_type == 'seurat'){
                if(inherits(app_object()$rds@assays[[ obj_details$assay ]], 'Assay5')){

                  all_g <- rownames(app_object()$rds@assays[[ obj_details$assay ]]@features)
                  ridx <- which(all_g %in% col)
                  gcol <- app_object()$rds@assays[[ obj_details$assay ]]@layers[[ obj_details$slot ]][ridx,]
                } else {
                  gcol <- slot(app_object()$rds@assays[[ obj_details$assay ]], 'data')[col, ]
                }
              } else if(obj_type == 'anndata'){
                gcol <- app_object()$rds[[ obj_details$assay ]][, col]
              }

              tmp_idx <- gcol >= tmp[1] & gcol <= tmp[2]

              # NOTE: currently filters are added using
              # intersect or logical *AND*
              idx <- intersect(idx, which(tmp_idx))
            } else if(col %in% names(filter_list$selection)){
              tmp <- filter_levels$selection[[ col ]]
              validate(
                need(!is.null(tmp),
                     'Must select at least one cell to plot')
              )
              tmp_idx <- mdata$rn %in% tmp

              # NOTE: currently filters are added using
              # intersect or logical *AND*
              idx <- intersect(idx, which(tmp_idx))

            }

            # update/subset metadata & obj dfs
            mdata <- mdata[idx, ]
          }

          # get barcodes from remaining data
          bc <- mdata$rn
          if(obj_type == 'seurat'){
            idx <- colnames(obj) %in% bc
          } else if(obj_type == 'anndata'){
            idx <- rownames(obj) %in% bc
          }

          validate(
            need(sum(idx) > 0,
                 'No cells left! Please change filter criteria')
          )

          if(obj_type == 'seurat'){
            cell_info$after <- colnames(obj)[idx]
          } else if(obj_type == 'anndata'){
            cell_info$after <- rownames(obj)[idx]
          }

        }

        ################ update var levels after filtering ############

        bc <- cell_info$after
        mdata <- mdata_orig
        idx <- mdata$rn %in% bc
        mdata_sel <- mdata[idx,]

        if(length(cell_info$after) != length(cell_info$before)){
        update <- FALSE
        for(type in c('metadata', 'gene')){
          for(col in names(filter_levels[[ type ]]$full)){
            if(type == 'metadata'){
              tmp <- mdata_sel[, col, with=FALSE]

              if(is.numeric(tmp)){
                pretmp <- mdata[, col, with=FALSE]

                # update distributions
                predist <- hist(pretmp, breaks=20, plot=FALSE)
                postdist <- hist(tmp, breaks=predist$breaks,
                                 plot=FALSE)
                df <- data.frame(mids=postdist$mids,
                                 counts=postdist$counts)
                filter_levels[[ type ]]$dist[[ col ]] <- df
              }
            } else if(type == 'gene'){
              if(obj_type == 'seurat'){

                if(inherits(app_object()$rds@assays[[ obj_details$assay ]], 'Assay5')){
                  all_g <- rownames(app_object()$rds@assays[[ obj_details$assay ]]@features)
                  ridx <- which(all_g %in% col)
                  cidx <- which(rownames(app_object()$rds@assays[[ obj_details$assay ]]@cells) %in% bc)
                  pretmp <- app_object()$rds@assays[[ obj_details$assay ]]@layers[[ obj_details$slot ]][ridx,]
                  tmp <- pretmp[cidx]
                } else {
                  pretmp <- slot(app_object()$rds@assays[[ obj_details$assay ]], 'data')[col, ]
                  tmp <- pretmp[bc]
                }
              } else if(obj_type == 'anndata'){
                pretmp <- app_object()$rds[[ obj_details$assay ]][, col]
                tmp <- pretmp[bc]

                pretmp <- t(pretmp)
                tmp <- t(tmp)
              }

              # update distributions
              predist <- hist(pretmp, breaks=20, plot=FALSE)
              postdist <- hist(tmp, breaks=predist$breaks,
                               plot=FALSE)
              filter_levels$gene$dist$current[[ col ]] <- data.frame(mids=postdist$mids,
                                                                     counts=postdist$counts)
            }

            current <- filter_levels[[ type ]]$current[[ col ]]

            # 1: factor/character variable
            if(is.character(tmp) | is.factor(tmp)){
              full <- unique(tmp)
              full <- full[order(full)]

              if(any(!current %in% full)){
                update <- TRUE
                filter_levels[[ type ]]$current[[ col ]] <- intersect(current, full)
              }
            } else if(is.numeric(tmp)){
              tmp <- get_limits(tmp)
              full <- c(min(tmp), max(tmp))

              if(current[1] < full[1]){
                update <- TRUE
                current[1] <- full[1]
              }

              if(current[2] > full[2]){
                update <- TRUE
                current[2] <- full[2]
              }

              filter_levels[[ type ]]$current[[ col ]] <- current
            }
          }
        }

          if(update){
            showNotification(
              'Updating range to match remaining cells',
              type='warning'
            )
          }
        }

        return(cell_info$after)
      })

      # Style rank list labels
      #
      # @param x label, vector, 1st element: key, rest depends on filter type.
      #          For metadata, dropped levels, or for numeric filters, min & max.
      #          For gene, min & max.
      #          TODO: selection?
      # @param type string, can be 'metadata', 'gene' or 'selection'.
      #
      style_rank_list_labels <- function(x, type){
        key <- x[1]
        rem <- x[2:length(x)]

        if(type == 'metadata'){
          if(key %in% names(filter_levels$metadata$full)){
            all <- filter_levels$metadata$full[[ key ]]
            if(length(rem) < (length(all) - length(rem))){
              connector <- 'does not contain'
            } else {
              connector <- 'contains only'
              rem <- setdiff(all, rem)
            }

            tags$div(
              span(key, style='font-weight: bold; color: red;'),
              connector,
              em(paste(rem, collapse=', '))
            )
          } else {
            tags$div(
              em(rem[1]),
              '<=',
              span(key, style='font-weight: bold; color: red;'),
              '<=',
              em(rem[2])
            )
          }
        } else if(type == 'gene'){
          tags$div(
            em(rem[1]),
            '<=',
            span(key, style='font-weight: bold; color: blue;'),
            '<=',
            em(rem[2])
          )
        } else if(type == 'selection'){
          tags$div(
            span(key, style='font-weight: bold; color: green;'),
            ':',
            rem[1]
          )
        } else {
          x
        }
      }

      output$filter_summary <- renderUI({
        labels <- lapply(filter_list$order, function(x){
                    if(x %in% names(filter_list$metadata)){
                      style_rank_list_labels(filter_list$metadata[[ x ]], type='metadata')
                    } else if(x %in% names(filter_list$gene)){
                      style_rank_list_labels(filter_list$gene[[ x ]], type='gene')
                    } else {
                      style_rank_list_labels(filter_list$selection[[ x ]], type='selection')
                    }
                  })

        tagList(
          paste0('Before filtering: ',
                 length(cell_info$before),
                 ' cells'),

          fluidRow(
            column(12, align='center',
              rank_list(
                text='Current filters',
                labels=labels,
                input_id='filter_list_rank'
              )
            )
          ),

          paste0('After filtering: ',
                 length(cell_info$after),
                 ' cells')

        )
      })

      helpButtonServer('subset_help', size='l')

      return(reactive(apply_filters()))
    } # function
  ) # moduleServer
} # subsetServer
