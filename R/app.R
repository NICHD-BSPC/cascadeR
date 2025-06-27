#' Cascade
#'
#' Interactive shiny dashboard for exploring single-cell or spatial RNA-Seq data.
#'
#' @param credentials path to encrypted sqlite db with user credentials.
#' @param passphrase passphrase for credentials db.
#' @param enable_admin if TRUE, admin view is shown. Note, this is only available
#'        if credentials have sqlite backend.
#' @param ... parameters passed to shinyApp() call
#'
#' @export
run_cascade <- function(credentials=NULL, passphrase=NULL, enable_admin=TRUE, ...){

  ## read config yaml
  cfg <- get_config()

  oopt <- options(spinner.type = 4)

  # reset to previous options on exit
  on.exit(options(oopt))

  if(!is.null(credentials)){
    # check to see if shinymanager is available
    if(!requireNamespace('shinymanager', quietly=TRUE)){
      stop(
        paste('Login functionality using SQL/sqlite credentials requires "shinymanager".',
              'Please install using "install.packages(\'shinymanager\')"'),
        .call=FALSE
      )
    } else if(!file.exists(credentials)){
      stop(
        paste0('Credentials specified, but file not found: "',
               credentials, '"')
      )
    }
  }

  ui <- fluidPage(
    useShinyjs(),
    introjsUI(),
    theme=shinythemes::shinytheme('united'),

    # custom CSS styles
    tags$head(
      tags$style(
        HTML(cfg$style$global)
      )  # tags$style
    ), # tags$head

    titlePanel(
      fluidRow(
        column(1, span()),
        column(9,

          tags$div(
            HTML(
              paste0(
                'ca',

                # using primary color from united theme
                span('sc', style='color: #e95420; font-weight: bold;'),

                'ade'
              ) # paste
            ), # HTML

            align='center',
            style='font-family: Helvetica; font-size: 40px; margin-top: 20px;'

          ) # tags$div

        ), # column

        column(2,
          actionButton('intro', 'Take a tour',
                       style='margin-top: 20px;',
                       class='btn-primary')
        ) # column
      ), # fluidRow
      windowTitle='Cascade'
    ), # titlePanel

    sidebarPanel(width=1, id='sidebar',
      fluidRow(align='center',
        introBox(
          dropdownButton(
            tagList(

              fluidRow(
                column(12, align='right',
                  helpButtonUI('global_help')
                ) # column
              ), # fluidRow
              div(id='assay_menu',
                selectInput('assay', label='Assay',
                            choices=NULL,
                            selected=NULL)
              ), # div

              selectInput('dimred', label='Reductions',
                          choices=NULL,
                          selected=NULL),

              selectInput('grp_by', label='Cluster by',
                          choices=NULL,
                          selected=NULL),

              conditionalPanel("input.mode == 'Cell Markers'",
                markerTableUI('marker_tbl_tab', panel='global')
              ), # conditionalPanel

              fluidRow(align='center',
                column(12,
                  actionButton('plt_do', 'Reload',
                               class='btn-primary',
                               style='margin-bottom: 10px;')
                ) # column
              ) # fluidRow

            ),


            icon = icon("sliders-h"), width = "400px",
            size='sm',

            tooltip = tooltipOptions(title = "Global settings")

          ), # dropdownButton

          data.step=5,
          data.intro='Click this button to access global settings'
        ), # introBox
        br(),

        introBox(
          dropdownButton(
            tagList(

              conditionalPanel('input.mode == "Summary"',

                summaryUI('summ_tab', panel='sidebar')

              ),

              conditionalPanel('input.mode == "Metadata Viewer"',

                clustSummaryUI('clust_summ_tab', panel='sidebar')

              ), # conditionalPanel


              conditionalPanel('input.mode == "Cluster Tree"',

                clustreeUI('clustree', panel='sidebar')

              ), # conditionalPanel

              conditionalPanel('input.mode == "Cell Markers"',

                markerTableUI('marker_tbl_tab', panel='sidebar')

              ), # conditionalPanel

              conditionalPanel('input.mode == "Marker Plots"',

                markerPlotUI('mrkrplt_tab', panel='sidebar')

              ), # conditionalPanel

              conditionalPanel('input.mode == "Cell Embeddings"',

                dimredUI('dimred_tab', panel='sidebar')

              ), # conditionalPanel

              conditionalPanel(condition = "input.mode == 'Settings'",
                uiOutput('settings_sidebar')
              ) # conditionalPanel

            ), # tagList

            icon = icon("gear"), width = "400px",
            size='sm',

            tooltip = tooltipOptions(title = "Settings")

          ), # dropdownButton

          data.step=6,
          data.intro='Click this button to access tab-specific settings'
        ), # introBox
        br(),

        introBox(
          dropdownButton(
            tagList(

              subsetUI('subset_tab')

            ), # tagList

            icon = icon("filter"), width = "450px",

            size='sm',
            tooltip = tooltipOptions(title = "Filters")
          ), # dropdownButton
          data.step=7,
          data.intro='Click this button to access filter menu'
        ), # introBox

        br(),

        introBox(
          dropdownButton(
            tagList(

              fluidRow(
                 column(6, strong('Gene scratchpad')),
                 column(6, align='right',
                  helpButtonUI('gene_scratchpad_help')
                ) # column
              ), # fluidRow

              selectizeInput('gene.to.plot',
                           label=NULL,
                           choices=NULL, selected=NULL,
                           options=list(create=TRUE),
                           multiple=TRUE
              ), # selectizeInput

              actionButton('reset.genes', label='Reset',
                           class='btn-primary'
              ) # actionButton

            ), # tagList

            icon = icon("clipboard"), width = "400px",

            size='sm',
            tooltip = tooltipOptions(title = "Gene scratchpad")

          ), # dropdownButton
          data.step=8,
          data.intro='Keep track of your favorite genes with the "Gene scratchpad" here'
        ), # introBox
        br(),

        introBox(
          dropdownButton(
            tagList(
              conditionalPanel("input.mode == 'Cell Embeddings'",
                dimredUI('dimred_tab', panel='selection')
              ), # conditionalPanel

              conditionalPanel("input.mode == 'Cell Markers'",
                markerTableUI('marker_tbl_tab', panel='selection')
              ), # conditionalPanel

              conditionalPanel("input.mode == 'Marker Plots'",
                markerPlotUI('mrkrplt_tab', panel='selection')
              ), # conditionalPanel

              conditionalPanel("input.mode != 'Cell Embeddings' && input.mode != 'Cell Markers' && input.mode != 'Marker Plots'",
                'No selection settings available for this tab'
              ) # conditionalPanel
            ), # tagList

            icon = icon("hand-pointer", class='fa-solid'), width = "300px",

            size='sm',
            tooltip = tooltipOptions(title = "Selection settings")
          ), # dropdownButton
          data.step=9,
          data.intro='Settings for selecting points on UMAP/spatial plots or marker tables can be found here.'
        ) # introBox

      ) # fluidRow
    ), # sidebarPanel

    mainPanel(width=11, id='mainpanel',
      introBox(
      tabsetPanel(type='tabs',
                  id='mode',

        tabPanel('Load data',
          fluidRow(
            column(3,
              introBox(
                selectizeInput('proj',
                               label=h5('Available projects'),
                               choices=NULL,
                               selected=NULL),
                data.step=1,
                data.intro='First, select a project from this menu'
              ), # introBox
              introBox(
                selectizeInput('analysis',
                               label=h5('Available analyses'),
                               choices=NULL,
                               selected=NULL),
                data.step=2,
                data.intro='Next, choose an analysis here. If there is only one analysis for the selected project, it is selected automatically.'
              ), # introBox
              introBox(
                actionButton('load_do', label='Go!',
                             class='btn-primary'),
                data.step=3,
                data.intro='Finally, click this button to load the data.'
              ) # introBox
            ), # column
            column(9, style='margin-top: 20px',
              DTOutput('analysis_desc')
            )
          ) # fluidRow

        ), #tabPanel

        tabPanel('Summary',
          summaryUI('summ_tab', panel='main')
        ), # tabPanel

        tabPanel('Cell Embeddings',

          dimredUI('dimred_tab', panel='main')

        ), # tabPanel

        tabPanel('Metadata Viewer',

          clustSummaryUI('clust_summ_tab', panel='main')

        ), #tabPanel

        tabPanel('Cluster Tree',

          clustreeUI('clustree', panel='main')

        ), # tabPanel

        tabPanel('Cell Markers',

          markerTableUI('marker_tbl_tab', panel='main')

        ), # tabPanel

        tabPanel('Marker Plots',

          markerPlotUI('mrkrplt_tab', panel='main')

        ), # tabPanel

        tabPanel('Settings',
          uiOutput('settings_main')
        ) # tabPanel settings

      ), # tabsetPanel
        data.step=4,
        data.intro='Once the data is loaded, use these tabs to navigate analysis results'
      ) # introBox
    ) # mainPanel
  ) # ui

  # add custom login page, shown only if credentials are specified
  if(!is.null(credentials)){
    ui <- shinymanager::secure_app(
            ui,

            # customize login page
            tags_top =
              tags$div(
                HTML(
                  paste0(
                  'ca',
                  span('sc', style='color: #e95420;'), # primary color from united theme
                  'ade'
                  ) # paste0
                ), # HTML
                align='center',
                style='font-family: Helvetica; font-size: 40px;'
              ), # tags$div

            # add information on bottom ?
            tags_bottom = tags$div(
              tags$p(
                "For any question, please  contact cascade authors",
              )
            ),

            enable_admin=enable_admin,
            theme=shinytheme('united')
          )
  }


  server <- function(input, output, session){

    #################### authentication ####################

    # check_credentials directly on sqlite db
    if(!is.null(credentials)){
      res_auth <- shinymanager::secure_server(
        check_credentials = shinymanager::check_credentials(
            db=credentials,
            passphrase=passphrase
        )
      )
    } else {
      res_auth <- NULL
    }

    user_details <- reactiveValues(username=NULL, admin=FALSE)

    observeEvent(res_auth, {

      req(res_auth$user)

      user_details$username <- res_auth$user
      user_details$admin <- res_auth$user_info
    })

    ######################### Intro #########################

    observeEvent(input$intro, {

      introjs(session,
              options = list("nextLabel"="Next",
                             "prevLabel"="Back")
              )

    }) # observeEvent

    observeEvent(input$intro_done, {
      showModal(
        modalDialog(
          "Good luck!",
          easyClose=TRUE
        )
      )

      Sys.sleep(3)
      removeModal()
    })

    ################## Admin panel ##########################

    settings <- settingsServer('settings',
                               details=reactive({
                                 list(
                                   username=user_details$username,
                                   where=input$shinymanager_where
                                  )
                               }),
                               depth=3,
                               end_offset=1,
                               assay_fun=function(x) basename(dirname(x)),
                               config
                               )

    assay_list <- reactiveValues(l=NULL)
    load_time <- reactiveValues(t=NULL)

    # update assay list
    observeEvent(settings(), {
      l <- settings()

      assay_list$l <- l$assay_list
      project_info$descriptions <- l$project_descriptions

      validate(
         need(!is.null(l$assay_list), 'No projects found')
      )
      # arrange in alphabetical order to list in menu
      order_idx <- order(names(assay_list$l))

      updateSelectizeInput(session, 'proj',
                           choices=c('Choose one', names(l$assay_list)[order_idx]))

      if(l$reload_parent) session$reload()

      # hide admin tab if user not in admin group
      if(!l$is_admin){
          hideTab(inputId = 'mode', target='Settings')
      }
      load_time$t <- Sys.time()
    })

    output$settings_main <- renderUI({
      settingsUI('settings', panel='main', username=reactive({ user_details$username }))
    })

    output$settings_sidebar <- renderUI({
      settingsUI('settings', panel='sidebar', username=reactive({ user_details$username }))
    })


    ################# Set up internal tracking vars ##################

    # reactive values to save app object
    app_object <- reactiveValues(rds=NULL,
                                 obj_type=NULL,
                                 metadata=NULL,
                                 metadata_levels=list(all=NULL, filtered=NULL),
                                 metadata_numeric=list(all=NULL, filtered=NULL),
                                 spatial_coords=NULL,
                                 imagerow_max=list(),
                                 imagerow_min=list(),
                                 allmarkers=NULL,
                                 consmarkers=NULL,
                                 demarkers=NULL,
                                 cluster_colors=NULL,
                                 grouping_vars=NULL,
                                 selected_points=list(umap=NULL,
                                                      spatial=NULL))

    # var to stop app flow
    stop_flow <- reactiveVal(FALSE)

    # var to store config
    config <- reactiveVal(cfg)

    # list to hold project/analysis descriptions
    project_info <- reactiveValues(descriptions=list(), current=NULL)

    # reactive values to keep track of genes
    all_genes <- reactiveValues(choices=NULL)

    reset_data <- function(){
      # reset data
      app_object$rds <- NULL
      app_object$obj_type <- NULL
      app_object$metadata <- NULL
      app_object$metadata_levels <- list(all=NULL, filtered=NULL)
      app_object$metadata_numeric <- list(all=NULL, filtered=NULL)
      app_object$spatial_coords <- NULL
      app_object$imagerow_max <- list()
      app_object$imagerow_min <- list()
      app_object$allmarkers <- NULL
      app_object$consmarkers <- NULL
      app_object$demarkers <- NULL
      app_object$cluster_colors <- NULL
      app_object$grouping_vars <- NULL
      app_object$selected_points <- list(umap=NULL,
                                         spatial=NULL)

      # reset gene choices
      all_genes$choices <- NULL

      removeNotification('metadata_notify')

    }

    observeEvent(input$proj, {
      validate(
        need(input$proj != 'Choose one' & input$proj != '', 'Waiting for selection')
      )

      l <- assay_list$l

      assay_choices <- l[[input$proj]]
      if(length(assay_choices) == 1){
        updateSelectizeInput(session, 'analysis',
                             choices=assay_choices)
      } else {
        updateSelectizeInput(session, 'analysis',
                             choices=c('choose one', assay_choices))
      }

      stop_flow(TRUE)
    }) # observeEvent

    # this observer stops app flow if analysis input is changed
    # - this way the app updates *only* when 'Go' is clicked
    observeEvent(input$analysis, {

      stop_flow(TRUE)

    })

    output$analysis_desc <- renderDT({
      req(input$proj)

      proj_desc <- project_info$descriptions[[ input$proj ]]

      # check fields present in descriptions
      field_names <- unique(unlist(lapply(proj_desc, names)))

      if(is.null(field_names)){
        # here entries are a single string description
        df <- data.frame(
          'analysis_name'=names(proj_desc),
          'description'=unname(unlist(proj_desc))
        )
      } else {
        # here we handle multiple fields for each entry
        df.i <- lapply(proj_desc, function(x){
                 if(all(field_names %in% names(x))) x
                 else {
                   # check for missing fields and add NAs
                   ff <- setdiff(field_names, names(x))
                   ff <- c(x, setNames(rep(NA, length(ff)), ff))

                   # reorder
                   ff <- ff[ field_names ]
                 }
               })
        df <- as.data.frame(do.call('rbind', df.i))
        cnames <- colnames(df)
        df$analysis_name <- rownames(df)
        df <- df[, c('analysis_name', cnames)]
      }

      datatable(df,
                rownames=FALSE,
                selection='none',
                caption=tags$caption(style='font-weight: bold; font-size: 15px;',
                                     'Summary of available analyses'),
                options=list(dom='t'))

    })

    ############################## Load data #######################################

    observeEvent(input$load_do, {
      validate(
          need(input$analysis != '' & input$analysis != 'choose one',
               'Waiting for selection')
      )

      stop_flow(FALSE)

      # switch to summary tab
      updateTabsetPanel(session, inputId='mode',
                        selected='Summary')

      reset_data()

      # get file size for message
      fs <- file.size(input$analysis)
      if(is.na(fs)){
        fs_human <- ''
      } else if(fs < (1024**3)){
        fs_human <- paste0('(', round(fs/(1024**2), digits=1), ' MB)')
      } else {
        fs_human <- paste0('(', round(fs/(1024**3), digits=1), ' GB)')
      }

      showModal(
        modalDialog(
          span(paste('Loading data', fs_human, 'please wait')),
          footer=NULL
        )
      )

      assay_base <- dirname(input$analysis)

      # handle the case where an RDS doesn't exist on file
      # e.g. when a project analysis has been deployed
      if(!file.exists(input$analysis)){
        showModal(
          modalDialog(
            'Selected analysis does not exist on file any more. Reloading app',
            footer=NULL
          )
        )
        session$reload()
      }

      # only show assay menu for seurat objects
      if(regexpr('\\.(R|r)ds$', input$analysis) > 0){
        app_object$obj_type <- 'seurat'
        shinyjs::show('assay_menu')
      } else {
        app_object$obj_type <- 'anndata'
        shinyjs::hide('assay_menu')
      }

      if(app_object$obj_type == 'seurat'){
        obj<- readRDS(input$analysis)

        if(!inherits(obj, 'Seurat')){
          showModal(
            modalDialog(
              'This does not appear to be a Seurat object. Please choose different analysis',
              easyClose=TRUE
            )
          )

          validate(
            need(inherits(obj, 'Seurat'), 'Not a Seurat object')
          )
        }

        all_assays <- names(obj@assays)
        if('SCT' %in% all_assays) selected <- 'SCT'
        else selected <- all_assays[1]

        updateSelectInput(session, 'assay',
                          choices=all_assays,
                          selected=selected)

        # set DefaultAssay to non-integrated
        if(DefaultAssay(obj) == 'integrated'){
          other_assays <- setdiff(names(obj@assays), 'integrated')

          if(length(other_assays) > 0){
           if('SCT' %in% other_assays){
              DefaultAssay(obj) <- 'SCT'
            } else {
              # only switch to assay if not Assay5 object
              if(inherits(obj@assays[[ other_assays[1] ]], 'Assay5')){
                DefaultAssay(obj) <- other_assays[1]
              }
           }
          }
        }

        # get reductions & update menu
        dimred <- names(obj@reductions)
        idx <- grep('umap', dimred)
        if(length(idx) > 0) selected <- dimred[idx[1]]
        else selected <- dimred[1]
        updateSelectInput(session, 'dimred',
                          choices=dimred,
                          selected=selected)

        # add Idents() to obj metadata
        if(!is.null(Idents(obj))) obj@meta.data <- cbind(obj@meta.data, idents=Idents(obj))
        mdata <- obj@meta.data

        # save spatial object height min/max to flip plots
        # and spatial coordinates
        if(any(grepl('Spatial', names(obj@assays)))){
          if(inherits(obj@images[[1]], 'VisiumV2')){
            app_object$imagerow_max <- lapply(obj@images, function(x){
                                          x@boundaries$centroids@bbox['y', 'max']
                                        })
            app_object$imagerow_min <- lapply(obj@images, function(x){
                                          x@boundaries$centroids@bbox['y', 'min']
                                        })
            coords_list <- lapply(names(obj@images), function(x){
                             coords <- obj@images[[ x ]]@boundaries$centroids@coords
                             tmp <- data.table::as.data.table(coords)
                             colnames(tmp) <- c('imagerow', 'imagecol')
                             tmp$slice <- x
                             tmp$rn <- obj@images[[ x ]]@boundaries$centroids@cells
                             tmp
                           })
            app_object$spatial_coords <- data.table::rbindlist(coords_list)
          } else {
            app_object$imagerow_max <- lapply(obj@images, function(x){
                                          max(x@coordinates$imagerow)
                                        })
            app_object$imagerow_min <- lapply(obj@images, function(x){
                                          min(x@coordinates$imagerow)
                                        })
            coords_list <- lapply(names(obj@images), function(x){
                             coords <- obj@images[[ x ]]@coordinates
                             tmp <- data.table::as.data.table(coords, keep.rownames=T)
                             tmp$slice <- x
                             tmp
                           })
            app_object$spatial_coords <- data.table::rbindlist(coords_list)
          }
        } else if(any(grepl('Xenium', names(obj)))){
          app_object$imagerow_max <- lapply(obj@images, function(x){
                                        x$centroids@bbox['y', 'max']
                                      })
          app_object$imagerow_min <- lapply(obj@images, function(x){
                                        x$centroids@bbox['y', 'min']
                                      })
          coords_list <- lapply(names(obj@images), function(x){
                           coords <- obj@images[[ x ]]@boundaries$centroids@coords
                           tmp <- data.table::as.data.table(coords)
                           colnames(tmp) <- c('imagecol', 'imagerow')
                           tmp$slice <- x
                           tmp$rn <- obj@images[[ x ]]@boundaries$centroids@cells
                           tmp
                         })
          app_object$spatial_coords <- data.table::rbindlist(coords_list)
        }

      } else if(app_object$obj_type == 'anndata'){
        obj <- read_h5ad(input$analysis)

        if(!inherits(obj, 'AnnDataR6')){
          showModal(
            modalDialog(
              'This does not appear to be an anndata object. Please choose different analysis',
              easyClose=TRUE
            )
          )

          validate(
            need(inherits(obj, 'AnnDataR6'), 'Not an anndata object')
          )
        }

        mdata <- obj$obs

        updateSelectInput(session, 'assay',
                          choices=NULL,
                          selected=NULL)

        # get reductions & update menu
        dimred <- names(obj$obsm)
        names(dimred) <- sub('X_', '', dimred)

        idx <- grep('umap', dimred)
        if(length(idx) > 0) selected <- dimred[idx[1]]
        else selected <- dimred[1]
        updateSelectInput(session, 'dimred',
                          choices=dimred,
                          selected=selected)

        if('spatial' %in% names(obj$obsm)){
          app_object$imagerow_max <- list(slice=max(obj$obsm$spatial[,2]))
          app_object$imagerow_min <- list(slice=min(obj$obsm$spatial[,2]))

          tmp <- data.table::as.data.table(obj$obsm$spatial)
          colnames(tmp) <- c('imagecol', 'imagerow')
          tmp$slice <- 'slice'
          tmp$rn <- rownames(obj)
          app_object$spatial_coords <- tmp
        }

      }

      app_object$rds <- obj

      # handle NA values in metadata &
      # save factor column names & levels
      factor_cols <- NULL
      num_cols <- NULL
      all_same <- NULL
      dropped <- list(toosmall=NULL, toobig=NULL)
      for(mc in colnames(mdata)){
        tmp <- mdata[, mc]

        if(!is.numeric(tmp)){
          if(is.factor(tmp) | is.logical(tmp)){

            if(is.factor(tmp)){
              tmp <- droplevels(tmp)
              lvls <- levels(tmp)
            } else {
              lvls <- as.character(unique(tmp))
              lvls <- lvls[order(lvls)]
            }

            if(any(is.na(tmp))){
              na_idx <- is.na(tmp)

              tmp <- as.character(tmp)
              tmp[na_idx] <- 'NA'

              tmp <- factor(tmp,
                        levels=c(lvls[!is.na(lvls)], 'NA'))
            }

            mdata[, mc] <- tmp

            lvls <- levels(mdata[, mc])

            # only keep levels that are actually present
            lvls <- lvls[lvls %in% mdata[, mc]]
          } else if(is.character(tmp)){
            lvls <- unique(tmp[!is.na(tmp)])
            lvls <- lvls[order(lvls)]
            if(any(is.na(tmp))){
              lvls <- c(lvls, 'NA')
              na_idx <- is.na(tmp)
              tmp[na_idx] <- 'NA'

              mdata[, mc] <- factor(tmp, levels=lvls)
            }
          }

          if(length(lvls) < 2){
            dropped$toosmall <- c(dropped$toosmall, mc)
          } else {
            factor_cols <- c(factor_cols, mc)
            app_object$metadata_levels$all[[ mc ]] <- lvls
            app_object$metadata_levels$filtered[[ mc ]] <- lvls
          }
        } else {
          if(length(unique(tmp)) == 1){
            all_same <- c(all_same, mc)
          } else {
            num_cols <- c(num_cols, mc)
          }

          # get distribution of numeric metadata column
          tmp <- tmp[!is.na(tmp)]
          if(length(tmp) > 0){
            hh <- hist(tmp, breaks=20, plot=FALSE)
            hist_df <- data.frame(mids=hh$mids, counts=hh$counts)
            app_object$metadata_numeric$all[[ mc ]] <- hist_df
          }
        }
      }

      # if any factor columns are dropped show notification
      if(!all(unlist(lapply(dropped, is.null)))){
        msg <- ''
        if(!is.null(dropped$toosmall)){
          msg <- paste(paste(dropped$toosmall, collapse=', '), '(# levels < 2)')
        }

        if(!is.null(dropped$toobig)){
          tmp <- paste(paste(dropped$toobig, collapse=', '),
                       '(# levels > ',
                       config()$server$max_col_levels, ')')
          if(msg != '') msg <- paste0(msg, ', ', tmp)
          else msg <- tmp
        }
        showNotification(
          paste0('Warning: The following metadata columns will not be shown as grouping options: ', msg),
          type='warning',
          duration=20,
          id='metadata_notify'
        )
      }

      # if any numeric columns ignored, show notification
      if(length(all_same) > 0){
        showNotification(
          paste0('Warning: The following numeric metadata columns have a single value and will be skipped: ',
                 paste(all_same, collapse=', ')),
          type='warning',
          duration=20
        )
      }

      app_object$metadata <- mdata
      meta_cols <- colnames(mdata)

      f <- file.path(assay_base, 'allmarkers.tsv')
      if(file.exists(f)){
          df <- readr::read_tsv(f)

          # add dummy 'cluster' column if no cluster column present
          if(!'cluster' %in% colnames(df)) df[['cluster']] <- 'none'

          app_object$allmarkers <- df
      }

      f <- file.path(assay_base, 'consmarkers.tsv')
      if(file.exists(f)){
          df <- readr::read_tsv(f)

          # add dummy 'cluster' column if no cluster column present
          if(!'cluster' %in% colnames(df)) df[['cluster']] <- 'none'

          app_object$consmarkers <- df
      }

      f <- file.path(assay_base, 'demarkers.tsv')
      if(file.exists(f)){
          df <- readr::read_tsv(f)

          # add dummy 'cluster' column if no cluster column present
          if(!'cluster' %in% colnames(df)) df[['cluster']] <- 'none'

          app_object$demarkers <- df
      }

      # hide 'Cell Markers' tab if no marker tables uploaded
      if(all(is.null(app_object$allmarkers),
             is.null(app_object$consmarkers),
             is.null(app_object$demarkers))){

        hideTab(inputId='mode', target='Cell Markers')
      } else {
        showTab(inputId='mode', target='Cell Markers')

      }

      # get important grouping vars
      grp_col_opts <- config()$server$grouping_column$cols

      grouping_vars <- intersect(grp_col_opts, meta_cols)

      # handle multiple regex patterns
      grp_idx <- unique(
                    unlist(
                      lapply(config()$server$grouping_column$regex,
                        function(x) grep(x, meta_cols)
                      )
                    )
                  )
      grouping_vars <- unique(c(grouping_vars, meta_cols[grp_idx]))

      # move important grouping vars to beginning
      grouping_vars <- c(grouping_vars, setdiff(factor_cols, grouping_vars))

      # add number of levels to grouping var name
      grp_var_names <-
        unlist(lapply(grouping_vars,
            function(x){
              nlevels <- length(app_object$metadata_levels$all[[ x ]])
              paste0(x, ' (n = ', nlevels, ')')
            }
        ))
      names(grouping_vars) <- grp_var_names

      # here we check the number of levels of grouping variables
      # and if the first variable has > 25 levels, we find the next variable
      # with fewer levels and move it to the first position
      #
      # - This prevents the default selection of grouping variable to be too high
      #   on first load
      grp_sizes <- unname(unlist(lapply(grouping_vars, function(x) length(app_object$metadata_levels$all[[ x ]]))))
      if(grp_sizes[1] > 25){
        idx <- which(grp_sizes <= 25)
        if(length(idx) > 0){
          top_var <- names(grouping_vars)[idx[1]]
          grouping_vars <- grouping_vars[c(top_var, setdiff(names(grouping_vars), top_var))]
        }
      }

      if(length(grouping_vars) == 0){
        showNotification(
          'No suitable grouping columns found in metadata!'
        )
      } else {
        app_object$grouping_vars <- grouping_vars
      }
      updateSelectInput(session, 'grp_by',
                        choices=grouping_vars)

      # get label columns
      color_column <- grouping_vars
      names(color_column) <- color_column

      # get colors for levels of the label columns
      label_cols_all <- lapply(color_column, function(x){
                          if(x %in% names(app_object$metadata_levels$all)){
                            lvls <- app_object$metadata_levels$all[[ x ]]

                            if(any(lvls %in% 'NA')){
                              idx <- lvls %in% 'NA'
                              cols <- scales::hue_pal()(length(lvls) - sum(idx))
                              nona <- lvls[!idx]

                              # get gray colors for none values
                              gray <- paste0('gray', seq(20, 80, by=round(60/sum(idx))))
                              grayhex <- unlist(lapply(gray, function(x){
                                           tmp <- as.vector(col2rgb(x))
                                           rgb(tmp[1], tmp[2], tmp[3], maxColorValue=255)
                                         }))
                              cols <- c(cols, grayhex[1:sum(idx)])

                              if(sum(idx) > 1) names(cols) <- c(nona, paste0('NA', seq(1:sum(idx))))
                              else names(cols) <- c(nona, 'NA')
                            } else {
                              cols <- scales::hue_pal()(length(lvls))
                              names(cols) <- lvls
                            }
                            cols
                          }
                        })
      app_object$cluster_colors <- label_cols_all

      removeModal()
    }) # observeEvent

    # update gene scratchpad choices
    observeEvent(app_object$rds, {
      validate(
        need(!is.null(app_object$rds), '')
      )

      if(app_object$obj_type == 'seurat'){
        tmp_genes <- NULL
        for(assay in names(app_object$rds@assays)){
          if(inherits(app_object$rds@assays[[ assay ]], 'Assay5')){
            tmp_genes <- c(tmp_genes, rownames(app_object$rds@assays[[ assay ]]@features))
          } else {
            tmp_genes <- c(tmp_genes, rownames(app_object$rds@assays[[ assay ]]))
          }
        }
        all_genes$choices <- unique(tmp_genes)
      } else if(app_object$obj_type == 'anndata'){
        all_genes$choices <- rownames(app_object$rds$var)
      }
    }) # observeEvent

    reload_global <- reactive({
      input$plt_do

      # isolated flag to see if data is present
      # - this prevents this notif when app is first opened
      isolate({
        loaded <- !is.null(app_object$rds)
      })

      if(loaded){
        showNotification(
          'Refreshed global settings! Reload marker plots, metadata plots & cluster tree overlay to see effect',
          type='warning', duration=20
        )
      }
    })

    ##################### Filter controls ########################

    subset_args <- reactive({
      validate(
        need(!stop_flow(), '')
      )

      if(app_object$obj_type == 'seurat'){
        validate(
          need(input$assay != '', '')
        )
      }

      list(
        assay=input$assay
      )
    })

    metadata_args <- reactive({
      list(
        factor_levels=app_object$metadata_levels$all,
        numeric_dist=app_object$metadata_numeric$all
      )
    })

    apply_filters <- subsetServer('subset_tab',
                                  app_object,
                                  subset_args,
                                  metadata_args,
                                  reactive({ all_genes$choices }))

    observeEvent(apply_filters(), {
      bc <- apply_filters()

      # subset metadata levels
      idx <- which(rownames(app_object$metadata) %in% bc)
      mdata.dt <- data.table::as.data.table(app_object$metadata, keep.rownames=T)
      mdata.dt <- mdata.dt[idx,]
      for(mc in names(app_object$metadata_levels$all)){
        lvls <- t(unique(mdata.dt[[ mc ]]))
        app_object$metadata_levels$filtered[[ mc ]] <- intersect(app_object$metadata_levels$all[[ mc ]], lvls)
      }

      # update numeric metadata
      for(mc in names(app_object$metadata_numeric$all)){
        hh <- hist(mdata.dt[[ mc ]], breaks=20, plot=FALSE)
        hist_df <- data.frame(mids=hh$mids, counts=hh$counts)
        app_object$metadata_numeric$filtered[[ mc ]] <- hist_df
      }
    })

    #################### Summary table ####################

    summary_args <- reactive({
      list(
        project=input$proj,
        analysis=input$analysis
      )
    })

    summaryServer('summ_tab', app_object, summary_args)

    ############################## Clustering Summary #######################################

    clust_summary_args <- reactive({
      list(
        dimred=input$dimred,
        grp_by=input$grp_by
      )
    })

    clustSummaryServer('clust_summ_tab', app_object,
                       apply_filters,
                       clust_summary_args, reload_global, config)

    ####################### Cluster Tree #########################################

    clustree_args <- reactive({
      list(
        dimred=input$dimred,
        grp_by=input$grp_by
      )
    })

    clustreeServer('clustree', app_object,
                   apply_filters,
                   clustree_args, reload_global, config)

    ############################## Markers #######################################

    marker_tbl_data <- markerTableServer('marker_tbl_tab',
                                         app_object,
                                         gene_scratchpad,
                                         reactive({ input$reset.genes }),
                                         reload_global,
                                         config)

    observeEvent(marker_tbl_data(), {
      ll <- marker_tbl_data()

      g <- ll$g

      updateSelectizeInput(session, 'gene.to.plot',
                           choices=all_genes$choices,
                           selected=g,
                           server=TRUE)

    })

    ############################## Gene scratchpad #######################################

    gene_scratchpad <- reactive({
      g <- input$gene.to.plot
      if(!all(g %in% all_genes$choices)){
        g.diff <- g[!g %in% all_genes$choices]

        if(!is.null(all_genes$choices)){
          showNotification(
            paste('Warning: Skipping some genes not found in data set -',
                   paste(g.diff, collapse=', ')),
            type='warning', duration=5
          )
        }
        g <- setdiff(g, g.diff)

        updateSelectizeInput(session, 'gene.to.plot',
                             choices=all_genes$choices,
                             selected=g,
                             server=TRUE)
      }
      g
    })

    # observer to reset gene scratchpad
    observeEvent(input$reset.genes, {
      validate(
        need(!is.null(all_genes$choices), 'Loading ...')
      )

      updateSelectizeInput(session, 'gene.to.plot',
                           choices=all_genes$choices,
                           selected='',
                           server=TRUE)

    }) # observeEvent

    # observer to update gene scratchpad choices
    observeEvent(all_genes$choices, {
      # update gene selector with genes in object
      updateSelectizeInput(session, 'gene.to.plot',
                           choices=all_genes$choices,
                           selected='',
                           server=TRUE)
    }) # observeEvent

    ##################### UMAP plot ########################

    dimred_args <- reactive({
      list(
        dimred=input$dimred,
        grp_by=input$grp_by
      )
    })

    dimred_selected <- dimredServer('dimred_tab', app_object,
                                    apply_filters,
                                    dimred_args,
                                    reload_global,
                                    config)

    observeEvent(dimred_selected(), {
      ll <- dimred_selected()

      app_object$selected_points$umap <- ll$umap

      if('spatial' %in% names(ll)){
        app_object$selected_points$spatial <- ll$spatial
      }

    })

    ##################### Marker Plots ########################

    plot_args <- reactive({
      list(
        assay=input$assay,
        dimred=input$dimred,
        grp_by=input$grp_by
      )
    })

    markerPlotServer('mrkrplt_tab',
                     app_object,
                     apply_filters,
                     gene_scratchpad,
                     plot_args,
                     reactive({ all_genes$choices }),
                     reload_global,
                     config)

    ########################## Help #############################

    helpButtonServer('global_help', size='l')
    helpButtonServer('gene_scratchpad_help', size='l')

  } # server

  shinyApp(ui=ui, server=server, ...)

}
