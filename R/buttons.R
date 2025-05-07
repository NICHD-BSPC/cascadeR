#' Help button module ui
#'
#' UI for help button module
#'
#' @param id Input id
#'
helpButtonUI <- function(id){
  ns <- NS(id)

  actionButton(ns('help_btn'),
               label=NULL,
               icon=icon('question'),
               class='help-buttons')
}

#' Help button module server
#'
#' Server for help button module
#'
#' @param id Input id
#' @param ... other params passed to helpModal()
#'
helpButtonServer <- function(id, ...){
  moduleServer(
    id,

    function(input, output, session){

      ns <- NS(id)

      observeEvent(input$help_btn, {
        # convert id to full system path
        mdfile <- system.file('extdata', 'help',
                              paste0(sub('_help', '', id), '.md'),
                              package=packageName())

        showModal(
          helpModal(mdfile=mdfile, ...)
        )
      })

    }   # function
  ) # moduleServer
}


#' Help modal
#'
#' This generates a modal dialog that includes text
#' from a markdown file.
#'
#' @param mdfile path to markdown file
#' @param title Title of modal dialog
#' @param ... other params passed to modalDialog()
#'
helpModal <- function(mdfile, title=NULL, ...){
  modalDialog(
      title=title,
      includeMarkdown(mdfile),
      footer=tagList(
          modalButton('OK')
      ),
      easyClose=TRUE,
      ...
  )
}

#' Download button module ui
#'
#' UI for download button module
#'
#' @param id Input id
#'
downloadPlotUI <- function(id){
  ns <- NS(id)

  actionButton(ns('dload_btn'),
               label='Download',
               icon=icon('download'),
               class='dload-buttons')
}

#' Download button module server
#'
#' Server for download button module
#'
#' @param id Input id
#' @param outplot reactive plot handle
#' @param plot_type reactive/static value used for output filename
#' @param direction direction of phylogenetic tree
#'
downloadPlotServer <- function(id, outplot, plot_type, direction=NULL){
  moduleServer(
    id,

    function(input, output, session){

      ns <- session$ns

      observeEvent(input$dload_btn, {
        dims <- list(width=9, height=6)

        showModal(
          modalDialog(
            title='Save plot to PDF?',
            tagList(
              fluidRow(
                column(12,
                  span('Edit plot dimensions below ...',
                    style='font-style: italic; font-size: 15px;')
                ) # column
              ), # fluidRow
              fluidRow(style='margin-top: 15px;',
                column(6, 'height (in inches)'),
                column(6,
                  numericInput(ns('plot_ht'),
                               label=NULL,
                               value=dims$height)
                ) # column
              ), # fluidRow
              fluidRow(
                column(6, 'width (in inches)'),
                column(6,
                  numericInput(ns('plot_wd'),
                               label=NULL,
                               value=dims$width)
                ) # column
              ), # fluidRow

              uiOutput(ns('ppi_input'))
            ), # tagList
            footer=tagList(
                downloadButton(ns('download'), label='OK'),
                modalButton('Cancel')
            ),
            easyClose=TRUE
          )
        ) # showModal
      })

      # reactive to return plot filename & handles
      # both reactive and static values
      plot_filename <- reactive({
          if(is.reactive(plot_type)) plot_type()
          else plot_type
      })

      tree_direction <- reactive({
        if(is.reactive(direction)) direction()
        else direction
      })

      output$ppi_input <- renderUI({
        if(inherits(outplot(), 'plotly')){
          fluidRow(
            column(6, 'resolution (in ppi)'),
            column(6,
              selectInput(ns('plot_ppi'),
                          label=NULL,
                          choices=c('low (96 ppi)'=96,
                                    'high (300 ppi)'=300))
            ) # column
          ) # fluidRow
        }
      })

      output$download <- downloadHandler(
        filename = function(){
          paste0(plot_filename(), '.pdf')
        },
        content = function(file){
          # dendrogram & upset plot use base R graphics
          if(plot_filename() == 'clustree_single'){
            pdf(file, width=input$plot_wd, height=input$plot_ht)

            if(is.null(direction)){
              ape::plot.phylo(outplot(), cex=1.5)
            } else {
              ape::plot.phylo(outplot(), direction=tree_direction(), cex=1.5)
            }

            dev.off()

          } else if(plot_filename() %in% c('qc_filtered_upset')){
            pdf(file, width=input$plot_wd, height=input$plot_ht)
            print(outplot())
            dev.off()
          } else {
            # NOTE: this needs 'kaleido' module in python to be
            #       available for 'reticulate'
            if(inherits(outplot(), 'plotly')){
              ppi <- as.numeric(input$plot_ppi)

              # NOTE: turn off mathjax to prevent "Loading MathJax ..." box in saved plot
              # - solution from https://stackoverflow.com/questions/79464233/loading-mathjax-extensions-mathmenu-js-box-visible-in-pdf-when-running-plotl/
              k <- plotly::kaleido()
              k$scope$mathjax <- FALSE

              k$transform(outplot(),
                          file=file,
                          width=input$plot_wd*ppi,
                          height=input$plot_ht*ppi)
            } else if(inherits(outplot(), 'ggplot')){
              ggsave(file, plot = outplot(),
                   device='pdf',
                   width=input$plot_wd, height=input$plot_ht)
            } else {
              showNotification(
                'Warning: Download only supported for plotly/ggplot plots',
                type='warning'
              )
            }
          }
          removeModal()
        }
      )

    }   # function
  ) # moduleServer
}

#' Download file module ui
#'
#' UI for download file module
#'
#' @param id Input id
#'
downloadFileUI <- function(id){
  ns <- NS(id)

  style <- 'padding-top: 2px;
    padding-bottom: 2px;
    padding-left: 5px;
    padding-right: 5px;
    margin-bottom: 10px;
    font-style: bold;'

  actionButton(ns('dload_btn'),
                 label='Download',
                 icon=icon('download'),
                 style=style)
}

#' Download file module server
#'
#' Server for download file module
#'
#' @param id Input id
#' @param data reactive w data frame
#' @param file_type reactive/static value used for output filename
#'
downloadFileServer <- function(id, data, file_type){
  moduleServer(
    id,

    function(input, output, session){

      ns <- session$ns

      # reactive to return plot filename & handles
      # both reactive and static values
      filename <- reactive({
          if(is.reactive(file_type)) file_type()
          else file_type
      })

      observeEvent(input$dload_btn, {
        showModal(
          modalDialog(
            title=NULL,
            span('Download table as TSV?'),
            footer=tagList(
              downloadButton(ns('download'), label='OK'),
              modalButton('Cancel')
            ),
            easyClose=TRUE
          ) # modalDialog
        ) # showModal
      }) # observerEvent

      output$download <- downloadHandler(
        filename = function(){
          paste0(filename(), '.tsv')
        },
        content = function(file){
          df <- data()
          readr::write_tsv(as.data.frame(df), file)

          removeModal()
        }
      )

    }   # function
  ) # moduleServer
}

