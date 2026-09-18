#' Download selection module
#'
#' UI & server for selection module
#'
#' @param id Input id
#' @param selections reactive list w selections
#' @param obj reactive app object
#'
#' @return Shiny UI & server for the download selection button module
#'
#' @rdname selmod
#' @name selmod
#'
NULL

#' @rdname selmod
#' @export
selectionUI <- function(id){
  ns <- NS(id)

  actionButton(ns('dload_btn'),
                 label='Download',
                 icon=icon('download'))
}

#' @rdname selmod
#' @export
selectionServer <- function(id, selections, obj){
  moduleServer(
    id,

    function(input, output, session){

      ns <- session$ns

      metadata <- reactive({ obj$metadata })

      observeEvent(input$dload_btn, {
        showModal(
          modalDialog(
            title=h3('Download selections'),
            selectizeInput(ns('select'),
                           label=h4('Choose which selections to download'),
                           choices=names(selections()),
                           selected=names(selections()),
                           multiple=TRUE, width='100%'),
            actionButton(ns('sel_none'),
                         label='none',
                         icon=shiny::icon('minus')),
            actionButton(ns('sel_all'),
                         label='all',
                         icon=shiny::icon('check')),
            checkboxInput(ns('add_id'),
                          label='add selection id?',
                          value=FALSE),
            footer=tagList(
              downloadButton(ns('download'),
                             label='Download', class='btn-primary'),
              modalButton('Cancel')
            ),
            easyClose=TRUE
          ) # modalDialog
        ) # showModal
      }) # observerEvent


      output$download <- downloadHandler(
        filename = function(){
          paste0('clicked-points.tsv')
        },
        content = function(file){
          bc <- unique(unlist(selections()[ input$select ]))

          # only output unique barcodes
          mdata <- data.table::as.data.table(metadata(), keep.rownames=TRUE)
          idx <- mdata$rn %in% bc

          mdata_sel <- as.data.frame(mdata[idx,])
          rn_idx <- which(colnames(mdata_sel) == 'rn')
          colnames(mdata_sel)[rn_idx] <- 'barcodes'

          # add selection_id
          if(input$add_id){
            # create new column in df
            mdata_sel$selection_id <- ''

            # add sel_ids to corresponding barcodes in a loop
            for(sel in input$select){
              idx <- which(mdata_sel$barcodes %in% selections()[[ sel ]])
              mdata_sel$selection_id[ idx ] <- paste(mdata_sel$selection_id[ idx ],
                                                     sel, sep=',')
            }

            # trim off leading commas
            mdata_sel$selection_id <- sub('^,', '', mdata_sel$selection_id)
          }

          write.table(mdata_sel, file=file, sep='\t', quote=FALSE,
                      row.names=FALSE)

          removeModal()
        }
      )

      # filter selection controls
      observeEvent(input$sel_none, {
        updateSelectizeInput(session, 'select',
                             selected='')
      })

      observeEvent(input$sel_all, {
        updateSelectizeInput(session, 'select',
                             selected=names(selections()))
      })

    }   # function
  ) # moduleServer
}
