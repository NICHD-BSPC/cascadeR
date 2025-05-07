#' Filter control module UI
#'
#' @param id string, input id
#' @param label string, heading for controls
#'
#' @export
#'
controlUI <- function(id, label){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12, strong(label))
    ),
    fluidRow(
      column(12,
        selectizeInput(ns('filter'), label=NULL,
                       choices=NULL,
                       selected=NULL,
                       multiple=TRUE,
                       width='100%')
      ) # column
    ), # fluidRow

    fluidRow(
      align='center',
      style='margin-bottom: 20px;',
      column(12,
        splitLayout(cellWidths=c('50%', '50%'),
            actionButton(ns('sel_none'),
                         label='none',
                         icon=shiny::icon('minus'),
                         class='select-buttons'),
            actionButton(ns('sel_all'),
                         label='all',
                         icon=shiny::icon('check'),
                         class='select-buttons')
        ) # splitLayout
      ) # column
    ) # fluidRow
  ) # tagList
}

#' Filter control module server
#'
#' @param id string, input id
#' @param full_obj reactive, object used to generate controls. can be data frame or list
#' @param column string or reactive list, name of column or list element present in full_obj
#'        used to generate controls
#' @param global reactive list used to set input externally
#' @param default numeric vector or string, which elements to initialize with. If 'all' (default)
#'        all elements are selected, otherwise, can specify indices as numeric integer(s).
#'
#' @export
#'
controlServer <- function(id, full_obj, column, global, default='all'){
  moduleServer(
    id,

    function(input, output, session){
      ns <- NS(id)

      col <- reactive({
        if(is.reactive(column)) column()
        else column
      })

      observeEvent(c(full_obj(), col()), {
        df <- full_obj()

        if(is.data.frame(df)){
          if(col() %in% colnames(df)){
            dat <- unique(df[[ col() ]])
          } else {
            dat <- ''
          }
        } else if(is.list(df)){
          if(col() %in% names(df)) dat <- df[[ col() ]]
          else dat <- ''
        } else {
          dat <- ''
        }

        if(is.numeric(default)){
          default <- intersect(default, 1:length(dat))
          if(length(default) > 0){
            selected <- dat[default]
          }
        } else {
          if(is.character(default) & default != 'all'){
            message('"default" can only be "all" or a numeric vector')
          }
          selected <- dat
        }
        updateSelectizeInput(session, 'filter',
                             choices=dat,
                             selected=selected)

      })

      # filter selection controls
      observeEvent(input$sel_none, {
        updateSelectizeInput(session, 'filter',
                             selected='')
      })

      observeEvent(input$sel_all, {
        all_levels <- unique(full_obj()[[ col() ]])
        updateSelectizeInput(session, 'filter',
                             selected=all_levels)
      })

      observeEvent(global(), {
        updateSelectizeInput(session, 'filter',
                             selected=global())
      })

      return(
        reactive({
          input$filter
        })
      )
    } # function
  ) # moduleServer
} # server

