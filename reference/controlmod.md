# Filter control module

Filter control module

## Usage

``` r
controlUI(id, label)

controlServer(id, full_obj, column, global, default = "all")
```

## Arguments

- id:

  string, input id

- label:

  string, heading for controls

- full_obj:

  reactive, object used to generate controls. can be data frame or list

- column:

  string or reactive list, name of column or list element present in
  full_obj used to generate controls

- global:

  reactive list used to set input externally

- default:

  numeric vector or string, which elements to initialize with. If 'all'
  (default) all elements are selected, otherwise, can specify indices as
  numeric integer(s).

## Value

UI returns filter controls UI Server returns reactive expression
containing the selected filter values

## Examples

``` r
if (FALSE) { # interactive()
metadata <- data.frame(
  cluster = factor(rep(c("A", "B"), each = 4), levels = c("A", "B")),
  condition = factor(rep(c("ctrl", "stim"), times = 4),
                     levels = c("ctrl", "stim"))
)

ui <- shiny::fluidPage(
  controlUI("cluster_filter", "Cluster"),
  shiny::verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  selected <- controlServer(
    "cluster_filter",
    full_obj = shiny::reactive({ metadata }),
    column = "cluster",
    global = shiny::reactive({ NULL })
  )

  output$selected <- shiny::renderPrint({
    selected()
  })
}

shiny::shinyApp(ui, server)
}
```
