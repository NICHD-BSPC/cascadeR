# Generalized marker table module

Generalized marker table module

## Usage

``` r
markerTableGeneralUI(id, panel, type, label = NULL)

markerTableGeneralServer(
  id,
  obj,
  type,
  genes_to_plot,
  reset_genes,
  global_args,
  args,
  reload_global,
  config
)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'sidebar', 'selection' or 'main'

- type:

  string, used to define type of table

- label:

  string, label for table

- obj:

  reactive list with marker tables

- genes_to_plot:

  reactive list of genes to plot

- reset_genes:

  reactive to trigger gene selection reset

- global_args:

  reactive list with global settings

- args:

  reactive list with 'max_padj', 'max_lfc'

- reload_global:

  reactive to trigger global args reload

- config:

  reactive list with config settings

## Value

UI returns sidebar/selection/main panel UI elements for a marker table
Server returns reactive expression containing marker filters and
selected genes

## Examples

``` r
if (FALSE) { # interactive()
marker_tbl <- data.frame(
  gene = c("GeneA", "GeneB", "GeneC", "GeneD"),
  cluster = c("A", "A", "B", "B"),
  avg_log2FC = c(1.2, 0.7, 1.1, 0.8),
  pct.1 = c(0.9, 0.8, 0.85, 0.75),
  pct.2 = c(0.2, 0.3, 0.25, 0.35),
  p_val_adj = c(0.001, 0.02, 0.005, 0.03)
)

config <- get_config()

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      markerTableGeneralUI(
        "allmarkers",
        panel = "sidebar",
        type = "allmarkers",
        label = "Cluster Markers"
      ),
      markerTableGeneralUI(
        "allmarkers",
        panel = "selection",
        type = "allmarkers",
        label = "Cluster Markers"
      )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        markerTableGeneralUI(
          "allmarkers",
          panel = "main",
          type = "allmarkers",
          label = "Cluster Markers"
        )
      ),
      shiny::verbatimTextOutput("selected")
    )
  )
)

server <- function(input, output, session) {
  selected <- markerTableGeneralServer(
    "allmarkers",
    obj = shiny::reactive({ list(markers = marker_tbl) }),
    type = "allmarkers",
    genes_to_plot = shiny::reactive({ character() }),
    reset_genes = shiny::reactive({ NULL }),
    global_args = shiny::reactive({ list() }),
    args = shiny::reactive({ list(max_padj = 0.1, min_lfc = 0) }),
    reload_global = shiny::reactiveVal(0),
    config = shiny::reactive({ config })
  )

  output$selected <- shiny::renderPrint({
    selected()
  })
}

shiny::shinyApp(ui, server)
}
```
