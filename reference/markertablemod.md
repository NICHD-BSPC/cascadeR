# Marker tables module

This is a master wrapper for generalized marker tables.

## Usage

``` r
markerTableUI(id, panel)

markerTableServer(id, obj, genes_to_plot, reset_genes, reload_global, config)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'global', 'sidebar', 'selection' or 'main'

- obj:

  Cascade app object

- genes_to_plot:

  reactive with list of genes to be plotted/selected

- reset_genes:

  reactive to trigger gene selection reset

- reload_global:

  reactive to trigger reload

- config:

  reactive list with config settings

## Value

UI returns global/sidebar/selection/main panel UI elements for marker
tables Server returns reactive expression containing selected marker
genes

## Examples

``` r
if (FALSE) { # interactive()
# example obj
obj <- make_example_seurat_object()

marker_tbl <- data.frame(
  gene = c("GeneA", "GeneB", "GeneC", "GeneD"),
  cluster = c("A", "A", "B", "B"),
  avg_log2FC = c(1.2, 0.7, 1.1, 0.8),
  pct.1 = c(0.9, 0.8, 0.85, 0.75),
  pct.2 = c(0.2, 0.3, 0.25, 0.35),
  p_val_adj = c(0.001, 0.02, 0.005, 0.03)
)

app_object <- list(
  rds = obj,
  allmarkers = marker_tbl,
  consmarkers = NULL,
  demarkers = NULL
)

config <- get_config()

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      markerTableUI("markers", "global"),
      markerTableUI("markers", "sidebar"),
      markerTableUI("markers", "selection")
    ),
    shiny::mainPanel(
      markerTableUI("markers", "main"),
      shiny::verbatimTextOutput("selected")
    )
  )
)

server <- function(input, output, session) {
  selected <- markerTableServer(
    "markers",
    obj = app_object,
    genes_to_plot = shiny::reactive({ character() }),
    reset_genes = shiny::reactive({ NULL }),
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
