# Spatial feature plot module

Spatial feature plot module

## Usage

``` r
spatialFeaturePlotUI(id, panel)

spatialFeaturePlotServer(
  id,
  app_object,
  filtered,
  genes_to_plot,
  args,
  gene_choices,
  slice,
  all_selected,
  show_selection,
  reset_selection,
  reload_global,
  refresh,
  config
)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'sidebar' or 'main'

- app_object:

  Cascade app object

- filtered:

  barcodes to filter object

- genes_to_plot:

  reactive list with genes in scratchpad

- args:

  reactive list with elements: 'assay' for selected assay, 'dimred' for
  which dimension reduction to use and 'grp_by' for grouping variable

- gene_choices:

  reactive list with all genes present in object

- slice:

  reactive with slices to be used for plotting

- all_selected:

  reactive containing list of selected points

- show_selection:

  reactive to show selection

- reset_selection:

  reactive to reset selection

- reload_global:

  reactive to trigger reload

- refresh:

  reactive to trigger plot refresh from sidebar button

- config:

  reactive list with config settings

## Value

UI returns sidebar/main panel UI elements for spatial feature plot
Server returns reactive expression containing selected points from the
spatial feature plot

## Examples

``` r
if (FALSE) { # interactive()
# example obj
obj <- make_example_seurat_object()

# prep metadata
metadata <- obj[[]]
metadata_levels <- lapply(
  metadata[c("cluster", "condition", "orig.ident", "seurat_clusters")],
  levels
)

spatial_coords <- data.frame(
  rn = colnames(obj),
  slice = as.character(metadata$orig.ident),
  imagecol = rep(c(1, 2, 1, 2), length.out = ncol(obj)),
  imagerow = rep(c(1, 1, 2, 2), length.out = ncol(obj)),
  stringsAsFactors = FALSE
)
imagerow_max <- as.list(
  tapply(spatial_coords$imagerow, spatial_coords$slice, max)
)
imagerow_min <- as.list(
  tapply(spatial_coords$imagerow, spatial_coords$slice, min)
)

# get grouping vars and colors
grouping_vars <- names(metadata_levels)
names(grouping_vars) <- paste0(
  grouping_vars,
  " (n = ",
  lengths(metadata_levels),
  ")"
)
cluster_colors <- lapply(metadata_levels, function(lvls) {
  stats::setNames(rep_len(c("#4477aa", "#cc6677"), length(lvls)), lvls)
})

app_object <- list(
  rds = obj,
  obj_type = "seurat",
  metadata = metadata,
  metadata_levels = metadata_levels,
  cluster_colors = cluster_colors,
  grouping_vars = grouping_vars,
  spatial_coords = spatial_coords,
  imagerow_max = imagerow_max,
  imagerow_min = imagerow_min
)

global_args <- list(
  assay = "RNA",
  slot = "data",
  grp_by = "cluster",
  dimred = "umap"
)

config <- get_config()

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::sidebarLayout(
    shiny::sidebarPanel(spatialFeaturePlotUI("spatial_feature", "sidebar")),
    shiny::mainPanel(
      shiny::tabsetPanel(spatialFeaturePlotUI("spatial_feature", "main")),
      shiny::verbatimTextOutput("selected")
    )
  )
)

server <- function(input, output, session) {
  selected <- spatialFeaturePlotServer(
    "spatial_feature",
    app_object = shiny::reactive({ app_object }),
    filtered = shiny::reactive({ colnames(obj) }),
    genes_to_plot = shiny::reactive({ "GeneA" }),
    args = shiny::reactive({ global_args }),
    gene_choices = shiny::reactive({ rownames(obj) }),
    slice = shiny::reactive({ unique(spatial_coords$slice)[1] }),
    all_selected = shiny::reactive({ list() }),
    show_selection = shiny::reactive({ NULL }),
    reset_selection = shiny::reactive({ NULL }),
    reload_global = shiny::reactiveVal(0),
    refresh = shiny::reactiveVal(0),
    config = shiny::reactive({ config })
  )

  output$selected <- shiny::renderPrint({
    selected()
  })
}

shiny::shinyApp(ui, server)
}
```
