# Cluster tree module

Cluster tree module

## Usage

``` r
clustreeUI(id, panel)

clustreeServer(id, obj, filtered, args, reload_global, config)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'sidebar' or 'main'

- obj:

  Cascade app object

- filtered:

  cell barcodes for filtering object

- args:

  reactive list with global args, 'grp_by' for grouping variable and
  'dimred' for which dimension reduction to use

- reload_global:

  reactive to reload module

- config:

  reactive list with config settings

## Value

UI returns sidebar/main panel UI elements for the cluster tree Server
called for the side effect of rendering cluster tree plots.

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
  metadata_levels = list(
    all = metadata_levels,
    filtered = metadata_levels
  ),
  cluster_colors = cluster_colors,
  grouping_vars = grouping_vars,
  spatial_coords = NULL,
  imagerow_max = NULL,
  imagerow_min = NULL
)

global_args <- list(
  grp_by = "cluster",
  dimred = "umap"
)

config <- get_config()

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::sidebarLayout(
    shiny::sidebarPanel(clustreeUI("clustree", "sidebar")),
    shiny::mainPanel(clustreeUI("clustree", "main"))
  )
)

server <- function(input, output, session) {
  clustreeServer(
    "clustree",
    obj = app_object,
    filtered = shiny::reactive({ colnames(obj) }),
    args = shiny::reactive({ global_args }),
    reload_global = shiny::reactiveVal(0),
    config = shiny::reactive({ config })
  )
}

shiny::shinyApp(ui, server)
}
```
