# Add new cascade analysis

Add new cascade analysis

## Usage

``` r
add_cascade_analysis(
  obj_path,
  data_dir,
  project,
  analysis,
  cluster_markers = NULL,
  de_markers = NULL,
  conserved_markers = NULL,
  overwrite = FALSE,
  execute = FALSE
)
```

## Arguments

- obj_path:

  path to Seurat/anndata object

- data_dir:

  output data directory. For convenience use the same data directory for
  all cascade projects.

- project:

  project name. Creates a subfolder with this name inside data_dir if it
  doesn't exist.

- analysis:

  analysis label. Creates a subfolder with this name inside
  data_dir/project

- cluster_markers:

  (optional) path(s) to tab-delimited file(s) containing cluster
  markers. Output from Seurat's FindAllMarkers and scanpy's
  rank_genes_groups are supported.

- de_markers:

  (optional) path(s) to tab-delimited file(s) containing differentially
  expressed markers from Seurat's FindMarkers or scanpy's
  rank_genes_groups.

- conserved_markers:

  (optional) path(s) to tab-delimited file(s) containing conserved
  markers from Seurat's FindConservedMarkers function.

- overwrite:

  boolean, if TRUE, existing analysis folder will be overwritten
  (default=FALSE)

- execute:

  boolean, set this to TRUE to actually run the commands (default=FALSE)

## Value

Invisibly returns NULL; called for the side effect of printing or
executing setup commands.

## Examples

``` r
if (FALSE) { # interactive()
local({
  data_dir <- tempfile("cascade-data-")
  dir.create(data_dir)

  obj_path <- tempfile("object-", fileext = ".rds")
  file.create(obj_path)

  on.exit(unlink(c(data_dir, obj_path), recursive = TRUE))

  add_cascade_analysis(
    obj_path = obj_path,
    data_dir = data_dir,
    project = "project1",
    analysis = "analysis1",
    execute = FALSE
  )
})
}
```
