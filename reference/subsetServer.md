# Filter settings module server

Filter settings module server

## Usage

``` r
subsetServer(id, obj, args, metadata_args, gene_choices, selected_points)
```

## Arguments

- id:

  Input id

- obj:

  Cascade app object

- args:

  reactive list with 'assay' with name of selected assay

- metadata_args:

  reactive list with 'factor_levels' that has levels of categorical
  metadata & 'numeric_dist' that has distributions of numeric metadata

- gene_choices:

  reactive list with all genes present in object

- selected_points:

  reactive list with cell selections

## Value

reactive expression containing filtered cell barcodes
