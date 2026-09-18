# Get pseudobulk expression from anndata object

This is a stripped-down lightweight version of Seurat's internal
function adapted for an anndata object that has genes in columns not
rows.

## Usage

``` r
PseudoBulkExpression2(
  object,
  pb.method = "average",
  features = NULL,
  group.by = "ident"
)
```

## Arguments

- object:

  anndata object

- pb.method:

  pseudo-bulk method. Can be 'average' (default) or 'aggregate'.

- features:

  features to analyze. Default is all features in the object

- group.by:

  metadata column to group cells by

## Value

matrix with pseudo-bulk counts
