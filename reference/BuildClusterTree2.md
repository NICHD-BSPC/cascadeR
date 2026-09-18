# Build cluster tree for anndata objects

This is a stripped down version of Seurat::BuildClusterTree for anndata
objects. It takes the same arguments as the Seurat function.

## Usage

``` r
BuildClusterTree2(
  object,
  features = NULL,
  dims = NULL,
  reduction = "X_pca",
  clust_column = "leiden"
)
```

## Arguments

- object:

  anndata object

- features:

  genes to use for building cluster tree

- dims:

  dimensions to use. This is only used if reduction is pca.

- reduction:

  reduction to use for cluster tree

- clust_column:

  column to use for grouping cells

## Value

phylogenetic tree
