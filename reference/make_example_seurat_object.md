# Create a tiny example Seurat object

This creates a small normalized Seurat object with an RNA assay,
metadata, identities, and a two-dimensional UMAP reduction. It is
intended for documentation examples and lightweight module demos.

## Usage

``` r
make_example_seurat_object(
  genes = paste0("Gene", LETTERS[seq_len(4)]),
  cells = paste0("cell", seq_len(8)),
  assay = "RNA"
)
```

## Arguments

- genes:

  character vector of gene names to include.

- cells:

  character vector of cell names to include.

- assay:

  assay name to use when creating the object.

## Value

A Seurat object.

## Examples

``` r
if (FALSE) { # interactive()
obj <- make_example_seurat_object()
obj
}
```
