# Get highly variable genes

This function calculates most variable genes from either Seurat or
anndata objects.

## Usage

``` r
get_var_genes(obj, obj_type, assay_name)
```

## Arguments

- obj:

  object to get variable genes from

- obj_type:

  type of object being used. can be 'seurat' or 'anndata'

- assay_name:

  name of assay for variable genes. Ignored if obj_type == 'anndata'.

## Value

vector of gene names
