# Create cascade python environment

This function installs 'plotly' and 'kaleido' python packages in an
environment to allow PDF downloads from plotly plots, and 'anndata' to
allow h5ad files.

## Usage

``` r
install_cascade(envname, ...)
```

## Arguments

- envname:

  name of the python environment

- ...:

  parameters passed to reticulate::py_install

## Value

Return value from reticulate::py_install(); called for the side effect
of installing Python packages.

## Examples

``` r
if (FALSE) { # interactive()

install_cascade()
}
```
