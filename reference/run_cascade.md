# Cascade

Interactive shiny dashboard for exploring single-cell or spatial RNA-Seq
data.

## Usage

``` r
run_cascade(credentials = NULL, passphrase = NULL, enable_admin = TRUE, ...)
```

## Arguments

- credentials:

  path to encrypted sqlite db with user credentials.

- passphrase:

  passphrase for credentials db.

- enable_admin:

  if TRUE, admin view is shown. Note, this is only available if
  credentials have sqlite backend.

- ...:

  parameters passed to shinyApp() call

## Value

Shiny app object

## Examples

``` r
if (FALSE) { # interactive()
shiny::runApp(
  run_cascade()
)
}
```
