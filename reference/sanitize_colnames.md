# Sanitize strings

This function removes special characters from strings. Meant to be used
to sanitize column names of a data frame before plotting.

## Usage

``` r
sanitize_colnames(cnames, bad_char = "\\W+", repl = "_")
```

## Arguments

- cnames:

  column names to sanitize

- bad_char:

  regex with characters to be replaced with 'repl'

- repl:

  replacement character

## Value

sanitized string vector
