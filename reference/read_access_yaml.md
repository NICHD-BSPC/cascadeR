# Read access yaml with user groups and data areas

This function reads the access yaml file and returns user groups and
data areas as a list of data frames.

## Usage

``` r
read_access_yaml()
```

## Value

list with user group and data area settings

## Examples

``` r
if (FALSE) { # interactive()
local({
  access_dir <- tempfile("cascade-access-")
  dir.create(access_dir)

  old <- Sys.getenv("CASCADE_ACCESS_YAML", unset = NA_character_)
  Sys.setenv(CASCADE_ACCESS_YAML = access_dir)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("CASCADE_ACCESS_YAML")
    } else {
      Sys.setenv(CASCADE_ACCESS_YAML = old)
    }
    unlink(access_dir, recursive = TRUE)
  })

  create_access_yaml("user1", "lab1", "/data/lab1")
  read_access_yaml()
})
}
```
