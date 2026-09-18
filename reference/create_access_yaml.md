# Create access yaml

This function creates an access yaml file. This is primarily intended
for the first run.

## Usage

``` r
create_access_yaml(user, user_group, data_area)
```

## Arguments

- user:

  User name

- user_group:

  User group

- data_area:

  Path to data area containing RDS files

## Value

Invisibly returns NULL; called for the side effect of writing the access
yaml file.

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
})
}
```
