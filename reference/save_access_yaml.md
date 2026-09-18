# Save access yaml to file

This function saves access details (user groups and data areas) to the
designated access yaml file.

## Usage

``` r
save_access_yaml(lst)
```

## Arguments

- lst:

  list of data frames with user_groups and data_areas

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

  save_access_yaml(list(
    user_group = list(user1 = "lab1", user2 = "lab2"),
    data_area = list(lab1 = "/data/lab1", lab2 = "/data/lab2")
  ))
  read_access_yaml()
})
}
```
