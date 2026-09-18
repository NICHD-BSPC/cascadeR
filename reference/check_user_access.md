# Get data areas a user has access to

This function takes a username and returns a list with two elements:

## Usage

``` r
check_user_access(al, u, admin = "admin")
```

## Arguments

- al:

  list with access settings; should have two elements - user_group &
  data_area

- u:

  user name

- admin:

  Admin user group

## Value

list with user_group and data_area entries, or NULL if no access is
found

## Details

user_group: one element vector data_area: vector of data areas

## Examples

``` r
# save access details to file
home <- Sys.getenv('HOME')

# create carnation data area if it doesn't exist
cascade_home <- file.path(home, 'cascade/data')
if(!dir.exists(cascade_home)) dir.create(cascade_home)
#> Warning: cannot create dir '/home/runner/cascade/data', reason 'No such file or directory'

create_access_yaml(user = 'admin',
                   user_group = 'admin',
                   data_area = cascade_home)
#> Environment variable "CASCADE_ACCESS_YAML" not found.Using default location for access yaml:/home/runner

# get current user access details
al <- read_access_yaml()
#> Environment variable "CASCADE_ACCESS_YAML" not found.Using default location for access yaml:/home/runner

lst <- check_user_access(al, u='admin')
```
