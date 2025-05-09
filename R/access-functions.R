#' Get path to access yaml
#'
#' This function checks for an environment variable 'CASCADE_ACCESS_YAML'
#' to specify directory containing access yaml. If env variable does not exist
#' uses home directory as save location.
#'
#' @return path to access yaml
#'
get_access_path <- function(){
  if(Sys.getenv('CASCADE_ACCESS_YAML') != ''){
    path <- Sys.getenv('CASCADE_ACCESS_YAML')
    if(!dir.exists(path)){
      stop(
        paste('Environment variable "CASCADE_ACCESS_YAML" exists',
              'but specified location does not exist on disk:', path)
      )
    }
  } else {
    path <- path.expand('~')
    message(
      paste('Environment variable "CASCADE_ACCESS_YAML" not found.',
            'Using default location for access yaml:', path)
    )
  }
  file.path(path, '.cascade-access.yaml')
}

#' Create access yaml
#'
#' This function creates an access yaml file.
#' This is primarily intended for the first run.
#'
#' @param user User name
#' @param user_group User group
#' @param data_area Path to data area containing RDS files
#'
#' @export
create_access_yaml <- function(user, user_group, data_area){
    ug <- setNames(as.list(user_group), user)
    da <- setNames(as.list(data_area), user_group)

    path <- get_access_path()

    write_yaml(list(user_group=ug, data_area=da),
               path)
}

#' Read access yaml with user groups and data areas
#'
#' This function reads the access yaml file and
#' returns user groups and data areas
#' as a list of data frames.
#'
#' @export
read_access_yaml <- function(){
    # figure out access file based on host
    f <- get_access_path()

    # check if yaml exists
    if(!file.exists(f)){
        stop('Access yaml not found. Have you run "create_access_yaml()" yet?')
    }

    al <- read_yaml(f)

    return(al)
}

#' Get data areas a user has access to
#'
#' This function takes a username and returns a
#' list with two elements:
#'
#' user_group: one element vector
#' data_area: vector of data areas
#'
#' @param al list with access settings; should have two elements - user_group & data_area
#' @param u user name
#' @param admin Admin user group
#'
#' @export
#'
check_user_access <- function(al, u, admin='admin'){

    # lab of user
    idx <- which(names(al$user_group) == u)
    if(length(idx) == 0){
        return(NULL)
    } else {
        user_group <- al$user_group[ idx ]

        # if admin, give access to everything
        if(admin %in% unlist(user_group)){
            data_area <- al$data_area
        } else {
            idx <- which(names(al$data_area) %in% unlist(user_group))
            if(length(idx) == 0){
                return(NULL)
            } else {
                data_area <- al$data_area[ idx ]
            }
        }
        return(list(user_group=user_group, data_area=data_area))
    }
}

#' Save access yaml to file
#'
#' This function saves access details (user groups
#' and data areas) to the designated access yaml file.
#'
#' @param lst list of data frames with user_groups and
#'  data_areas
#'
#' @export
save_access_yaml <- function(lst){
    # get access file
    f <- get_access_path()

    write_yaml(list(user_group=lst$user_group,
                    data_area=lst$data_area), f)
}

#' Get project name from path
#'
#' This function takes in a path to an RDS file and returns
#' a string to be used as project name
#'
#' So if the path is: /path/to/project/test/clustered.Rds
#' and depth=2 & end_offset=0
#' this returns: project/test
#'
#' @param x character path to RDS file
#' @param depth integer how many levels below path to look?
#' @param end_offset integer how far from the end of path to end?
#' @param staging_dir name of staging directory
#' @param fsep file separator to split path with
#'
#' @return project name
#'
#' @export
#'
get_project_name_from_path <- function(x,
                                       depth=2, end_offset=0,
                                       staging_dir='staged',
                                       fsep=.Platform$file.sep){
  d <- dirname(x)

  # split path by fsep
  tok <- unlist(strsplit(d, fsep))

  # if path contains staging_dir, increase depth
  if(any(tok == staging_dir)){
    depth <- depth + 1
  }

  # join upto 'level' elements from the end
  end_idx <- length(tok) - end_offset
  start_idx <- length(tok) - depth + 1

  p <- paste(tok[start_idx:end_idx], collapse=fsep)

  p
}

