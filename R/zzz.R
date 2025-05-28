# global variable to store cascadeR python env
cascade <- "r-cascade"

.onLoad <- function(...){
  reticulate::use_virtualenv(cascade, required=FALSE)
}

#' Create cascade python environment
#'
#' This function installs 'plotly' and 'kaleido' python packages
#' in an environment to allow PDF downloads from plotly plots,
#' and 'anndata' to allow h5ad files.
#'
#' @param envname name of the python environment
#' @param ... parameters passed to reticulate::py_install
#'
#' @export
install_cascade <- function(envname, ...) {
  if(missing(envname)) envname <- cascade
  reticulate::py_install(c("plotly", "kaleido", "anndata"),
                         envname = envname, ...)
}
