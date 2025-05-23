% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{install_cascade}
\alias{install_cascade}
\title{Create carnation python environment}
\usage{
install_cascade(envname, ...)
}
\arguments{
\item{envname}{name of the python environment}

\item{...}{parameters passed to reticulate::py_install}
}
\description{
This function installs 'plotly' and 'kaleido' python packages
in an environment to allow PDF downloads from plotly plots,
and 'anndata' to allow h5ad files.
}
