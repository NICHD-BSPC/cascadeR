% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marker_tables_general.R
\name{markerTableGeneralServer}
\alias{markerTableGeneralServer}
\title{Generalize marker table module server}
\usage{
markerTableGeneralServer(
  id,
  obj,
  type,
  genes_to_plot,
  reset_genes,
  global_args,
  args,
  reload_global,
  config
)
}
\arguments{
\item{id}{Input id}

\item{obj}{reactive list with marker tables}

\item{type}{string, can be 'allmarkers', 'consmarkers' or 'demarkers'}

\item{genes_to_plot}{reactive list of genes to plot}

\item{reset_genes}{reactive to trigger gene selection reset}

\item{global_args}{reactive list with global settings}

\item{args}{reactive list with 'max_padj', 'max_lfc'}

\item{reload_global}{reactive to trigger global args reload}

\item{config}{reactive list with config settings}
}
\description{
Generalize marker table module server
}
