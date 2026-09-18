# Cascade

**Unleash the power of your single-cell data**

Cascade is an interactive Shiny dashboard designed for exploring and
analyzing single-cell RNA sequencing (scRNA-Seq) and spatial
transcriptomics data. It provides a user-friendly interface for
researchers to visualize and interpret complex single-cell genomics
data.

## General workflow

- Load a pre-processed single-cell or spatial dataset and (optional)
  marker gene tables
- Explore cell clusters in reduced dimensional space
- Visualize expression patterns of genes of interest
- Compare expression across different conditions or cell types
- Filter and subset data for focused analysis

## Key Features

1.  Data Support

- Compatible with `Seurat`, `SingleCellExperiment` (`.Rds` files) and
  `AnnData` objects (`.h5ad` files) from scanpy
- Handles both single-cell RNA-Seq and spatial transcriptomics data
  (including Visium and Xenium)

2.  User Interface

- Clean, modern interface with a tabbed layout for different analysis
  views
- Interactive tour functionality to guide new users
- Optional user authentication system
- Consistent help documentation for all modules

3.  Main Analysis Modules:

- *Summary*: Provides an overview of the dataset with basic statistics
- *Cell Embeddings*:
  - Visualizes cells in reduced dimensional space (UMAP, t-SNE, etc.)
  - Interactive plots with split view and selection capabilities
  - Spatial visualization for spatial transcriptomics data
- *Metadata Viewer*:
  - Displays and explores cell metadata and clustering information
  - Summarizes cluster characteristics
  - Visualizes metadata distributions with various plot types
- *Cluster Tree*:
  - Visualizes hierarchical relationships between cell clusters
  - Three visualization modes:
    - Single: Hierarchical tree for a single clustering
    - Compare resolutions (Tree): Tree diagram showing relationships
      between clusters at different resolutions
    - Compare resolutions (Overlay): Overlay of cluster relationships on
      dimension reduction plots
- *Cell Markers*:
  - Three types of marker tables:
    - Cluster Markers: Genes that define each cluster (e.g. from
      `FindAllMarkers` (Seurat) or `rank_genes_groups` (scanpy).
    - Conserved Markers: Genes conserved across groups (from
      [`Seurat::FindConservedMarkers`](https://satijalab.org/seurat/reference/FindConservedMarkers.html))
    - DE Markers: Differentially expressed genes between conditions
      (from
      [`Seurat::FindMarkers`](https://satijalab.org/seurat/reference/FindMarkers.html)
      or pseudo-bulk analysis from `DESeq2`)
  - Interactive tables with filtering and selection capabilities
  - Integration with gene scratchpad for cross-module analysis
- *Marker Plots*:
  - Multiple visualization options for gene expression:
    - Violin plots
    - Dot plots
    - Feature plots (on UMAP or spatial coordinates)
    - Co-expression plots (on UMAP or spatial coordinates)
    - Scatter plots
  - Download functionality for all plots
- *Settings*:
  - Configure data directories
  - Manage user access (if authentication is enabled)

4.  Interactive Features:

- Point selection in UMAP/spatial cell-embeddings or marker plots
- Gene selection from marker tables
- Filtering capabilities for cells based on metadata, gene expression or
  lasso selection
- Customizable plot parameters (colors, sizes, opacity, etc.)
- Download options for plots and data
- Gene scratchpad for tracking genes of interest across modules

## Installation

`CascadeR` can be installed using
[`BiocManager::install`](https://bioconductor.github.io/BiocManager/reference/install.html).
First, start R (version: 4.6) and then run:

``` r

# first check to see if BiocManager is available
if(!requireNamespace('BiocManager', quietly=TRUE)){
  install.packages('BiocManager')
}

BiocManager::install('cascadeR')
```

To install the ‘devel’ version:

``` r

BiocManager::install('cascadeR', version='devel')
```

### conda

An alternative way to get started with cascadeR is through conda, which
handles all dependencies automatically:

``` bash
# Create environment outside the cascadeR directory
cd .. && conda env create -p env --file cascadeR/requirements-pinned.yaml
conda activate ./env
R
```

Then install the package with the `remotes` package. Here we set
upgrade=‘never’ to make sure the conda-installed package versions remain
unchanged.

``` r

remotes::install_github('NICHD-BSPC/cascadeR@r4.5', upgrade='never')
```

**Note:**

Conda packages for R \>= 4.6.0 may not be available yet causing
installation using the default github branch to fail. To avoid this, use
branch r4.3 which pins R to a lower version.

``` bash
# Create environment outside the cascadeR directory
cd .. &&  env create -p env --file cascadeR/requirements-pinned.yaml
conda activate ./env
```

## Getting Started

### Data Organization

Organize your data in a directory structure that Cascade can easily
navigate:

    /cascade/data/
      ├─ project1
      │  ├─ seurat
      │  │  ├─ clustered.Rds
      │  │  └─ allmarkers.tsv
      │  │
      │  └─ scanpy
      │     ├─ clustered.h5ad
      │     └─ allmarkers.tsv
      │
      └─ project2
         └─ seurat5
            └─ clustered.Rds

### Launch Cascade

First, load the library and install required Python dependencies:

``` r

library(cascadeR)
install_cascade()  # Installs plotly and kaleido for interactive plots
run_cascade()      # Launch the app!
```

The first time you run Cascade, you’ll be prompted to choose a data
directory. Point it to your data location (e.g., `/cascade/data`), and
you’re ready to explore!

### Remote Access

Running Cascade on a remote server? No problem:

``` r

run_cascade(options=list(port=12345, launch.browser=FALSE))
```

Then access the app at `http://127.0.0.1:12345` through your SSH tunnel.

## Documentation

Each module includes comprehensive help documentation accessible through
the help buttons throughout the interface.

## Contributing

We welcome contributions to Cascade! Please feel free to submit issues
or pull requests to the GitHub repository.

## License

Cascade is available under the MIT license.
