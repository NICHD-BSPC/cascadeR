# Cascade

**Unleash the power of your single-cell data**

Cascade is an interactive dashboard that transforms complex single-cell RNA-Seq and spatial transcriptomics data (Visium and Xenium) into beautiful, insightful visualizations. Designed for both computational and experimental biologists, Cascade makes exploring your data intuitive and exciting.

## Key Features

- **Universal Compatibility**: Works seamlessly with Seurat (`.Rds`) and scanpy (`.h5ad`) objects
- **Lightning-Fast Visualizations**: Interactive plots powered by `plotly` that respond in real-time
  - Violin plots - Compare expression across clusters
  - Dot plots - Visualize expression prevalence and intensity
  - Feature plots - Map gene expression onto UMAP or spatial coordinates
  - Co-expression plots - Discover genes with similar expression patterns
  - Scatter plots - Explore relationships between genes
  - Line plots - Track expression trends across conditions
- **Spatial Transcriptomics**: Visualize gene expression in tissue context with Visium and Xenium support
- **Interactive Cell Selection**: Lasso select cells of interest directly on UMAP/spatial plots and export barcodes for downstream analysis
- **Smart Filtering**: Filter cells based on metadata and gene expression with an intuitive interface
- **Cluster Relationships**: Visualize hierarchical relationships between cell clusters with beautiful tree plots
- **Gene Tracking**: Keep your genes of interest at your fingertips with the "Gene scratchpad"
- **Powerful Marker Analysis**: Interactive marker tables with advanced filtering capabilities
- **Flexible Deployment**: Run locally for personal analysis or on a server to share with collaborators
- **User Management**: Optional authentication system for controlled access in multi-user environments

## Installation

### conda (recommended)

The easiest way to get started with Cascade is through conda, which handles all dependencies automatically:

```bash
# Create environment outside the cascadeR directory
cd .. &&  env create -p env --file cascadeR/requirements-pinned.yaml
conda activate ./env
```

Then install the package using one of these methods:

**Option 1**: Using `remotes::install_github`

Note that here we use `upgrade='never'` to leave the conda installed package versions unchanged.

```r
remotes::install_github('NICHD-BSPC/cascadeR', upgrade='never')
```

**Option 2**: Using `R CMD build` and `install.packages`:

```bash
R CMD build cascadeR/
Rscript -e "install.packages('cascadeR_1.0.tar.gz', repos=NULL)"
```

### remotes

Alternatively, install directly with `remotes`:

```r
install.packages('remotes')
setRepositories(ind=c(1,2,3,4,5))  # Get both CRAN and Bioconductor packages
remotes::install_github('NICHD-BSPC/cascadeR')
```

## Getting Started

### Data Organization

Organize your data in a directory structure that Cascade can easily navigate:

```
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
```

### Launch Cascade

First, load the library and install required Python dependencies:

```r
library(cascadeR)
install_cascade()  # Installs plotly and kaleido for interactive plots
run_cascade()      # Launch the app!
```

The first time you run Cascade, you'll be prompted to choose a data directory. Point it to your data location (e.g., `/cascade/data`), and you're ready to explore!

### Remote Access

Running Cascade on a remote server? No problem:

```r
run_cascade(options=list(port=12345, launch.browser=FALSE))
```

Then access the app at `http://127.0.0.1:12345` through your SSH tunnel.

## Server Mode with Authentication

For multi-user environments, Cascade supports authentication:

```r
# Create user database
credentials <- data.frame(
  user = c('shinymanager'),
  password = c('12345'),
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# Initialize the database
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = 'credentials.sqlite',
  passphrase = 'admin_passphrase'
)

# Run with authentication
run_cascade(credentials='credentials.sqlite', passphrase='admin_passphrase')
```

## Exploring Your Data

Once your data is loaded, Cascade offers multiple ways to explore:

- **Summary Tab**: Get a quick overview of your dataset
- **Cell Embeddings**: Visualize cells in UMAP or spatial context
- **Metadata Viewer**: Explore cell metadata and cluster characteristics
- **Cluster Tree**: Understand relationships between cell clusters
- **Cell Markers**: Identify and filter marker genes for each cluster
- **Marker Plots**: Create beautiful visualizations of gene expression
- **Settings**: Configure data directories and user access

## Documentation

Each module includes comprehensive help documentation accessible through the help buttons throughout the interface.

## Contributing

We welcome contributions to Cascade! Please feel free to submit issues or pull requests to the GitHub repository.

## License

Cascade is available under the MIT license.
