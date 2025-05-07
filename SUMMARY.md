# Cascade feature summary and flow

Cascade is an interactive Shiny dashboard designed for exploring and
analyzing single-cell RNA sequencing (scRNA-Seq) and spatial transcriptomics data. It provides a user-friendly interface
for researchers to visualize and interpret complex single-cell genomics data. Here is an overview of features
and logical flow used in the app.

## General workflow

- Users load a pre-processed single-cell or spatial dataset
- They can explore cell clusters in reduced dimensional space
- Identify marker genes for each cluster
- Visualize expression patterns of genes of interest
- Compare expression across different conditions or cell types
- Filter and subset data for focused analysis

## Key Features

1. Data Support

  - Compatible with both Seurat objects (.Rds files) and AnnData objects (.h5ad files) from scanpy
  - Handles both single-cell RNA-Seq and spatial transcriptomics data (including Visium and Xenium)

2. User Interface

  - Clean, modern interface with a tabbed layout for different analysis views
  - Interactive tour functionality to guide new users
  - Optional user authentication system
  - Consistent help documentation for all modules

3. Main Analysis Modules:

  - Summary: Provides an overview of the dataset with basic statistics
  - Cell Embeddings:
    - Visualizes cells in reduced dimensional space (UMAP, t-SNE, etc.)
    - Interactive plots with selection capabilities
    - Spatial visualization for spatial transcriptomics data
  - Metadata Viewer:
    - Displays and explores cell metadata and clustering information
    - Summarizes cluster characteristics
    - Visualizes metadata distributions with various plot types
  - Cluster Tree:
    - Visualizes hierarchical relationships between cell clusters
    - Three visualization modes:
      - Single: Hierarchical tree for a single clustering
      - Compare resolutions (Tree): Tree diagram showing relationships between clusters at different resolutions
      - Compare resolutions (Overlay): Overlay of cluster relationships on dimension reduction plots
  - Cell Markers:
    - Three types of marker tables:
      - Cluster Markers: Genes that define each cluster (from FindAllMarkers/rank_genes_groups)
      - Conserved Markers: Genes conserved across groups (from FindConservedMarkers)
      - DE Markers: Differentially expressed genes between conditions (from FindMarkers)
    - Interactive tables with filtering and selection capabilities
    - Integration with gene scratchpad for cross-module analysis
  - Marker Plots:
    - Multiple visualization options for gene expression:
      - Violin plots
      - Dot plots
      - Feature plots (on UMAP or spatial coordinates)
      - Co-expression plots
      - Scatter plots
      - Line plots
    - Download functionality for all plots
  - Settings:
    - Configure data directories
    - Manage user access (if authentication is enabled)

4. Interactive Features:

  - Point selection in UMAP/spatial plots
  - Gene selection from marker tables
  - Filtering capabilities for cells based on metadata and gene expression
  - Customizable plot parameters (colors, sizes, opacity, etc.)
  - Download options for plots and data
  - Gene scratchpad for tracking genes of interest across modules

5. Technical Implementation:

  - Built as an R package (cascadeR)
  - Modular design with separate UI and server components for each feature
  - Uses plotly for interactive visualizations
  - Implements a reactive programming model for responsive updates

# Logical Flow

Cascade is built as a modular Shiny application with a central data store and reactive programming model,
allowing for interactive exploration of single-cell and spatial transcriptomics data.

## Initialization and Configuration

### App Startup

- The app begins with the `run_cascade()` function, which is the main entry point
- Configuration is loaded from a YAML file using `get_config()`
- Optional user authentication is set up if credentials are provided
- The UI is constructed with a main tabbed interface and sidebar panels

### Authentication (Optional)

- If credentials are provided, the app uses `shinymanager` to handle user authentication
- User details are stored in user_details reactive values
- Admin privileges are determined based on user information

## Data Loading Flow

### Project and Analysis Selection

- The app loads available projects from configured data directories
- User selects a project from the dropdown menu
- Available analyses for the selected project are populated
- User selects an analysis and clicks "Go!" button

### Data Loading Process

- The "Go!" button triggers data loading
- App determines the object type (Seurat or AnnData) based on file extension
- Data is loaded using either `readRDS()` for Seurat objects or `read_h5ad()` for AnnData
- Metadata is extracted and processed:
  - Categorical variables are identified and levels are stored
  - Numeric variables are processed for filtering
- Marker tables are loaded if available:
  - `allmarkers.tsv` - Cluster markers
  - `consmarkers.tsv` - Conserved markers
  - `demarkers.tsv` - DE markers
- UI elements are updated based on loaded data:
  - Assay selection (for Seurat objects)
  - Dimension reduction options
  - Grouping variables (cluster identifiers)

## Data Flow Between Modules

- Central Data Store
  - The app_object reactive values object serves as the central data store
  - Contains the loaded object, metadata, marker tables, and other shared data
  - Modules access this shared data through reactive functions
- Filtering System
  - The `subsetServer` module handles data filtering
  - Filters can be applied based on:
    - Metadata (categorical or numeric)
    - Gene expression levels
    - Selected points from visualizations
  - The `apply_filters()` reactive function:
    - Processes all active filters
    - Returns a vector of cell barcodes that pass all filters
    - Is passed to all visualization modules
- Module Interactions
  - Global Settings → All Modules:
    - Assay selection
    - Dimension reduction selection
    - Grouping variable selection
    - These are passed to modules through reactive functions
  - Cell Embeddings → Filtering:
    - Selected points from UMAP/spatial plots are stored in `app_object$selected_points`
    - Can be used as filters in the subset module
  - Marker Tables → Gene Selection:
    - Selected genes from marker tables are passed to the gene scratchpad
    - Gene scratchpad is used by the marker plots module

## Visualization Flow

- Summary Module
  - Provides an overview of the dataset with basic statistics
  - Shows sample information, assay details, and dimensional reductions
  - Displays cell counts (raw and filtered if QC information is available)
  - Serves as a starting point for dataset exploration

- Cell Embeddings Module
  - Receives filtered cell barcodes from `apply_filters()`
  - Uses global settings for dimension reduction and grouping
  - Generates UMAP or spatial plots based on selected data
  - Allows point selection, which is stored in `app_object$selected_points`
  - Selection can be used for filtering in other modules

- Metadata Viewer Module
  - Provides multiple views of cell metadata:
    - Summary: Overview of categorical and numeric metadata variables
    - Cell Counts: Distribution of cells across clusters and conditions
    - Feature Plot: Visualization of metadata on UMAP or t-SNE
    - Spatial Feature Plot: Visualization of metadata on spatial coordinates
    - Heatmap: Correlation between metadata variables
  - Uses filtered cell barcodes from `apply_filters()`
  - Integrates with global settings for dimension reduction and grouping

- Cluster Tree Module
  - Uses filtered cell barcodes from `apply_filters()`
  - Provides three visualization approaches:
    - Single: Hierarchical tree for a single clustering
    - Compare resolutions (Tree): Tree diagram showing relationships between clusters
    - Compare resolutions (Overlay): Overlay of cluster relationships on dimension reduction
  - Builds cluster trees using either:
    - Seurat's `BuildClusterTree` function
    - Custom implementation for AnnData objects
  - Visualizes hierarchical relationships between clusters using `clustree`

- Marker Tables Module
  - Loads marker data from app_object
  - Provides three types of marker tables:
    - Cluster Markers: Genes that define each cluster
    - Conserved Markers: Genes conserved across groups
    - DE Markers: Differentially expressed genes between conditions
  - Applies filters based on user selections:
    - p-value and log fold change thresholds
    - Cluster selection
    - Resolution selection (if available)
    - Comparison selection (for DE markers)
  - Selected genes are passed to the gene scratchpad
  - Gene selections can be used in the marker plots module

- Marker Plots Module
  - Receives genes from gene scratchpad
  - Uses filtered cell barcodes from `apply_filters()`
  - Generates various visualizations:
    - Violin plots
    - Dot plots
    - Feature plots (on UMAP or spatial coordinates)
    - Co-expression plots
    - Scatter plots
    - Line plots
  - Plots can be customized and downloaded

## Data Refresh and Update Flow

- Global Refresh
  - The "Reload" button in the global settings triggers `reload_global()`
  - This notifies all modules to refresh their visualizations
  - Each module responds to this event independently
- Module-Specific Updates
  - Each module has its own update triggers
  - For example, marker tables have their own "Generate Table" button
  - These trigger module-specific reactive functions
- Filter Application
  - When filters are changed and applied:
  - `apply_filters()` recalculates the filtered cell barcodes
  - Modules that depend on filtered data are notified
  - Visualizations are updated with the new filtered data

## User Interaction Flow

- Tour System
  - The app includes an interactive tour using `introjs`
  - Tour steps guide users through the interface
  - Each step highlights a specific UI element with explanatory text
- Help System
  - Help buttons throughout the interface trigger modal dialogs
  - Help content is loaded from markdown files in the help directory
  - Each module has its own help content
- Download System
  - Plots can be downloaded using the download button module
  - The `downloadPlotServer` function handles the download process
  - Plots are saved in various formats (PNG, PDF)
- Gene Scratchpad
  - Central repository for genes of interest
  - Populated from marker tables and manual entry
  - Used by marker plots module for visualization
  - Persists across different modules during a session

## Settings and Configuration

- User Settings
  - The settings tab allows users to:
    - Add or remove data directories
    - Manage user access (admin only)
    - Configure other app settings
- Configuration System
  - App configuration is stored in a YAML file
  - Configuration is loaded at startup
  - Configuration is passed to modules through the config reactive function