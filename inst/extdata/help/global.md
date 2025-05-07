### Global Settings
--------------------

Central controls that affect visualizations throughout the application.

#### Purpose

The Global Settings panel provides a unified way to configure key parameters used across multiple visualization modules. Changes made here will affect most plots and tables in the application, allowing for consistent analysis across different views.

#### Available Settings

- **Assay**: Select which data assay to use for visualizations when using Seurat objects, e.g., RNA, SCT, integrated, etc.
  
- **Reductions**: Choose which dimension reduction to use for plotting
  - UMAP (default if available)
  - PCA
  - Other custom reductions in your dataset
  
- **Cluster by**: Select metadata column to use for cell clustering

#### Usage

1. Open the Global Settings panel by clicking the gear icon in the top navigation bar
2. Adjust the settings as needed for your analysis
3. Click the `Reload` button to apply changes
4. All visualizations will update to reflect your new settings

#### Effects on Modules

Changes to global settings affect the following modules:

- **Cell Embeddings**: Updates dimension reduction type and coloring
- **Metadata Viewer**: Updates grouping variables and visualizations
- **Cluster Tree**: Updates clustering used for tree generation
- **Marker Plots**: Updates assay and grouping for all plot types

#### Tips

- Use consistent settings across your analysis session for comparable results
- The selected assay determines which gene expression values are displayed
- Different dimension reductions may reveal different aspects of your data
- Experiment with different clustering resolutions to find the optimal level of granularity

#### Notes

- Some plots have their own local settings that can be used to override global settings
- The `Reload` button must be clicked to apply changes and refresh visualizations
- Available options depend on your loaded dataset and may vary between analyses