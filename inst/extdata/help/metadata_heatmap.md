### Heatmap
-----------

Visualization of numeric metadata variables grouped by categorical metadata using a heatmap.

#### Features

- **Aggregated view**: Shows average values for each group
- **Multiple variables**: Compare many metadata variables simultaneously
- **Hierarchical clustering**: Option to cluster rows and/or columns
- **Scaled visualization**: Option to scale data for better comparison

#### Controls

- **Choose numeric columns**: Select which metadata variables to include
- **Cluster rows?**: Toggle hierarchical clustering of rows
- **Cluster columns?**: Toggle hierarchical clustering of columns
- **Scale by**: Choose scaling method:
  - `row`: Scale each row independently (compare patterns across groups)
  - `column`: Scale each column independently (compare patterns across variables)
  - `none`: Use raw values without scaling
- **Color map**: Select color scheme for visualization
- **Refresh**: Update the visualization after changing settings

#### Usage

- Compare QC metrics across cell types
- Visualize cell type marker scores
- Identify correlations between metadata variables
- Explore batch effects across multiple metrics

#### Export Options

- Heatmaps can be saved as PDF with customizable dimensions
