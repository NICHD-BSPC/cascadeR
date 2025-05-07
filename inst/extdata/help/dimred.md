### UMAP
--------

UMAP (Uniform Manifold Approximation and Projection) is a dimension reduction techniques that place similar cells close together in a 2D plot. These visualizations help you:

- Identify distinct cell populations and clusters
- Explore relationships between cell types
- Visualize metadata variables across your dataset
- Select specific cell populations for further analysis

#### Controls and Features

- **Color By**: Choose a metadata variable to color the points
  - Categorical variables (e.g., clusters, sample ID) use discrete colors
  - Numeric variables (e.g., QC metrics) use continuous color gradients
  - The legend shows the mapping between colors and variable values

- **Split By**: Divide the visualization by a categorical variable
  - When set to 'none' (default), all cells appear in a single plot
  - When a variable is selected, separate plots are created for each category
  - Useful for comparing cell distributions across conditions or samples

- **Display Options**:
  - **Free Axes**: When enabled, each subplot uses independent axis ranges
  - **Marker Size**: Adjust point size (smaller values help with dense plots)
  - **Opacity**: Control point transparency (lower values help visualize overlapping cells)
  - **Scale Plot**: Adjust the overall size of the visualization

- **Point Selection**: Select cells directly on the plot
  - Use the box or lasso selection tools in the plot toolbar
  - Selected points can be used to create filters in the Filters panel
  - Download selected cell information using the "Download selection" button

#### Performance Considerations

- For datasets with >100,000 cells:
  - Rendering may take longer, especially with low opacity settings
  - Consider applying filters to reduce the number of displayed cells
  - Adjust marker size and opacity for optimal visualization

#### Exporting Results

- Use the "Download" button to save the current visualization as PDF
  - Customize dimensions and resolution in the download dialog

- Use "Download selection" to export metadata for selected cells/spots
  - Outputs a tab-delimited file with cell barcodes and metadata
