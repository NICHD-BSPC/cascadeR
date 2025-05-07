### Spatial plot
----------------

For spatial transcriptomics data (e.g., 10x Visium), the Spatial Plot shows cells in their original tissue context, allowing you to explore spatial patterns of cell types and gene expression.

#### Controls and Features

- **Color By**: Choose a metadata variable to color the spots/cells
  - Works the same as in UMAP visualization

- **Interactive Mode**:
  - **Interactive** (default): Shows spots without the tissue image background
    - **Marker Size** & **Opacity**: Control spot appearance
    - **Scale Plot**: Adjust the overall visualization size

  - **Static Image**: Shows spots overlaid on the tissue image
    - **Colors to Show**: Select which categories to display
    - **Image Opacity**: Control tissue image transparency
    - **Marker Opacity** & **Scale Markers**: Adjust spot appearance
    - **Label Markers**: Toggle spot labels on/off

- **Slice/Image Selection**: For multi-slice datasets, choose which tissue section to display

- **Point Selection**: Select spots directly on the plot
  - Selected spots can be used to create filters
  - Useful for analyzing specific tissue regions

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


