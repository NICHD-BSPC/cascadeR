### Spatial Feature Plot
-------------------------

Visualization of marker gene expression in a spatial context using a continuous color gradient.

#### Requirements

- This plot is only available when spatial data is present in the loaded object
- Supports Visium and Xenium spatial transcriptomics data

#### Usage

- Select genes to visualize from the dropdown at the top of the page
- Click `Generate plot` to create the visualization
- Marker genes in the `Gene scratchpad` appear at the top of dropdown options

#### Features

- **Multiple genes**: Shown on the same row with a shared color scale
- **Color scale**: Ranges between minimum and maximum expression across all displayed genes
- **Negative values**: Negative expression values are ignored in visualization
- **Multiple slices**: If multiple tissue sections are present, they can be visualized separately

#### Controls

- **Marker size**: Adjust the size of points in the plot
- **Marker opacity**: Control transparency of points
- **Color map**: Select color scheme for expression visualization
- **Reverse color**: Invert the color scale direction
- **Downsample**: Option to reduce number of cells shown for improved performance
- **Slice selection**: Choose which tissue section(s) to display

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog
