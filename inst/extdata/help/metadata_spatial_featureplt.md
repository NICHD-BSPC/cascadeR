### Spatial Feature Plot
------------------------

Visualization of numeric metadata variables in a spatial context using a continuous color gradient.

#### Requirements

- This plot is only available when spatial data is present in the loaded object
- Supports Visium and Xenium spatial transcriptomics data

#### Features

- **Spatial context**: View metadata in the original tissue context
- **Multiple variables**: Compare different metadata variables side by side
- **Multiple slices**: If multiple tissue sections are present, they can be visualized separately
- **Interactive exploration**: Hover for detailed information about specific spots/cells

#### Controls

- **Choose variable**: Select which numeric metadata variable to visualize
- **Color map**: Select color scheme for visualization
- **Marker size**: Adjust the size of points in the plot
- **Marker opacity**: Control transparency of points
- **Slice selection**: Choose which tissue section(s) to display
- **Refresh**: Update the visualization after changing settings

#### Usage

- Visualize QC metrics in spatial context
- Identify tissue regions with technical artifacts
- Explore spatial distribution of computed scores
- Validate spatial data quality

#### Export Options

- Plots can be saved as PDF with customizable dimensions
