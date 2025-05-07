### Marker Plot Controls
-------------------------

The Marker Plots module provides multiple visualization options for gene expression data. All plots share a common interface for selecting genes and customizing visualizations.

#### Common Controls

- **Assay**: Select which assay to use (default: `SCT`)
  - Shared by all plots except `Spatial` plots
- **Data slot**: Select which data slot to use (e.g., `data`, `counts`, `scale.data`)
- **Group by**: Metadata variable used to group values
  - For `DotPlot`, `Violin Plot` & `Line Plot`: shown on the x-axis
  - For `Violin plot`: also used to color violins in unsplit view
  - Use `Edit group levels` to customize level order and selection
- **Split by**: Metadata variable used to split plots
  - `Violin plot`: splits different levels of `Group by` variable
  - `Dotplot`: splits plot vertically by levels
  - `Feature plot` & `Coexpression Plot`: shows different levels in separate panels
- **Color map**: Choose color scheme for visualization
  - Available for `DotPlot` and `Feature Plot`

#### Plot Types

1. **Violin Plot**: Distribution of gene expression across groups
2. **Dot Plot**: Expression level (color) and percent expressing (size) across groups
3. **Feature Plot**: Gene expression on UMAP or spatial coordinates
4. **Coexpression Plot**: Compare two genes' expression patterns
5. **Gene-gene Scatter**: Compare expression of two genes in individual cells
6. **Line Plot**: Average expression across ordered groups

#### Gene Selection

- Select genes from the dropdown at the top of each plot
- Genes in the `Gene scratchpad` are prioritized in selection menus
- Click `Refresh plot` to update visualizations after changing settings

#### Additional Features

- All plots support downloading as PDF with customizable dimensions
- Interactive features allow hovering for detailed information
- Spatial plots require datasets with spatial coordinates
