### Dot Plot
------------

Visualization of gene expression across cell groups where dot size represents percent of cells expressing the gene and color intensity represents average expression level.

#### Usage

- Select genes to visualize from the dropdown at the top of the page
- Click `Generate plot` to create the visualization
- Marker genes in the `Gene scratchpad` appear at the top of dropdown options

#### Features

- **Dual encoding**: Color for expression level, size for percent expressing
- **Multiple genes**: Genes are shown on the y-axis
- **Split view**: Option to split plot by a metadata variable
- **Scale options**: Control how expression values are scaled

#### Controls

- **Group by**: Select metadata variable to group cells on x-axis
- **Split by**: Optionally split plot by another metadata variable
- **Color map**: Select color scheme for expression visualization
- **Dot scale**: Adjust the maximum size of dots
- **Scale data**: Option to use scaled expression values
- **Scale by**: Choose whether to scale data across rows or columns

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog

#### Limitations

- For best visualization, limit to 30 genes at a time
- When using split view, the limit is 30 genes across all groups combined
