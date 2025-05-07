### Violin Plot
---------------

Visualization of gene expression distribution across cell groups using violin plots.

#### Usage

- Select genes to visualize from the dropdown at the top of the page
- Click `Generate plot` to create the visualization
- Marker genes in the `Gene scratchpad` appear at the top of dropdown options

#### Features

- **Distribution view**: Shows the full distribution of expression values
- **Multiple genes**: Each gene is shown in a separate row
- **Split view**: Option to split violins by a metadata variable
- **Point overlay**: Option to show individual cell expression values as points

#### Controls

- **Group by**: Select metadata variable to group cells on x-axis
- **Split by**: Optionally split violins by another metadata variable
- **Point size**: Adjust the size of individual cell points (when shown)
- **Point opacity**: Control transparency of points
- **Show points**: Toggle visibility of individual cell expression values
- **Scale data**: Option to use scaled expression values

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog

#### Limitations

- For best visualization, limit to 5 genes at a time
- Very large datasets may have performance impacts when showing points
