### Line Plot
--------------

Visualization of average gene expression across ordered groups using an interactive line plot.

#### Usage

- Select genes to visualize from the dropdown at the top of the page
- Click `Generate plot` to create the visualization
- Marker genes in the `Gene scratchpad` appear at the top of dropdown options

#### Features

- **Trend visualization**: Shows how gene expression changes across ordered groups
- **Multiple genes**: Each gene is shown as a separate line with distinct colors
- **Split view**: Option to split plot by a metadata variable
- **Point overlay**: Option to show group averages as points on the lines

#### Controls

- **Group by**: Select metadata variable to group cells on x-axis
  - Use `Edit group levels` to customize level order and selection
- **Split by**: Optionally split plot by another metadata variable
- **Show points**: Toggle visibility of points at group averages
- **Free Y-axis**: Allow each gene to have its own Y-axis scale
- **Auto-scale**: Automatically adjust Y-axis to fit data range

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog

#### Use Cases

- Track gene expression changes across developmental stages
- Compare expression patterns across experimental conditions
- Visualize gene expression trends in spatial regions
- Identify genes with similar or opposite expression patterns

### Limitations

- Works best with ordered categorical variables (e.g., time points, spatial regions)
- For best visualization, limit to 5-10 genes at a time
- Very large datasets may have performance impacts
