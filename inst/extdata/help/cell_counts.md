### Metadata Viewer Controls
-----------------------------

The Metadata Viewer module provides multiple ways to visualize and analyze the distribution of cells across different metadata categories.

#### Common Controls

- **Group by**: Primary grouping variable for cells (default: `orig.ident`)
  - This control is available for all visualization tabs
  - For `Histogram` and `Heatmap` plots, this variable is shown on the x-axis
- **Split by**: Secondary grouping variable for cells (default: `none`)
  - This control is available for all visualization tabs
  - For `Histogram`, specifying a splitting variable will convert the histogram into stacked barplots
  - For `Feature plot`, levels of the splitting variable are shown in separate subplots
  - For spatial datasets, the `Slice` menu provides similar functionality by allowing selection of specific tissue sections
- **Edit group/split levels**: Customize which levels to display and their order using the `Edit group levels` or `Edit split levels` menus

#### Tab-Specific Controls

- **Cluster Distribution**

  - **Value type**: Choose data representation format
    - `proportion`: Show percentages instead of raw counts (default)
    - `count`: Show absolute number of cells
  - **Proportion w.r.t.**: When using proportions, calculate with respect to:
    - `Group by`: Proportions sum to 100% within each group
    - `Split by`: Proportions sum to 100% within each split category (default)
  - **Split plot?**: Toggle between combined or split visualization
    - Available only in the `Histogram` tab
  - **Refresh**: Update the visualization after changing settings

- **Feature Plot**

  - **Free axes?**: Allow each subplot to have independent axis scales
    - This is ignored in non-split views
  - **Color map**: Select color scheme for visualization
  - **Marker size**: Adjust the size of points in the plot
  - **Opacity**: Control transparency of points shown on the plot

- **Heatmap**

  - **Choose numeric columns**: Select which numeric metadata variables to include
    - By default, all numeric columns are selected when loading a new dataset
  - **Cluster rows?**: Toggle hierarchical clustering of rows (default: `yes`)
  - **Cluster columns?**: Toggle hierarchical clustering of columns (default: `yes`)
  - **Scale by**: Choose scaling method:
    - `row`: Scale each row independently (default)
    - `column`: Scale each column independently
    - `none`: Use raw values without scaling

