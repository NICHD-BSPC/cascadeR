#### Histogram
-------------

Visualize the distribution of numeric metadata variables across different cell groups using histograms or stacked bar charts.

### Features

- **Distribution view**: Shows the frequency distribution of values
- **Group comparison**: Compare distributions across different cell groups
- **Stacked view**: Option to show stacked distributions when using split variable

### Controls

- **Value type**: Choose between:
  - `proportion`: Show percentages instead of raw counts
  - `count`: Show absolute number of cells
- **Proportion w.r.t.**: When using proportions, calculate with respect to:
  - `Group by`: Proportions sum to 100% within each group. This shows a stacked view
    where all bars sum to 100%.
  - `Split by`: Proportions sum to 100% within each split category (default)
  - **Split plot?**: Split the plot by the splitting variable or show a
    combined plot.
- **Refresh**: Update the table after changing settings

### Usage

- Examine QC metric distributions (UMI counts, gene counts, etc.)
- Compare feature distributions across cell types
- Identify outlier populations
- Validate normalization effectiveness

### Export Options

- Plots can be saved as PDF with customizable dimensions
