#### Filter settings
--------------------

Use these settings to create filters for the data. The view is divided into two main sections:

- The top panel shows the `Current filters` in use.
  A summary of cells before and after filtering are also
  shown alongside.
- In the bottom section specific filters can be created
  using the `Add/edit filter` menu.

**Add/edit filter**

Three types of filters, selected via `Type of filter`, are currently supported: `metadata`,
`gene` and `selection`:

1. `metadata`

   - First select a metadata column using the `Choose variable`
     menu. Then a panel is shown below depending on the type of variable.
   - For factor/character variables, e.g. `seurat_clusters`, the current levels
     of the variable are shown. `Select none` or `Select all`
     buttons can be used to quickly empty or show all available
     levels.
   - For numeric columns, e.g. `nCount_RNA`, a histogram of values
     are shown along with a slider, which can be used to select a range
     of values.

2. `gene`

   - First select a gene to use for filtering with the `Choose gene` menu.
     Then a histogram of expression values for the gene are shown along with
     a slider, which can be used to select a range of expression for filtered
     cells.
   - Note that data from the `Assay` selected in global settings is used
     for generating the histogram.

3. `selection`

   For this type of filter, a selection either from the `UMAP` or `Spatial` plots
   can be used.

Note that, this menu serves as an 'edit' filter if a previously selected metadata
variable or gene are selected here.

- After selecting the type of filter and the range of values/selections,
  click the `Add filter` button, to add it to the list of current filters.
- Finally, click `Apply` to apply current filters to the data set.
- Click `Reset` to clear any applied filters and reset the data.
- `Save filter set` can be used to save a set of filters for use during a session.
  Clicking this button opens a dialog where you can specify the name for the filter set.
- `Load filter set` opens a dialog from where a saved filter set can be applied
  to the current data set.
  - Note that, this will clear any unsaved changes from the `Add/edit filter` menu.
