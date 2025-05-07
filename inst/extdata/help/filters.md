### Marker Table Filters
-------------------------

Interactive controls to refine marker gene tables based on statistical significance, expression patterns, and cell clusters.

#### Purpose

Marker table filters allow you to focus on the most relevant genes by applying statistical and biological thresholds. These filters help identify genes that are:
- Statistically significant markers of cell types
- Strongly differentially expressed between groups
- Specific to particular clusters or comparisons

#### Common Controls

- **Max adjusted p-value**: Only show genes for which adjusted p-value is less than this value (default: `0.1`)
  - For `Conserved Markers`, the `max_p_val_adj` is compared to this value
  - Lower values increase stringency and reduce false positives

- **Min log2 fold-change**: Only show genes for which `avg_log2FC` is greater than this value (default: `0`)
  - Higher values focus on genes with stronger expression differences
  - Useful for identifying genes with biological significance

- **Marker type**: Filter based on marker specificity
  - `unique` (default): Only show genes that appear as markers for a single cluster
  - `all`: Show all genes regardless of how many clusters they mark
  - Note: This control is unavailable for `DE Markers`, where genes are categorized by clusters, direction of change, and comparisons

#### Filter Menus

Four specialized filter menus appear when particular columns are present in the marker table:

1. **Filter by cluster**
   - Available when the `cluster` column is present (required for all marker tables)
   - Allows filtering the marker table to show genes for specific clusters only

2. **Filter by resolution**
   - Available when the `resolution` column is present
   - Allows filtering the marker table to show genes for specific clustering resolutions only

3. **Filter by comparison**
   - Available when the `comparison` column is present (typically in `DE Markers`)
   - Allows viewing specific pairwise comparisons in the marker table

4. **Filter by group**
   - Available when the `group` column is present
   - Filters the marker table based on values in the `group` column

5. **Filter columns**
   - Unique to the `Conserved Markers` table
   - Available when multiple groups of five columns are present:
     - `X_p_val`, `X_avg_log2FC`, `X_pct.1`, `X_pct.2`, `X_p_val_adj`
   - Allows selection of specific sample group columns to display

#### Usage Tips

- Start with stringent filters (low p-value, high fold-change) to identify top markers
- Gradually relax filters to discover additional genes of interest
- Use cluster filters to focus on specific cell populations
- For conserved markers, filter by columns to compare marker consistency across conditions
- Combine multiple filters to identify genes with specific expression patterns

#### Integration with Other Modules

- Click on gene names in filtered tables to add them to the Gene Scratchpad
- Genes in the scratchpad will appear at the top of dropdown menus in visualization modules
- Use the download button to export filtered marker tables for external analysis

