### Conserved Markers
--------------------

Table summarizing results of Seurat's `FindConservedMarkers()` function, which attempts to find marker genes conserved across groups.

#### Features

- **Cross-group analysis**: Identifies markers consistent across conditions
- **Group-specific statistics**: Shows metrics for each group separately
- **Conservation metrics**: Summarizes consistency across groups
- **Gene selection**: Click on genes to add them to the gene scratchpad via the selection settings menu

#### Controls

- **Filter by p-value**: Set maximum adjusted p-value threshold
- **Filter by log2FC**: Set minimum log2 fold-change threshold
- **Filter by cluster**: Show markers for specific clusters only
- **Filter by resolution**: For multi-resolution objects, filter by clustering resolution
- **Filter by group**: For grouped analyses, filter by specific group
- **Filter by samples**: Show only specific samples/conditions
- **Search**: Filter table to show only genes matching your search term

#### Key Columns

- `gene`: Gene symbol
- `cluster`: Cluster name
- `resolution`: (optional) Clustering resolution
- `group`: (optional) Custom grouping column
- `nclusters`: Number of clusters for which the gene is a marker
- `max_pval`, `minimump_p_val`: Max & min p-value for gene across groups

Additional columns contain `avg_log2FC`, `p_val`, `p_val_adj`, `pct.1` & `pct.2` values for each group, which appear
under group name subheaders.

#### Usage

- Find genes that define cell types consistently across conditions
- Identify robust cell type markers for annotation
- Discover genes that are condition-independent within cell types
- Select conserved genes for visualization in the Marker Plots module

#### Export Options

- Table can be downloaded as TSV file using the download button
- Selected genes can be added to the gene scratchpad for use in other modules
