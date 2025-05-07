### DE Markers
------------

Table summarizing differential expression analysis results between groups, with comparisons performed within each cluster using `FindMarkers()`.

#### Features

- **Within-cluster comparison**: Compares groups within each cluster
- **Direction annotation**: Indicates up/down regulation
- **Multiple comparisons**: Supports various group comparisons
- **Gene selection**: Click on genes to add them to the gene scratchpad via the selection settings menu

#### Controls

- **Filter by p-value**: Set maximum adjusted p-value threshold
- **Filter by log2FC**: Set minimum log2 fold-change threshold
- **Filter by cluster**: Show markers for specific clusters only
- **Filter by resolution**: For multi-resolution objects, filter by clustering resolution
- **Filter by group**: For grouped analyses, filter by specific group
- **Filter by comparison**: Show only specific group comparisons
- **Search**: Filter table to show only genes matching your search term

#### Column key

- `gene`: Gene symbol
- `cluster`: Cluster name
- `resolution`: (optional) Clustering resolution
- `group`: (optional) Custom grouping column
- `comparison`: Description of groups being compared
- `p_val`: p-value from Wilcoxon rank-sum test
- `avg_log2FC`: Log fold-change between compared groups
- `pct.1`: % of cells in group 1 expressing the gene
- `pct.2`: % of cells in group 2 expressing the gene
- `p_val_adj`: Adjusted p-value
- `direction`: Direction of change (`up` or `dn`)
- `ngene.cluster.directional`: (optional) Number of times this gene was DE across multiple comparisons within the same cluster
- `ngene.comparison.directional`: (optional) Number of times the gene was DE in multiple clusters in the same comparison

Note that the p-value and log-fold change columns might be named differently for scanpy results.

#### Usage

- Discover differentially expressed genes between experimental conditions
- Identify condition-specific changes within each cell type
- Find genes that respond to treatment across multiple cell types
- Select condition-specific genes for visualization in the Marker Plots module

#### Export Options

- Table can be downloaded as TSV file using the download button
- Selected genes can be added to the gene scratchpad for use in other modules
