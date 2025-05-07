### Cluster Markers
------------------

Table summarizing results from Seurat's `FindAllMarkers()` or scanpy's `rank_gene_groups` functions, which compare cells in each cluster to all other cells.

#### Features

- **Comprehensive comparison**: Each cluster compared against all other cells
- **Multiple metrics**: p-values, fold changes, and percent expression
- **Interactive filtering**: Filter by cluster, resolution, and statistical thresholds
- **Gene selection**: Click on genes to add them to the gene scratchpad via the selection settings menu

#### Controls

- **Filter by p-value**: Set maximum adjusted p-value threshold
- **Filter by log2FC**: Set minimum log2 fold-change threshold
- **Filter by cluster**: Show markers for specific clusters only
- **Filter by resolution**: For multi-resolution objects, filter by clustering resolution
- **Filter by group**: For grouped analyses, filter by specific group
- **Search**: Filter table to show only genes matching your search term

#### Column key

- `gene`: Gene symbol
- `cluster`: Cluster name
- `resolution`: (optional) Clustering resolution
- `group`: (optional) Custom grouping column
- `nclusters`: Number of clusters for which the gene is a marker
- `p_val`: p-value from Wilcoxon rank-sum test
- `avg_log2FC`: Log fold-change of mean expression in cluster vs rest of cells
- `pct.1`: % of cells in the cluster expressing the gene
- `pct.2`: % of other cells expressing the gene
- `p_val_adj`: Adjusted p-value

Note that the p-value and log-fold change columns might be named differently for scanpy results.

#### Usage

- Identify cell type-specific marker genes
- Discover genes that uniquely define each cluster
- Select genes for visualization in the Marker Plots module
- Build gene signatures for cell type annotation

#### Export Options

- Table can be downloaded as TSV file using the download button
- Selected genes can be added to the gene scratchpad for use in other modules

