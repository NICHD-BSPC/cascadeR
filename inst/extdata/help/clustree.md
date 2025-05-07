### Compare Resolutions (Tree)
------------------------------

Compares different clustering resolutions as a tree diagram where nodes represent clusters and arrows show relationships between them.

#### Features

- **Multi-resolution view**: Compare clusters across different resolutions
- **Flow visualization**: Arrows show how cells move between clusters at different resolutions
- **Quantitative relationships**: Arrow thickness and transparency encode relationship strength
- **Node sizing**: Node size represents number of cells in each cluster

#### Controls

- **Assay**: Which assay to use for creating the cluster tree (for Seurat objects)
  - If the `SCT` assay is found in the object, this is chosen by default
- **Cluster column prefix**: Prefix matching metadata columns containing clusterings
  - Note: It must be possible to convert column names to numeric after removing the prefix
- **Replace with**: String to replace cluster column prefix in labels (default: `res.`)
- **Clustering resolutions**: Choose which clustering resolutions to include
  - These are obtained by replacing the *Cluster column prefix* with the *Replace with* string.
  - Use *all* & *none* buttons to quickly select or deselect all
- **Minimum incoming node proportion**: Threshold for displaying arrows
  - Defined as: (cells in edge)/(cells in node it points to)
  - Arrows with values below this threshold are hidden
- **Generate plot**: Click to create or update the visualization
  - Note: This operation can be computationally intensive

#### How to Interpret

- Each node represents a cluster, with nodes in a single resolution shown on the same row
- The *color* of nodes represents different clusterings/resolutions
- Arrows connect nodes that have common cells
- The *color* of arrows represents the number of cells shared between connected nodes
- The *transparency* of arrows represents 'incoming node proportion'

#### Usage

- Compare clustering results across multiple resolutions
- Identify stable clusters that persist across resolutions
- Determine optimal clustering resolution for your analysis
- Understand how clusters split or merge as resolution changes

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog
