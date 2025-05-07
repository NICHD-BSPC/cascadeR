### Compare Resolutions (Overlay)
---------------------------------

Overlays clustering relationships on dimension reduction plots to visualize how clusters relate spatially across different resolutions.

#### Features

- **Spatial context**: View cluster relationships in the context of dimension reduction
- **Multi-resolution view**: Compare clusters across different resolutions
- **Side views**: Optional x-side and y-side projections for better visualization
- **Node labeling**: Option to label nodes with cluster identifiers

#### Controls

- **Assay**: Which assay to use for creating the cluster tree (for Seurat objects)
  - If the `SCT` assay is found in the object, this is chosen by default
- **Dimension reduction**: Dimension reduction to use for creating the cluster tree
- **Cluster column prefix**: Prefix matching metadata columns containing clusterings
- **Replace with**: String to replace cluster column prefix in labels (default: `res.`)
- **Clustering resolutions**: Choose which clustering resolutions to include.
  - These are obtained by replacing the *Cluster column prefix* with the *Replace with* string.
  - Use *all* & *none* buttons to quickly select or deselect all
- **Label nodes?**: If `yes`, nodes are labeled with a string, e.g. if *Replace with* is `res.`,
  then cluster 1 for resolution `0.1` will be labeled `res.0.1C1`
- **Side view**: Use this to show the overlay plot from the `x-side` or `y-side` (default: `no`)
- **Minimum incoming node proportion**: Threshold for displaying arrows
- **Generate plot**: Click to create or update the visualization
  - Note: This operation can be computationally intensive

#### How to Interpret

- Each node represents a cluster and the *color* of nodes represents different clusterings
- The *color* of arrows is the same as their nodes of origin, while the *transparency* depicts
  the incoming node proportion
- The *size* of nodes represents the number of cells in the node
- Since nodes can overlap in the overlay view, the `Side view` control can be used to get
  a view from the x-axis side or the y-axis side

#### Usage

- Visualize how clusters are positioned in reduced dimensional space
- Understand spatial relationships between clusters at different resolutions
- Identify regions where clusters split or merge as resolution changes
- Determine if clusters are well-separated in dimension reduction space

#### Export Options

- Click the `Download` button to save the current plot to PDF
- Edit plot dimensions and image resolution in the popup dialog
