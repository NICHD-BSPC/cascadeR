### Gene Scratchpad
--------------------

A central repository for tracking genes of interest across all visualization modules.

#### Purpose

The Gene Scratchpad serves as a persistent list of genes you want to track throughout your analysis session. It provides a convenient way to:

- Keep important genes readily accessible across different visualization tabs
- Prioritize genes of interest in selection dropdowns
- Build and refine gene sets for cell type characterization

#### Features

- **Persistent storage**: Genes remain in the scratchpad throughout your session
- **Priority display**: Scratchpad genes appear at the top of gene selection dropdowns in all marker plot tabs
- **Auto-complete**: Real-time gene name suggestions as you type
- **Validation**: Automatic checking of gene names against the dataset
- **Multiple sources**: Add genes from marker tables, manual entry, or literature

#### Usage

- **Adding genes**:
  - Type gene names directly into the scratchpad input field
  - Click on genes in marker tables and use "Add to scratchpad" button
  - Select multiple genes from tables by holding Shift or Ctrl while clicking
  
- **Removing genes**:
  - Click in the box and use backspace to remove individual genes
  - Use the "Reset" button to clear all genes at once

- **Using scratchpad genes**:
  - Genes in the scratchpad will automatically appear at the top of dropdown menus in:
    - Feature Plot
    - Violin Plot
    - Dot Plot
    - Co-expression Plot
    - Gene-Gene Scatter Plot
    - Line Plot
    - Spatial Feature Plot

#### Tips

- Add a small set of key marker genes at the beginning of your analysis session. Comma-separated lists can be pasted directly into the scratchpad input field.
- Group genes by cell type or biological pathway for systematic exploration
- Use the scratchpad to compare expression of related genes across visualizations
- When collaborating, share your gene list to ensure consistent analysis

#### Limitations

- Gene names must match exactly as they appear in the dataset
- The scratchpad is session-specific and will reset when the app is restarted
