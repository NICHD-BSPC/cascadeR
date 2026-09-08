#' Create a tiny example Seurat object
#'
#' This creates a small normalized Seurat object with an RNA assay, metadata,
#' identities, and a two-dimensional UMAP reduction. It is intended for
#' documentation examples and lightweight module demos.
#'
#' @param genes character vector of gene names to include.
#' @param cells character vector of cell names to include.
#' @param assay assay name to use when creating the object.
#'
#' @return A Seurat object.
#'
#' @examplesIf interactive()
#' obj <- make_example_seurat_object()
#' obj
#'
#' @export
make_example_seurat_object <- function(
  genes = paste0("Gene", LETTERS[1:4]),
  cells = paste0("cell", seq_len(8)),
  assay = "RNA"
) {
  base_counts <- matrix(
    c(
      5, 0, 3, 0, 2, 0, 1, 0,
      0, 4, 0, 2, 0, 3, 0, 1,
      1, 1, 1, 1, 0, 0, 2, 2,
      0, 0, 2, 2, 4, 4, 0, 0
    ),
    nrow = 4,
    byrow = TRUE
  )
  counts <- base_counts[
    rep(seq_len(nrow(base_counts)), length.out = length(genes)),
    rep(seq_len(ncol(base_counts)), length.out = length(cells)),
    drop = FALSE
  ]
  dimnames(counts) <- list(genes, cells)
  counts <- Matrix::Matrix(counts, sparse = TRUE)

  n <- length(cells)
  metadata <- data.frame(
    orig.ident = factor(
      rep(c("sample1", "sample2"), each = ceiling(n / 2), length.out = n),
      levels = c("sample1", "sample2")
    ),
    cluster = factor(
      rep(c("A", "B"), each = ceiling(n / 2), length.out = n),
      levels = c("A", "B")
    ),
    condition = factor(
      rep(c("ctrl", "stim"), length.out = n),
      levels = c("ctrl", "stim")
    ),
    seurat_clusters = factor(
      rep(c("0", "1"), each = ceiling(n / 2), length.out = n),
      levels = c("0", "1")
    ),
    quality_score = seq_along(cells),
    row.names = cells
  )

  obj <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = assay,
    meta.data = metadata,
    project = "cascadeR-example"
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj <- SeuratObject::`Idents<-`(obj, value = metadata$cluster)

  embeddings <- cbind(
    UMAP_1 = seq(-2, 2, length.out = length(cells)),
    UMAP_2 = rep(c(-1, 1), length.out = length(cells))
  )
  rownames(embeddings) <- cells

  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embeddings,
    key = "UMAP_",
    assay = assay
  )

  obj
}
