make_test_counts <- function(
  genes = paste0("Gene", LETTERS[1:4]),
  cells = paste0("cell", seq_len(8))
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

  Matrix::Matrix(counts, sparse = TRUE)
}

make_test_metadata <- function(cells = paste0("cell", seq_len(8))) {
  n <- length(cells)

  data.frame(
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
}

make_test_seurat <- function(
  counts = make_test_counts(),
  metadata = make_test_metadata(colnames(counts)),
  assay = "RNA"
) {
  obj <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = assay,
    meta.data = metadata,
    project = "cascadeR-test"
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  if ("cluster" %in% colnames(metadata)) {
    obj <- SeuratObject::`Idents<-`(obj, value = metadata$cluster)
  }

  if ("CreateDimReducObject" %in% getNamespaceExports("SeuratObject")) {
    cells <- colnames(obj)
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
  }

  obj
}

make_test_metadata_levels <- function(metadata) {
  metadata <- as.data.frame(metadata)
  level_columns <- vapply(
    metadata,
    function(x) is.factor(x) || is.character(x),
    logical(1)
  )

  lapply(metadata[level_columns], function(x) {
    if (is.factor(x)) {
      levels(droplevels(x))
    } else {
      lvls <- sort(unique(x[!is.na(x)]))
      if (any(is.na(x)) && !"NA" %in% lvls) {
        lvls <- c(lvls, "NA")
      }
      lvls
    }
  })
}

make_test_filtered_metadata_levels <- function(metadata, all_levels) {
  metadata <- as.data.frame(metadata)

  filtered_levels <- lapply(names(all_levels), function(col) {
    if (!col %in% colnames(metadata)) {
      return(character())
    }

    values <- as.character(metadata[[col]])
    values[is.na(values)] <- "NA"
    intersect(all_levels[[col]], unique(values))
  })
  stats::setNames(filtered_levels, names(all_levels))
}

make_test_metadata_numeric <- function(metadata) {
  metadata <- as.data.frame(metadata)
  numeric_columns <- vapply(metadata, is.numeric, logical(1))

  lapply(metadata[numeric_columns], function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(data.frame(mids = numeric(), counts = integer()))
    }

    hh <- hist(x, breaks = 20, plot = FALSE)
    data.frame(mids = hh$mids, counts = hh$counts)
  })
}

make_test_grouping_vars <- function(
  metadata_levels,
  preferred = c(
    "cluster", "condition", "orig.ident", "seurat_clusters", "idents"
  )
) {
  columns <- unique(c(
    intersect(preferred, names(metadata_levels)),
    setdiff(names(metadata_levels), preferred)
  ))
  grouping_vars <- columns
  names(grouping_vars) <- paste0(
    columns,
    " (n = ",
    lengths(metadata_levels[columns]),
    ")"
  )

  grouping_vars
}

make_test_cluster_colors <- function(metadata_levels) {
  palette <- c(
    "#4477aa", "#cc6677", "#228833", "#ee6677",
    "#aa3377", "#66ccee", "#ccbb44", "#bbbbbb"
  )

  lapply(metadata_levels, function(lvls) {
    cols <- rep_len(palette, length(lvls))
    names(cols) <- lvls
    cols
  })
}

make_test_app_state <- function(
  rds = make_test_seurat(),
  filtered_cells = colnames(rds),
  qc = NULL
) {
  metadata <- rds@meta.data
  if (!"idents" %in% colnames(metadata) &&
      !is.null(SeuratObject::Idents(rds))) {
    metadata$idents <- SeuratObject::Idents(rds)
  }

  filtered_metadata <- metadata[
    intersect(filtered_cells, rownames(metadata)),
    ,
    drop = FALSE
  ]

  all_levels <- make_test_metadata_levels(metadata)
  filtered_levels <- make_test_filtered_metadata_levels(
    filtered_metadata,
    all_levels
  )

  list(
    rds = rds,
    obj_type = "seurat",
    metadata = metadata,
    metadata_levels = list(
      all = all_levels,
      filtered = filtered_levels
    ),
    metadata_numeric = list(
      all = make_test_metadata_numeric(metadata),
      filtered = make_test_metadata_numeric(filtered_metadata)
    ),
    cluster_colors = make_test_cluster_colors(all_levels),
    grouping_vars = make_test_grouping_vars(all_levels),
    qc = qc,
    allmarkers = NULL,
    consmarkers = NULL,
    demarkers = NULL,
    spatial_coords = NULL,
    imagerow_max = NULL,
    imagerow_min = NULL
  )
}

make_test_plot_app_object <- function(app_state = make_test_app_state()) {
  list(
    rds = app_state$rds,
    obj_type = app_state$obj_type,
    metadata = app_state$metadata,
    metadata_levels = app_state$metadata_levels$filtered,
    cluster_colors = app_state$cluster_colors,
    grouping_vars = app_state$grouping_vars,
    spatial_coords = app_state$spatial_coords,
    imagerow_max = app_state$imagerow_max,
    imagerow_min = app_state$imagerow_min
  )
}

make_test_args <- function(
  assay = "RNA",
  slot = "data",
  grp_by = "cluster",
  dimred = "umap",
  project = file.path(tempdir(), "project1"),
  analysis = file.path(tempdir(), "project1", "analysis1", "object.rds")
) {
  list(
    assay = assay,
    slot = slot,
    grp_by = grp_by,
    dimred = dimred,
    project = project,
    analysis = analysis
  )
}

make_test_config <- function() {
  get_config()
}

make_test_module_reactives <- function(
  app_state = make_test_app_state(),
  filtered_cells = colnames(app_state$rds),
  genes = rownames(app_state$rds)[seq_len(2)],
  args = make_test_args(),
  config = make_test_config()
) {
  reload_global <- shiny::reactiveVal(0)
  refresh <- shiny::reactiveVal(0)

  list(
    app_state = app_state,
    app_object = shiny::reactive({
      make_test_plot_app_object(app_state)
    }),
    filtered = shiny::reactive({
      filtered_cells
    }),
    genes_to_plot = shiny::reactive({
      genes
    }),
    gene_choices = shiny::reactive({
      rownames(app_state$rds)
    }),
    args = shiny::reactive({
      args
    }),
    config = shiny::reactive({
      config
    }),
    reload_global = reload_global,
    refresh = refresh
  )
}
