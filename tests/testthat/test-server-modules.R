# ---- shared server fixtures ----

test_that("server fixtures build a tiny Seurat app state", {
  app_state <- make_test_app_state()

  expect_true(inherits(app_state$rds, "Seurat"))
  expect_equal(dim(app_state$rds), c(4, 8))
  expect_equal(rownames(app_state$rds), paste0("Gene", LETTERS[1:4]))
  expect_equal(rownames(app_state$metadata), paste0("cell", seq_len(8)))
  expect_equal(app_state$obj_type, "seurat")
  expect_equal(app_state$metadata_levels$all$cluster, c("A", "B"))
  expect_equal(app_state$metadata_levels$filtered$condition, c("ctrl", "stim"))
  expect_equal(unname(app_state$grouping_vars["cluster (n = 2)"]), "cluster")
  expect_true("quality_score" %in% names(app_state$metadata_numeric$all))
  expect_true("idents" %in% colnames(app_state$metadata))
})

test_that("server fixture reactives expose common module inputs", {
  fixtures <- make_test_module_reactives()

  expect_equal(
    shiny::isolate(fixtures$filtered()),
    colnames(fixtures$app_state$rds)
  )
  expect_equal(shiny::isolate(fixtures$genes_to_plot()), c("GeneA", "GeneB"))
  expect_equal(
    shiny::isolate(fixtures$gene_choices()),
    paste0("Gene", LETTERS[1:4])
  )
  expect_equal(shiny::isolate(fixtures$args())$assay, "RNA")
  expect_equal(shiny::isolate(fixtures$config()), get_config())
})

# ---- controlServer ----

test_that("controlServer returns selected values from dataframe controls", {
  metadata <- make_test_metadata()

  shiny::testServer(
    controlServer,
    args = list(
      full_obj = shiny::reactive({ metadata }),
      column = "cluster",
      global = shiny::reactive({ NULL })
    ),
    {
      selected <- session$getReturned()

      session$setInputs(filter = c("A", "B"))
      expect_equal(selected(), c("A", "B"))

      session$setInputs(filter = character())
      expect_equal(selected(), character())
    }
  )
})

test_that("controlServer supports reactive column choices", {
  metadata <- make_test_metadata()
  selected_column <- shiny::reactiveVal("cluster")

  shiny::testServer(
    controlServer,
    args = list(
      full_obj = shiny::reactive({ metadata }),
      column = shiny::reactive({ selected_column() }),
      global = shiny::reactive({ NULL })
    ),
    {
      selected <- session$getReturned()

      expect_equal(col(), "cluster")

      selected_column("condition")
      session$flushReact()
      expect_equal(col(), "condition")

      session$setInputs(filter = "stim")
      expect_equal(selected(), "stim")
    }
  )
})

test_that("controlServer returns selected values from list controls", {
  levels <- list(
    cluster = c("A", "B"),
    condition = c("ctrl", "stim")
  )

  shiny::testServer(
    controlServer,
    args = list(
      full_obj = shiny::reactive({ levels }),
      column = "condition",
      global = shiny::reactive({ NULL })
    ),
    {
      selected <- session$getReturned()

      session$setInputs(filter = c("ctrl", "stim"))
      expect_equal(selected(), c("ctrl", "stim"))
    }
  )
})

# ---- plot module helpers ----

plot_module_server_args <- function(fixtures) {
  list(
    app_object = fixtures$app_object,
    filtered = fixtures$filtered,
    genes_to_plot = fixtures$genes_to_plot,
    args = fixtures$args,
    gene_choices = fixtures$gene_choices,
    reload_global = fixtures$reload_global,
    refresh = fixtures$refresh,
    config = fixtures$config
  )
}

selectable_plot_module_server_args <- function(fixtures) {
  c(
    plot_module_server_args(fixtures),
    list(
      all_selected = shiny::reactive({ list() }),
      show_selection = shiny::reactive({ NULL }),
      reset_selection = shiny::reactive({ NULL })
    )
  )
}

# ---- summaryServer ----

test_that("summaryServer exposes app state through its internal reactive", {
  app_state <- make_test_app_state()

  shiny::testServer(
    summaryServer,
    args = list(
      obj = app_state,
      args = shiny::reactive({
        make_test_args(
          project = file.path(tempdir(), "project1"),
          analysis = file.path(tempdir(), "project1", "analysis1", "object.rds")
        )
      })
    ),
    {
      internal <- app_object()

      expect_identical(internal$rds, app_state$rds)
      expect_equal(internal$obj_type, "seurat")
      expect_equal(internal$metadata, app_state$metadata)
      expect_null(internal$qc)
    }
  )
})

test_that("summaryServer preserves QC metadata in its internal reactive", {
  raw_metadata <- make_test_metadata(paste0("raw_cell", seq_len(10)))
  qc <- list(metadata = list(raw = raw_metadata))
  app_state <- make_test_app_state(qc = qc)

  shiny::testServer(
    summaryServer,
    args = list(
      obj = app_state,
      args = shiny::reactive({ make_test_args() })
    ),
    {
      internal <- app_object()

      expect_identical(internal$qc, qc)
      expect_equal(nrow(internal$metadata), 8)
      expect_equal(sum(vapply(internal$qc$metadata, nrow, integer(1))), 10)
    }
  )
})

test_that("summaryServer validates that an object is loaded", {
  app_state <- make_test_app_state()
  app_state$rds <- NULL

  shiny::testServer(
    summaryServer,
    args = list(
      obj = app_state,
      args = shiny::reactive({ make_test_args() })
    ),
    {
      expect_error(output$summary_tbl, "Waiting for selection", fixed = TRUE)
    }
  )
})

# ---- featurePlotServer ----

test_that("featurePlotServer builds a plotly plot from fixture data", {
  fixtures <- make_test_module_reactives(genes = "GeneA")

  shiny::testServer(
    featurePlotServer,
    args = selectable_plot_module_server_args(fixtures),
    {
      session$setInputs(
        split_by = "none",
        marker_size = 2,
        marker_opacity = 0.8,
        free_axes = "no",
        colormap = "viridis",
        downsample = "no",
        scale = 1,
        plot_aspect = "wide",
        plt_genes = "GeneA",
        plt_do = 1
      )
      session$flushReact()

      p <- get_feature_plot()

      expect_true(inherits(p, "plotly"))
      expect_equal(plot_obj$df$color, "GeneA")
      expect_null(plot_obj$df$split)
      expect_equal(
        nrow(plot_obj$df$data),
        length(shiny::isolate(fixtures$filtered()))
      )
    }
  )
})

# ---- coexpressionPlotServer ----

test_that("coexpressionPlotServer builds a plotly plot from fixture data", {
  fixtures <- make_test_module_reactives()

  shiny::testServer(
    coexpressionPlotServer,
    args = selectable_plot_module_server_args(fixtures),
    {
      session$setInputs(
        split_by = "none",
        marker_size = 3,
        marker_opacity = 0.8,
        free_axes = "no",
        colormap = "red-blue",
        downsample = "no",
        scale = 1,
        plot_aspect = "wide",
        thres_1 = 25,
        thres_2 = 25,
        plt_genes = c("GeneA", "GeneB"),
        plt_do = 1
      )
      session$flushReact()

      p <- get_coexpression_plot()

      expect_true(inherits(p, "plotly"))
      expect_equal(plot_obj$df$blend_cols, c("GeneA", "GeneB"))
      expect_null(plot_obj$df$split)
      expect_equal(
        nrow(plot_obj$df$data),
        length(shiny::isolate(fixtures$filtered()))
      )
      expect_equal(names(plot_data$coexp_thres), c("GeneA", "GeneB"))
      expect_equal(nrow(plot_data$coexp_tbl$tbl), 4)
    }
  )
})

# ---- scatterPlotServer ----

test_that("scatterPlotServer builds a plotly plot from fixture data", {
  fixtures <- make_test_module_reactives()

  shiny::testServer(
    scatterPlotServer,
    args = selectable_plot_module_server_args(fixtures),
    {
      session$setInputs(
        grp_by = "cluster",
        split_by = "none",
        color_by = "metadata",
        marker_size = 3,
        marker_opacity = 0.8,
        scale = 1,
        plt_genes = c("GeneA", "GeneB"),
        `plt_grp_lvls-filter` = c("A", "B"),
        plt_do = 1
      )
      session$flushReact()

      expect_equal(global_args$grp_by, "cluster")
      expect_equal(plt_grp_lvls(), c("A", "B"))

      p <- get_scatter_plot()

      expect_true(inherits(p, "plotly"))
      expect_equal(active_plot_type(), "metadata")
      expect_equal(plot_obj$metadata$xcol, "GeneA")
      expect_equal(plot_obj$metadata$ycol, "GeneB")
      expect_null(plot_obj$metadata$split)
    }
  )
})

# ---- dotPlotServer ----

test_that("dotPlotServer builds a ggplot from fixture data", {
  fixtures <- make_test_module_reactives()

  shiny::testServer(
    dotPlotServer,
    args = plot_module_server_args(fixtures),
    {
      session$setInputs(
        grp_by = "cluster",
        split_by = "none",
        scale = "FALSE",
        colormap = "blues",
        plt_genes = c("GeneA", "GeneB"),
        `plt_grp_lvls-filter` = c("A", "B"),
        plt_do = 1
      )
      session$flushReact()

      expect_equal(global_args$grp_by, "cluster")
      expect_equal(plt_grp_lvls(), c("A", "B"))

      p <- get_dot_plot()

      expect_true(inherits(p, "ggplot"))
      expect_equal(levels(p$data$cluster), c("A", "B"))
      expect_equal(unique(as.character(p$data$gene)), c("GeneA", "GeneB"))
    }
  )
})

# ---- violinServer ----

test_that("violinServer builds a ggplot from fixture data", {
  fixtures <- make_test_module_reactives()

  shiny::testServer(
    violinServer,
    args = plot_module_server_args(fixtures),
    {
      session$setInputs(
        grp_by = "cluster",
        split_by = "none",
        plt_pts = "FALSE",
        plt_scales = "no",
        plt_genes = c("GeneA", "GeneB"),
        `plt_grp_lvls-filter` = c("A", "B"),
        plt_do = 1
      )
      session$flushReact()

      expect_equal(global_args$grp_by, "cluster")
      expect_equal(plt_grp_lvls(), c("A", "B"))

      p <- get_violin_plot()

      expect_true(inherits(p, "ggplot"))
      expect_equal(levels(p$data$cluster), c("A", "B"))
      expect_equal(
        unique(as.character(p$data$variable)),
        c("GeneA", "GeneB")
      )
    }
  )
})
