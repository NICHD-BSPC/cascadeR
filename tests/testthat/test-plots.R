make_plot_df <- function(n=100, reduction=FALSE) {
  set.seed(42)
  df <- data.frame(
    cluster = factor(
                sample(c("A", "B"), n, replace=TRUE),
                levels = c("A", "B", "unused")),
    condition = factor(
                  sample(c("stim", "ctrl"), n, replace=TRUE),
                  levels = c("stim", "ctrl", "unused")),
    gene1 = sample(1:30, n, replace=TRUE),
    gene2 = sample(1:30, n, replace=TRUE)
  )
  if(reduction){
    df <- cbind(df, data.frame(
      UMAP_1 = runif(n, min=-5, max=5),
      UMAP_2 = runif(n, min=-5, max=5)
    ))
  }
  df
}

make_blend_plot_df <- function() {
  df <- make_plot_df(n=8, reduction=TRUE)
  df$condition <- factor(
    rep(c("stim", "ctrl"), each=4),
    levels = c("stim", "ctrl", "unused")
  )
  df$gene1 <- rep(c(0, 10, 0, 10), 2)
  df$gene2 <- rep(c(0, 0, 10, 10), 2)
  df
}

plot_colors <- c(A = "#4477aa", B = "#cc6677")
feature_colors <- "Viridis"
blend_colors <- c("#ff0000", "#0000ff", "#ff00ff")
blend_groups <- c("neither", "gene1 only", "gene2 only", "both")

plotly_annotation_text <- function(p) {
  built <- suppressWarnings(plotly::plotly_build(p))
  vapply(built$x$layout$annotations, function(x) x$text, character(1))
}

plotly_trace_names <- function(p) {
  built <- suppressWarnings(plotly::plotly_build(p))
  vapply(
    built$x$data,
    function(x) if (is.null(x$name)) "" else x$name,
    character(1)
  )
}

plotly_axis_title <- function(axis) {
  if (is.list(axis$title)) {
    return(axis$title$text)
  }
  axis$title
}

expected_dotplot_summary <- function(df, genes, split=NULL) {
  clusters <- levels(droplevels(df$cluster))

  if(is.null(split)) {
    groups <- clusters
    group_df <- data.frame(cluster=groups)
    grouping_column <- as.character(df$cluster)
  } else {
    grouping_column <- paste0(df$cluster, "|", df[[split]])
    groups <- unique(grouping_column)
    group_df <- as.data.frame(do.call(rbind, strsplit(groups, "\\|")))
    colnames(group_df) <- c("cluster", split)
  }

  out <- lapply(genes, function(gene) {
    stats <- lapply(groups, function(group) {
      x <- df[[gene]][grouping_column == group]
      c(avg.exp=log1p(mean(x)), pct.exp=sum(x > 0) * 100 / length(x))
    })
    data.frame(
      gene=gene,
      group_df,
      do.call(rbind, stats),
      row.names=NULL
    )
  })

  do.call(rbind, out)
}

# ---- violin2 ----

test_that("violin2 returns a ggplot and drops unused x levels", {
  df <- make_plot_df()
  df2 <- reshape2::melt(df, id.vars=c("cluster", "condition"))

  p <- suppressWarnings(
    violin2(df2, xcol = "cluster", ycol = "value")
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(levels(p$data$cluster), c("A", "B"))
  expect_error(suppressWarnings(ggplot2::ggplot_build(p)), NA)
})

test_that("violin2 supports color splitting and point overlays", {
  df <- make_plot_df()
  df2 <- reshape2::melt(df, id.vars=c("cluster", "condition"))

  p <- suppressWarnings(
    violin2(
      df2,
      xcol = "cluster",
      ycol = "value",
      color = "condition",
      colors = c(ctrl = "#4477aa", stim = "#cc6677"),
      draw_points = TRUE
    )
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(levels(p$data$condition), c("stim", "ctrl"))
  expect_equal(length(p$layers), 2)
  expect_error(suppressWarnings(ggplot2::ggplot_build(p)), NA)
})

# ---- dotplot ----

test_that("dotplot returns a ggplot with expected gene and group summaries", {
  df <- make_plot_df()

  p <- suppressWarnings(
    dotplot(df, xcol = "cluster", ycol = c("gene1", "gene2"), scale = FALSE)
  )
  expected <- expected_dotplot_summary(df, genes=c("gene1", "gene2"))

  expect_true(inherits(p, "ggplot"))
  expect_equal(nrow(p$data), nrow(expected))
  expect_equal(as.character(p$data$gene), expected$gene)
  expect_equal(as.character(p$data$cluster), expected$cluster)
  expect_equal(levels(p$data$cluster), c("A", "B"))
  expect_equal(p$data$pct.exp, expected$pct.exp)
  expect_equal(as.numeric(p$data$avg.exp), expected$avg.exp)
  expect_error(suppressWarnings(ggplot2::ggplot_build(p)), NA)
})

test_that("dotplot supports split facets", {
  df <- make_plot_df()

  p <- suppressWarnings(
    dotplot(df, xcol = "cluster", ycol = "gene1", split = "condition", scale = FALSE)
  )
  expected <- expected_dotplot_summary(df, genes="gene1", split="condition")

  expect_true(inherits(p, "ggplot"))
  expect_true("condition" %in% colnames(p$data))
  expect_true(any(grepl("FacetWrap", class(p$facet))))
  expect_equal(as.character(p$data$cluster), expected$cluster)
  expect_equal(as.character(p$data$condition), expected$condition)
  expect_equal(p$data$pct.exp, expected$pct.exp)
  expect_equal(as.numeric(p$data$avg.exp), expected$avg.exp)
  expect_error(suppressWarnings(ggplot2::ggplot_build(p)), NA)
})

# ---- umap_ly ----

test_that("umap_ly returns a plotly object with axis annotations", {
  df <- make_plot_df(reduction=TRUE)

  p <- umap_ly(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    color = "cluster",
    colors = plot_colors,
    label_cols = c("cluster", "gene1"),
    showlegend = FALSE,
    showticklabels = FALSE,
    highlight_click = FALSE
  )
  built <- suppressWarnings(plotly::plotly_build(p))

  expect_true(inherits(p, "plotly"))
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_equal(plotly_annotation_text(p), c("UMAP_1", "UMAP_2"))
  expect_false(isTRUE(built$x$layout$xaxis$showticklabels))
  expect_false(isTRUE(built$x$layout$yaxis$showticklabels))
})

test_that("umap_ly sanitizes plotted column names", {
  df <- make_plot_df(reduction=TRUE)
  names(df)[names(df) == "UMAP_1"] <- "UMAP-1"
  names(df)[names(df) == "UMAP_2"] <- "UMAP.2"

  p <- umap_ly(
    df,
    xcol = "UMAP-1",
    ycol = "UMAP.2",
    color = "cluster",
    colors = plot_colors,
    label_cols = c("UMAP-1", "UMAP.2"),
    showlegend = FALSE,
    highlight_click = FALSE
  )

  expect_true(inherits(p, "plotly"))
  expect_equal(plotly_annotation_text(p), c("UMAP_1", "UMAP_2"))
})

test_that("umap_ly supports split subplots", {
  df <- make_plot_df(reduction=TRUE)

  p <- umap_ly(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    color = "cluster",
    colors = plot_colors,
    split = "condition",
    showlegend = FALSE,
    highlight_click = FALSE
  )
  annotations <- plotly_annotation_text(p)
  built <- suppressWarnings(plotly::plotly_build(p))

  expect_true(inherits(p, "plotly"))
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_true(all(c("UMAP_1", "UMAP_2", "<b> stim <b>", "<b> ctrl <b>") %in% annotations))
})

# ---- feature_ly ----

test_that("feature_ly returns a plotly object with feature annotation", {
  df <- make_plot_df(reduction=TRUE)

  p <- feature_ly(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    color = "gene1",
    colors = feature_colors,
    crange = c(0, 30),
    label_cols = c("cluster", "gene1"),
    showscale = FALSE,
    showticklabels = FALSE,
    reorder = FALSE
  )
  built <- suppressWarnings(plotly::plotly_build(p))

  expect_true(inherits(p, "plotly"))
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_equal(plotly_annotation_text(p), "<b> gene1 <b>")
  expect_false(isTRUE(built$x$layout$xaxis$showticklabels))
  expect_false(isTRUE(built$x$layout$yaxis$showticklabels))
})

test_that("feature_ly sanitizes numeric-leading feature names", {
  df <- make_plot_df(reduction=TRUE)
  df[["1-gene"]] <- df$gene1

  p <- feature_ly(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    color = "1-gene",
    colors = feature_colors,
    showscale = FALSE,
    reorder = FALSE
  )

  expect_true(inherits(p, "plotly"))
  expect_equal(plotly_annotation_text(p), "<b> X1_gene <b>")
})

test_that("feature_ly supports split subplots", {
  df <- make_plot_df(reduction=TRUE)

  p <- feature_ly(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    color = "gene1",
    colors = feature_colors,
    split = "condition",
    row_view = "single",
    showscale = FALSE,
    reorder = FALSE
  )
  annotations <- plotly_annotation_text(p)
  built <- suppressWarnings(plotly::plotly_build(p))

  expect_true(inherits(p, "plotly"))
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_true(all(c("<b> gene1 <b>", "<b> stim <b>", "<b> ctrl <b>") %in% annotations))
})

# ---- feature_blend ----

test_that("feature_blend returns plotly output and assigns coexpression groups", {
  df <- make_blend_plot_df()

  result <- feature_blend(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    blend_cols = c("gene1", "gene2"),
    colors = blend_colors,
    n = 10,
    showlegend = FALSE,
    showticklabels = FALSE,
    type = "scatter"
  )
  built <- suppressWarnings(plotly::plotly_build(result$plot))

  expect_equal(names(result), c("plot", "data"))
  expect_true(inherits(result$plot, "plotly"))
  expect_equal(levels(result$data$color), blend_groups)
  expect_equal(as.character(result$data$color[1:4]), blend_groups)
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_equal(plotly_annotation_text(result$plot), c("UMAP_1", "UMAP_2"))
  expect_false(isTRUE(built$x$layout$xaxis$showticklabels))
  expect_false(isTRUE(built$x$layout$yaxis$showticklabels))
  expect_true(all(blend_groups %in% plotly_trace_names(result$plot)))
  expect_true(all(vapply(
    built$x$data,
    function(x) isTRUE(x$mode == "markers"),
    logical(1)
  )))
})

test_that("feature_blend supports split subplots", {
  df <- make_blend_plot_df()

  result <- feature_blend(
    df,
    xcol = "UMAP_1",
    ycol = "UMAP_2",
    blend_cols = c("gene1", "gene2"),
    colors = blend_colors,
    split = "condition",
    n = 10,
    showlegend = FALSE,
    type = "scatter"
  )
  annotations <- plotly_annotation_text(result$plot)
  built <- suppressWarnings(plotly::plotly_build(result$plot))

  expect_true(inherits(result$plot, "plotly"))
  expect_equal(levels(result$data$color), blend_groups)
  expect_equal(built$x$layout$dragmode, "lasso")
  expect_true(all(c("UMAP_1", "UMAP_2", "<b> stim <b>", "<b> ctrl <b>") %in% annotations))
  expect_true(all(blend_groups %in% plotly_trace_names(result$plot)))
  expect_true(length(built$x$data) >= length(blend_groups))
})

# ---- get_coexp_legend ----

test_that("get_coexp_legend returns plotly output with threshold metadata", {
  p <- get_coexp_legend(
    colors = blend_colors,
    dimnames = c("gene1", "gene2"),
    xline = 0.25,
    yline = 0.75,
    n = 20,
    neutral_color = "#eeeeee"
  )
  built <- suppressWarnings(plotly::plotly_build(p))
  shapes <- built$x$layout$shapes

  expect_true(inherits(p, "plotly"))
  expect_equal(built$x$data[[1]]$type, "scatter")
  expect_equal(built$x$data[[1]]$mode, "markers")
  expect_equal(plotly_axis_title(built$x$layout$xaxis), "gene1")
  expect_equal(plotly_axis_title(built$x$layout$yaxis), "gene2")
  expect_equal(length(built$x$layout$annotations), 1)
  expect_equal(built$x$layout$annotations[[1]]$text, "<b>threshold</b>")
  expect_equal(built$x$layout$annotations[[1]]$x, 5)
  expect_equal(length(shapes), 6)
  expect_equal(
    vapply(shapes, function(x) x$type, character(1)),
    c("rect", "rect", "rect", "rect", "line", "line")
  )
  expect_equal(
    vapply(shapes[1:4], function(x) x$fillcolor, character(1)),
    c("#eeeeee", blend_colors[3], blend_colors[1], blend_colors[2])
  )
  expect_equal(shapes[[5]]$x0, 5)
  expect_equal(shapes[[5]]$x1, 5)
  expect_equal(shapes[[5]]$line$dash, "dot")
  expect_equal(shapes[[6]]$y0, 15)
  expect_equal(shapes[[6]]$y1, 15)
  expect_equal(shapes[[6]]$line$dash, "dot")
})
