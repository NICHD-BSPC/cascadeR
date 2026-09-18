#library(cascadeR)

# ---- get_limits ----

test_that("get_limits returns a length-2 vector", {
  result <- get_limits(c(1, 2, 3))
  expect_length(result, 2)
})

test_that("get_limits lower bound <= min and upper bound >= max", {
  x <- c(1.234, 3.456, 5.678)
  result <- get_limits(x)
  expect_lte(result[1], min(x))
  expect_gte(result[2], max(x))
})

test_that("get_limits rounds to sig_digits decimal places", {
  x <- c(1.23456, 5.6789)
  result <- get_limits(x, sig_digits = 2)
  fctr <- 10^2
  expect_equal(result[1], floor(min(x) * fctr) / fctr)
  expect_equal(result[2], ceiling(max(x) * fctr) / fctr)
})

test_that("get_limits removes NA values by default", {
  x <- c(1, NA, 5)
  result <- get_limits(x)
  expect_equal(result[1], 1)
  expect_equal(result[2], 5)
})

test_that("get_limits propagates NA when na.rm = FALSE", {
  x <- c(1, NA, 5)
  result <- get_limits(x, na.rm = FALSE)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
})

# ---- sanitize_colnames ----

test_that("sanitize_colnames replaces hyphens with underscores", {
  expect_equal(sanitize_colnames("gene-A"), "gene_A")
})

test_that("sanitize_colnames replaces dots with underscores", {
  expect_equal(sanitize_colnames("gene.A"), "gene_A")
})

test_that("sanitize_colnames replaces colons with underscores", {
  expect_equal(sanitize_colnames("gene:A"), "gene_A")
})

test_that("sanitize_colnames replaces forward slashes with underscores", {
  expect_equal(sanitize_colnames("gene/A"), "gene_A")
})

test_that("sanitize_colnames replaces asterisks with underscores", {
  expect_equal(sanitize_colnames("gene*A"), "gene_A")
})

test_that("sanitize_colnames handles a vector of names", {
  result <- sanitize_colnames(c("gene-A", "cell.type", "cluster:1"))
  expect_equal(result, c("gene_A", "cell_type", "cluster_1"))
})

test_that("sanitize_colnames does not alter clean names", {
  result <- sanitize_colnames(c("geneA", "cell_type", "UMAP1"))
  expect_equal(result, c("geneA", "cell_type", "UMAP1"))
})

# ---- get_project_name_from_path ----

test_that("get_project_name_from_path returns correct name at depth 2", {
  result <- get_project_name_from_path(
    "/path/to/project/test/file.Rds", depth = 2, fsep = "/"
  )
  expect_equal(result, "project/test")
})

test_that("get_project_name_from_path respects end_offset", {
  result <- get_project_name_from_path(
    "/path/to/project/test/file.Rds", depth = 2, end_offset = 1, fsep = "/"
  )
  expect_equal(result, "project")
})

test_that("get_project_name_from_path increases depth when staging_dir present", {
  result <- get_project_name_from_path(
    "/path/project/staged/test/file.Rds",
    depth = 2, staging_dir = "staged", fsep = "/"
  )
  # depth bumped to 3: tokens [staged, project, test]
  expect_equal(result, "project/staged/test")
})

test_that("get_project_name_from_path returns single folder at depth 1", {
  result <- get_project_name_from_path(
    "/path/to/project/file.Rds", depth = 1, fsep = "/"
  )
  expect_equal(result, "project")
})

# ---- get_binned_exp ----

test_that("get_binned_exp returns a data.frame with same dimensions", {
  df <- data.frame(geneA = c(0, 2, 4), geneB = c(0, 1, 2))
  result <- get_binned_exp(df, bins = 3)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
})

test_that("get_binned_exp range mode bins to [0, bins-1]", {
  df <- data.frame(geneA = c(0, 2, 4))
  result <- get_binned_exp(df, bins = 3, mode = "range")
  vals <- as.numeric(result[, 1])
  expect_equal(vals, c(0, 1, 2))
})

test_that("get_binned_exp range mode handles constant columns", {
  df <- data.frame(geneA = c(0, 0, 0), geneB = c(5, 5, 5))
  result <- get_binned_exp(df, bins = 3, mode = "range")
  expect_equal(as.numeric(result[, "geneA"]), c(0, 0, 0))
  expect_equal(as.numeric(result[, "geneB"]), c(2, 2, 2))
})

test_that("get_binned_exp quantile mode returns values in [1, bins]", {
  set.seed(42)
  df <- data.frame(geneA = rnorm(50))
  result <- get_binned_exp(df, bins = 5, mode = "quantile")
  vals <- as.numeric(result[, 1])
  expect_true(all(!is.na(vals)))
  expect_true(all(vals >= 1 & vals <= 5))
})

test_that("get_binned_exp quantile mode handles tied values", {
  df <- data.frame(geneA = c(0, 0, 0, 1, 2))
  result <- get_binned_exp(df, bins = 5, mode = "quantile")
  vals <- as.numeric(result[, 1])
  expect_true(all(!is.na(vals)))
  expect_true(all(vals >= 1 & vals <= 5))
})

test_that("get_binned_exp errors on invalid mode", {
  df <- data.frame(geneA = c(1, 2, 3))
  expect_error(
    get_binned_exp(df, mode = "invalid"),
    '"mode" can only be "range" or "quantile"'
  )
})

# ---- get_coexp_tbl ----

test_that("get_coexp_tbl returns a data.frame with 4 rows", {
  df <- data.frame(geneA = c(0, 0, 5, 5, 10),
                   geneB = c(0, 5, 0, 5, 10))
  result <- get_coexp_tbl(df, genes = c("geneA", "geneB"), n = 10)
  expect_true(inherits(result, "data.frame"))
  expect_equal(nrow(result), 4)
})

test_that("get_coexp_tbl cell counts sum to total number of cells", {
  df <- data.frame(geneA = c(0, 0, 5, 5, 10),
                   geneB = c(0, 5, 0, 5, 10))
  result <- get_coexp_tbl(df, genes = c("geneA", "geneB"), n = 10)
  expect_equal(sum(result[["# cells"]]), nrow(df))
})

test_that("get_coexp_tbl percentages sum to 100", {
  df <- data.frame(geneA = c(0, 0, 5, 5, 10),
                   geneB = c(0, 5, 0, 5, 10))
  result <- get_coexp_tbl(df, genes = c("geneA", "geneB"), n = 10)
  expect_equal(sum(result[["%" ]]), 100)
})

test_that("get_coexp_tbl assigns expected categories", {
  df <- data.frame(geneA = c(0, 10, 0, 10),
                   geneB = c(0, 0, 10, 10))
  result <- get_coexp_tbl(df, genes = c("geneA", "geneB"), n = 10)
  expect_equal(result[["labels"]], c("neither", "geneA only", "geneB only", "both"))
  expect_equal(result[["# cells"]], c(1, 1, 1, 1))
  expect_equal(result[["%"]], c(25, 25, 25, 25))
})

test_that("get_coexp_tbl respects custom thresholds", {
  df <- data.frame(geneA = c(0, 10, 0, 10),
                   geneB = c(0, 0, 10, 10))
  low_threshold <- get_coexp_tbl(
    df, genes = c("geneA", "geneB"), n = 10,
    threshold1 = 0, threshold2 = 0
  )
  high_threshold <- get_coexp_tbl(
    df, genes = c("geneA", "geneB"), n = 10,
    threshold1 = 1, threshold2 = 1
  )
  expect_equal(low_threshold[["# cells"]], c(0, 0, 0, 4))
  expect_equal(high_threshold[["# cells"]], c(4, 0, 0, 0))
})

test_that("get_coexp_tbl errors unless exactly 2 genes are provided", {
  df <- data.frame(geneA = c(1, 2), geneB = c(1, 2), geneC = c(1, 2))
  expect_error(
    get_coexp_tbl(df, genes = "geneA"),
    "Need exactly 2 genes to get coexpression"
  )
  expect_error(
    get_coexp_tbl(df, genes = c("geneA", "geneB", "geneC")),
    "Need exactly 2 genes to get coexpression"
  )
})

test_that("get_coexp_tbl errors when a gene column is missing", {
  df <- data.frame(geneA = c(1, 2), geneB = c(1, 2))
  expect_error(get_coexp_tbl(df, genes = c("geneA", "missing")))
})

# ---- get_coexplt_colors ----

test_that("get_coexplt_colors returns 4 colours for red-blue", {
  result <- get_coexplt_colors("red-blue")
  expect_length(result, 4)
  expect_equal(result[1], "#d3d3d3")  # neutral
  expect_equal(result[2], "#ff0000")  # gene A (red)
  expect_equal(result[3], "#0000ff")  # gene B (blue)
  expect_equal(result[4], "#ff00ff")  # both (purple)
})

test_that("get_coexplt_colors returns 4 colours for red-green", {
  result <- get_coexplt_colors("red-green")
  expect_length(result, 4)
  expect_equal(result[1], "#d3d3d3")  # neutral
  expect_equal(result[2], "#ff0000")  # gene A (red)
  expect_equal(result[3], "#006400")  # gene B (dark green)
  expect_equal(result[4], "#ff6400")  # both (orange)
})
