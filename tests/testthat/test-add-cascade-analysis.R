capture_add_cascade_messages <- function(expr) {
  messages <- character()
  withCallingHandlers(
    force(expr),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  messages
}

make_add_cascade_fixture <- function() {
  data_dir <- tempfile("cascade-data-")
  dir.create(data_dir)

  obj_path <- tempfile("object-", fileext = ".rds")
  file.create(obj_path)

  list(data_dir = data_dir, obj_path = obj_path)
}

test_that("add_cascade_analysis errors for unsupported object extensions", {
  data_dir <- tempfile("cascade-data-")
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  expect_error(
    add_cascade_analysis(
      obj_path = file.path(data_dir, "object.txt"),
      data_dir = data_dir,
      project = "project1",
      analysis = "analysis1"
    ),
    "does not appear to be RDS/h5ad file"
  )
})

test_that("add_cascade_analysis errors when data directory is missing", {
  expect_error(
    add_cascade_analysis(
      obj_path = "object.rds",
      data_dir = tempfile("missing-data-"),
      project = "project1",
      analysis = "analysis1"
    ),
    "Data directory"
  )
})

test_that("add_cascade_analysis errors when analysis directory exists without overwrite", {
  fixture <- make_add_cascade_fixture()
  on.exit(unlink(c(fixture$data_dir, fixture$obj_path), recursive = TRUE), add = TRUE)

  dir.create(file.path(fixture$data_dir, "project1", "analysis1"), recursive = TRUE)

  expect_error(
    suppressMessages(add_cascade_analysis(
      obj_path = fixture$obj_path,
      data_dir = fixture$data_dir,
      project = "project1",
      analysis = "analysis1"
    )),
    "already exists"
  )
})

test_that("add_cascade_analysis errors when optional marker files are missing", {
  fixture <- make_add_cascade_fixture()
  on.exit(unlink(c(fixture$data_dir, fixture$obj_path), recursive = TRUE), add = TRUE)

  missing_marker <- file.path(fixture$data_dir, "missing.tsv")

  expect_error(
    suppressMessages(add_cascade_analysis(
      fixture$obj_path, fixture$data_dir, "project1", "analysis1",
      cluster_markers = missing_marker
    )),
    "Cluster marker file"
  )
  expect_error(
    suppressMessages(add_cascade_analysis(
      fixture$obj_path, fixture$data_dir, "project1", "analysis1",
      de_markers = missing_marker
    )),
    "DE marker file"
  )
  expect_error(
    suppressMessages(add_cascade_analysis(
      fixture$obj_path, fixture$data_dir, "project1", "analysis1",
      conserved_markers = missing_marker
    )),
    "Conserved marker file"
  )
})

test_that("add_cascade_analysis dry run reports setup commands without creating directories", {
  fixture <- make_add_cascade_fixture()
  on.exit(unlink(c(fixture$data_dir, fixture$obj_path), recursive = TRUE), add = TRUE)

  cluster_markers <- tempfile("allmarkers-", fileext = ".tsv")
  file.create(cluster_markers)
  on.exit(unlink(cluster_markers), add = TRUE)

  messages <- capture_add_cascade_messages(
    add_cascade_analysis(
      obj_path = fixture$obj_path,
      data_dir = fixture$data_dir,
      project = "project1",
      analysis = "analysis1",
      cluster_markers = cluster_markers,
      execute = FALSE
    )
  )

  expect_false(dir.exists(file.path(fixture$data_dir, "project1")))
  expect_true(any(grepl("mkdir -p", messages, fixed = TRUE)))
  expect_true(any(grepl("allmarkers.tsv", messages, fixed = TRUE)))
  expect_true(any(grepl("execute=TRUE", messages, fixed = TRUE)))
})

test_that("add_cascade_analysis overwrite dry run preserves existing files", {
  fixture <- make_add_cascade_fixture()
  on.exit(unlink(c(fixture$data_dir, fixture$obj_path), recursive = TRUE), add = TRUE)

  analysis_path <- file.path(fixture$data_dir, "project1", "analysis1")
  dir.create(analysis_path, recursive = TRUE)
  sentinel <- file.path(analysis_path, "keep.txt")
  file.create(sentinel)

  messages <- capture_add_cascade_messages(
    add_cascade_analysis(
      obj_path = fixture$obj_path,
      data_dir = fixture$data_dir,
      project = "project1",
      analysis = "analysis1",
      overwrite = TRUE,
      execute = FALSE
    )
  )

  expect_true(file.exists(sentinel))
  expect_true(any(grepl("rm -r", messages, fixed = TRUE)))
})

test_that("add_cascade_analysis execute creates analysis directory and object symlink", {
  skip_on_os("windows")

  fixture <- make_add_cascade_fixture()
  on.exit(unlink(c(fixture$data_dir, fixture$obj_path), recursive = TRUE), add = TRUE)

  old_wd <- getwd()
  capture_add_cascade_messages(
    add_cascade_analysis(
      obj_path = fixture$obj_path,
      data_dir = fixture$data_dir,
      project = "project1",
      analysis = "analysis1",
      execute = TRUE
    )
  )

  analysis_path <- file.path(fixture$data_dir, "project1", "analysis1")
  linked_obj <- file.path(analysis_path, basename(fixture$obj_path))

  expect_equal(getwd(), old_wd)
  expect_true(dir.exists(analysis_path))
  expect_true(file.exists(linked_obj))
})
