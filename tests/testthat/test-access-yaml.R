set_cascade_access_dir <- function() {
  old <- Sys.getenv("CASCADE_ACCESS_YAML", unset = NA_character_)
  access_dir <- tempfile("cascade-access-")
  dir.create(access_dir)
  Sys.setenv(CASCADE_ACCESS_YAML = access_dir)

  list(access_dir = access_dir, old = old)
}

restore_cascade_access_dir <- function(state) {
  if (is.na(state$old)) {
    Sys.unsetenv("CASCADE_ACCESS_YAML")
  } else {
    Sys.setenv(CASCADE_ACCESS_YAML = state$old)
  }
  unlink(state$access_dir, recursive = TRUE)
}

test_that("get_access_path uses CASCADE_ACCESS_YAML when set", {
  state <- set_cascade_access_dir()
  on.exit(restore_cascade_access_dir(state), add = TRUE)

  expect_equal(
    get_access_path(),
    file.path(state$access_dir, ".cascade-access.yaml")
  )
})

test_that("get_access_path errors when CASCADE_ACCESS_YAML points to missing directory", {
  old <- Sys.getenv("CASCADE_ACCESS_YAML", unset = NA_character_)
  missing_dir <- tempfile("missing-access-")
  Sys.setenv(CASCADE_ACCESS_YAML = missing_dir)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("CASCADE_ACCESS_YAML")
    } else {
      Sys.setenv(CASCADE_ACCESS_YAML = old)
    }
  }, add = TRUE)

  expect_error(
    get_access_path(),
    'Environment variable "CASCADE_ACCESS_YAML" exists'
  )
})

test_that("create_access_yaml writes access details that read_access_yaml can read", {
  state <- set_cascade_access_dir()
  on.exit(restore_cascade_access_dir(state), add = TRUE)

  create_access_yaml("alice", "lab1", "/data/lab1")

  expect_true(file.exists(get_access_path()))
  result <- read_access_yaml()
  expect_equal(unlist(result$user_group), c(alice = "lab1"))
  expect_equal(unlist(result$data_area), c(lab1 = "/data/lab1"))
})

test_that("save_access_yaml overwrites access details", {
  state <- set_cascade_access_dir()
  on.exit(restore_cascade_access_dir(state), add = TRUE)

  create_access_yaml("alice", "lab1", "/data/lab1")
  save_access_yaml(list(
    user_group = list(alice = "lab1", bob = "lab2"),
    data_area = list(lab1 = "/data/lab1", lab2 = "/data/lab2")
  ))

  result <- read_access_yaml()
  expect_equal(unlist(result$user_group), c(alice = "lab1", bob = "lab2"))
  expect_equal(unlist(result$data_area), c(lab1 = "/data/lab1", lab2 = "/data/lab2"))
})

test_that("read_access_yaml errors when access yaml has not been created", {
  state <- set_cascade_access_dir()
  on.exit(restore_cascade_access_dir(state), add = TRUE)

  expect_error(
    read_access_yaml(),
    "Access yaml not found"
  )
})

# ---- check_user_access ----

test_that("check_user_access returns data areas for a regular user", {
  access <- list(
    user_group = list(alice = "lab1", bob = "lab2", root = "admin"),
    data_area = list(lab1 = "/data/lab1", lab2 = "/data/lab2")
  )

  result <- check_user_access(access, "alice")

  expect_equal(unlist(result$user_group), c(alice = "lab1"))
  expect_equal(unlist(result$data_area), c(lab1 = "/data/lab1"))
})

test_that("check_user_access gives admin users all data areas", {
  access <- list(
    user_group = list(alice = "lab1", root = "admin"),
    data_area = list(lab1 = "/data/lab1", lab2 = "/data/lab2")
  )

  result <- check_user_access(access, "root")

  expect_equal(unlist(result$user_group), c(root = "admin"))
  expect_equal(unlist(result$data_area), c(lab1 = "/data/lab1", lab2 = "/data/lab2"))
})

test_that("check_user_access returns NULL for an unknown user", {
  access <- list(
    user_group = list(alice = "lab1"),
    data_area = list(lab1 = "/data/lab1")
  )

  expect_null(check_user_access(access, "missing"))
})

test_that("check_user_access returns NULL when user's group has no data area", {
  access <- list(
    user_group = list(alice = "lab1"),
    data_area = list(lab2 = "/data/lab2")
  )

  expect_null(check_user_access(access, "alice"))
})
