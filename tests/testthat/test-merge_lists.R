simple_list <- list(
  a = 1L,
  b = "base string",
  c = list(
    d = 2L,
    e = "another base string"
  )
)

test_that("Merging empty overlay returns base list", {
  overlay <- list()
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = simple_list
  )
})

test_that("Merging empty base returns overlay list", {
  base <- list()
  overlay <- simple_list
  result <- merge_lists(
    base_list = base,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = overlay
  )
})

test_that("Merging NULL overlay returns base list", {
  overlay <- NULL
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = simple_list
  )
})

test_that("Merging NULL base returns overlay list", {
  base <- NULL
  overlay <- simple_list
  result <- merge_lists(
    base_list = base,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = overlay
  )
})

test_that("Merging top level keys works", {
  overlay <- list(b = "overlay string")
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "overlay string",
      c = list(
        d = 2L,
        e = "another base string"
      )
    )
  )
})

test_that("Merging top level keys works, no recursion", {
  overlay <- list(b = "overlay string")
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay,
    recursive = FALSE
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "overlay string",
      c = list(
        d = 2L,
        e = "another base string"
      )
    )
  )
})

test_that("Merging a vector can replace a list", {
  overlay <- list(c = letters[1L:5L])
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay,
    recursive = FALSE
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "base string",
      c = c("a", "b", "c", "d", "e")
    )
  )
})

test_that("Merging non-intersecting lists works", {
  overlay <- list(
    x = "overlay string",
    y = 1.5,
    z = list(
      v = 1L,
      w = "string"
    )
  )
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay,
    recursive = FALSE
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "base string",
      c = list(
        d = 2L,
        e = "another base string"
      ),
      x = "overlay string",
      y = 1.5,
      z = list(
        v = 1L,
        w = "string"
      )
    )
  )
})

test_that("Merging nested non-intersecting keys works", {
  overlay <- list(c = list(f = "overlay_string"))
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "base string",
      c = list(
        d = 2L,
        e = "another base string",
        f = "overlay_string"
      )
    )
  )
})

test_that("Merging nested non-intersecting keys replaces with no recursion", {
  overlay <- list(c = list(f = "overlay string"))
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay,
    recursive = FALSE
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "base string",
      c = list(
        f = "overlay string"
      )
    )
  )
})

test_that("Merging nested intersecting keys works", {
  overlay <- list(c = list(e = "overlay string"))
  result <- merge_lists(
    base_list = simple_list,
    overlay_list = overlay
  )
  testthat::expect_identical(
    object = result,
    expected = list(
      a = 1L,
      b = "base string",
      c = list(
        d = 2L,
        e = "overlay string"
      )
    )
  )
})

test_that("Unnamed lists throw errors - Base", {
  base <- list("foo", "bar", "baz")
  testthat::expect_error(
    object = merge_lists(
      base_list = base,
      overlay_list = simple_list
    ),
    regexp = "^Lists must be named.$"
  )
})

test_that("Unnamed lists throw errors - Overlay", {
  overlay <- list("qux", "quux")
  testthat::expect_error(
    object = merge_lists(
      base_list = simple_list,
      overlay_list = overlay
    ),
    regexp = "^Lists must be named.$"
  )
})

test_that("Unnamed lists throw errors - Both", {
  base <- list("foo", "bar", "baz")
  overlay <- list("qux", "quux")
  testthat::expect_error(
    object = merge_lists(
      base_list = base,
      overlay_list = overlay
    ),
    regexp = "^Lists must be named.$"
  )
})

test_that("Unnamed lists throw errors - Partial naming", {
  overlay <- list(a = "qux", b = "quux", "foo", "bar")
  testthat::expect_error(
    object = merge_lists(
      base_list = simple_list,
      overlay_list = overlay
    ),
    regexp = "^Lists must be named.$"
  )
})

test_that("Vectors are not acceptable inputs", {
  base <- c(a = "foo", b = "bar", c = "baz")
  overlay <- c(a = "qux", d = "quux")
  testthat::expect_error(
    object = merge_lists(
      base_list = base,
      overlay_list = overlay
    ),
    regexp = "^Both base and overlay must be lists.$"
  )
})
