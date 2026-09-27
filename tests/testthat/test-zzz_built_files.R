
# Files that are built manually from a source file, and are committed along
# with it. Names are the source files, values the files built from them, both
# relative to the package root.
built_files <- c(
  "README.Rmd"                     = "README.md",
  "data-raw/generate-chk-atomic.R" = "R/chk-1-atomic.R"
)

test_that("Manually built files are newer than their sources", {
  skip_on_cran()
  skip_on_ci()

  # The sources are build-ignored, so this only runs in the source tree. It
  # compares modification times only, which a fresh checkout does not
  # preserve, so it is skipped on CI as well.
  root <- test_path("..", "..")
  if (!file.exists(file.path(root, "README.Rmd")))
    skip("Skipping, this only runs in the package source tree.")

  for (src in names(built_files)) {
    out      <- built_files[[src]]
    src_path <- file.path(root, src)
    out_path <- file.path(root, out)

    expect(file.exists(src_path), sprintf("Source file %s not found.", src))
    expect(file.exists(out_path), sprintf("Built file %s not found.", out))

    if (file.exists(src_path) && file.exists(out_path)) {
      expect(
        isTRUE(file.mtime(out_path) > file.mtime(src_path)),
        sprintf("%s is older than its source %s. Rebuild it.", out, src)
      )
    }
  }
})
