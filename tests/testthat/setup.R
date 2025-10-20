library(data.table)

options(
  reproducible.cachePath = file.path(tempdir(), "cache-test")
  )

withr::defer(
  {
    options(opts)
    try(reproducible::clearCache(ask = FALSE, verbose = -1))
    try(unlink("CHECKSUMS.txt"), silent = TRUE) # comes from an unknown place
  },
  teardown_env()
)
