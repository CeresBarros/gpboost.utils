testInit <- function() {
  pf <- parent.frame()

  tmpdir <- reproducible::normPath(withr::local_tempdir(tmpdir = reproducible::tempdir2(), .local_envir = pf))
  tmpCache <- reproducible::normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))

  withr::local_options(list("reproducible.cachePath" = tmpCache), .local_envir = pf)

  reproducible::clearCache(cachePath = tmpCache, ask = FALSE, verbose = -1)
}
