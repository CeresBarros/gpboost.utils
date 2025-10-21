library(assertthat)
library(data.table)
library(gpboost)
library(reproducible)

opts <- options(
  repos = c(
    PE = "https://predictiveecology.r-universe.dev",
    CRAN = paste0("https://", "cloud.", "r-project.", "org")
  )
)

# withr::defer(
#   {
#     options(opts)
#     try(reproducible::clearCache(ask = FALSE, verbose = -1))
#     try(unlink("CHECKSUMS.txt"), silent = TRUE) # comes from an unknown place
#   }
# )
