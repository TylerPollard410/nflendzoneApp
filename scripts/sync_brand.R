box::use(
  utils[download.file, unzip],
)

sync_brand_files <- function() {
  url <- "https://github.com/TylerPollard410/my-brand/archive/refs/heads/main.zip"
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  source_dir <- file.path(temp_dir, "my-brand-main", "_extensions", "my-brand")
  dest_dir <- "brand"

  download.file(url, temp_zip, quiet = TRUE)
  unzip(temp_zip, exdir = temp_dir)

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  files <- list.files(source_dir, full.names = TRUE)
  file.copy(files, dest_dir, overwrite = TRUE, recursive = TRUE)

  invisible(dest_dir)
}
