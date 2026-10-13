# Run from the repository root.
stopifnot(file.exists("models/functions.stan"))

options(timeout = max(1800, getOption("timeout")))
dir.create("data/downloaded", recursive = TRUE, showWarnings = FALSE)

download_input <- function(url, filename, expected_bytes) {
  destination <- file.path("data/downloaded", filename)

  if (file.exists(destination)) {
    if (!isTRUE(file.info(destination)$size == expected_bytes)) {
      stop("Existing file has an unexpected size: ", destination)
    }
    message("Already downloaded: ", filename)
    return(invisible(destination))
  }

  temporary <- tempfile(tmpdir = "data/downloaded")
  on.exit(unlink(temporary), add = TRUE)

  message("Downloading: ", filename)
  download.file(url, temporary, mode = "wb")

  if (!isTRUE(file.info(temporary)$size == expected_bytes)) {
    stop("Downloaded file has an unexpected size: ", filename)
  }

  if (!file.rename(temporary, destination)) {
    stop("Could not save: ", destination)
  }

  invisible(destination)
}

# Published survey snapshots, pinned to an upstream commit.
source_commit <- "c4e81571653922c98657dda11638f6288106b0ee"

waves <- c(
  "2024-08-15_2024-08-23",
  "2024-09-07_2024-09-12",
  "2024-09-22_2024-09-29",
  "2024-10-05_2024-10-15",
  "2024-10-17_2024-10-26"
)

survey_bytes <- c(925697, 920549, 943549, 1048187, 1021510)

survey_base <- paste0(
  "https://raw.githubusercontent.com/",
  "robertocerinaprojects/PoSSUM/",
  source_commit, "/survey_data/"
)

for (i in seq_along(waves)) {
  filename <- paste0("survey_object.clean_", waves[i], ".csv")
  download_input(
    paste0(survey_base, filename),
    filename,
    survey_bytes[i]
  )
}

# Population frame: approximately 348 MiB.
download_input(
  paste0(
    "https://github.com/robertocerinaprojects/",
    "PoSSUM_dynamic_replication/releases/download/",
    "data-v0.1.0/SF_extended_simple.rds"
  ),
  "SF_extended_simple.rds",
  364466876
)

message("All six input files are available in data/downloaded/.")
