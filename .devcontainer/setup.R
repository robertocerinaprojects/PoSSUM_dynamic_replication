# Run from the repository root: Rscript .devcontainer/setup.R
# The Codespace also runs this file automatically when it is first created.

# 1. Linux libraries required by the spatial R packages ----------------------
system_packages <- c(
  "make", "cmake", "pandoc", "gh", "libabsl-dev", "libssl-dev",
  "libgdal-dev", "gdal-bin", "libgeos-dev", "libproj-dev",
  "libsqlite3-dev", "libudunits2-dev"
)

if (system2("sudo", c("apt-get", "update")) != 0L) {
  stop("Could not update the Linux package index.")
}
if (system2("sudo", c("apt-get", "install", "-y", "--no-install-recommends",
                      system_packages)) != 0L) {
  stop("Could not install the Linux libraries.")
}

# 2. R packages --------------------------------------------------------------
# This Ubuntu 24.04 mirror supplies precompiled packages for the Codespace.
# Install geostan and its RStan dependencies from the same repository.
cran_repo <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"
options(
  repos = c(CRAN = cran_repo),
  HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)),
  timeout = 1800,
  Ncpus = 2
)

packages <- c("data.table", "mltools", "posterior", "renv", "sf", "spdep", "geostan")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

# CmdStanR comes from the Stan repository; reuse it if already installed.
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr", repos = c(
    STAN = "https://stan-dev.r-universe.dev", CRAN = cran_repo
  ))
}
stopifnot(all(vapply(c(packages, "cmdstanr"), requireNamespace, logical(1), quietly = TRUE)))

# 3. CmdStan -----------------------------------------------------------------
# Keep the tested version and reuse the existing installation when available.
cmdstanr::check_cmdstan_toolchain()
stan_version <- "2.40.0"
stan_dir <- file.path(path.expand("~"), ".cmdstan", paste0("cmdstan-", stan_version))
dir.create(dirname(stan_dir), recursive = TRUE, showWarnings = FALSE)

required_binaries <- file.path(stan_dir, "bin", c("stanc", "stansummary"))
if (!all(file.exists(required_binaries))) {
  cmdstanr::install_cmdstan(
    version = stan_version, dir = dirname(stan_dir),
    cores = 2, timeout = 1800, overwrite = TRUE
  )
}
cmdstanr::set_cmdstan_path(stan_dir)
stopifnot(as.character(cmdstanr::cmdstan_version()) == stan_version)

message("Setup complete: analysis packages and CmdStan ", stan_version)
