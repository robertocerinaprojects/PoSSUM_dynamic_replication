# Install the core R packages and the tested Stan version.
options(
  repos = c(
    STAN = "https://stan-dev.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  ),
  timeout = 1800,
  Ncpus = 2
)

packages <- c(
  "data.table", "mltools", "posterior", "cmdstanr", "renv"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing)) {
  install.packages(missing)
}

stopifnot(
  all(vapply(packages, requireNamespace, logical(1), quietly = TRUE))
)

cmdstanr::check_cmdstan_toolchain()

stan_version <- "2.40.0"
stan_dir <- file.path(
  path.expand("~"), ".cmdstan", paste0("cmdstan-", stan_version)
)

dir.create(dirname(stan_dir), recursive = TRUE, showWarnings = FALSE)

required_binaries <- file.path(
  stan_dir, "bin", c("stanc", "stansummary")
)

if (!all(file.exists(required_binaries))) {
  cmdstanr::install_cmdstan(
    version = stan_version,
    dir = dirname(stan_dir),
    cores = 2,
    timeout = 1800,
    overwrite = TRUE
  )
}

cmdstanr::set_cmdstan_path(stan_dir)

stopifnot(
  as.character(cmdstanr::cmdstan_version()) == stan_version
)

message("Setup complete: core R packages and CmdStan ", stan_version)
