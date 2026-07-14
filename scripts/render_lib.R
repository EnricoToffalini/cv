find_quarto <- function() {
  path <- Sys.which("quarto")
  if (!nzchar(path)) stop("Quarto is not available on PATH", call. = FALSE)
  unname(path)
}

render_variant <- function(lang, level, format = c("html", "pdf"), root = ".") {
  allowed_lang <- c("it", "en")
  allowed_level <- c("full", "short")
  allowed_format <- c("html", "pdf")
  if (!lang %in% allowed_lang) stop("Invalid --lang. Allowed: it, en", call. = FALSE)
  if (!level %in% allowed_level) stop("Invalid --level. Allowed: full, short", call. = FALSE)
  if (any(!format %in% allowed_format)) stop("Invalid --format. Allowed: html, pdf, both", call. = FALSE)
  quarto <- find_quarto()
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  system_root <- Sys.getenv("SystemRoot", "C:/Windows")
  latex <- Sys.which("lualatex")
  essential_path <- unique(c(
    dirname(quarto),
    file.path(R.home("bin")),
    if (nzchar(latex)) dirname(latex) else character(),
    file.path(system_root, "System32"),
    system_root,
    file.path(system_root, "System32", "WindowsPowerShell", "v1.0")
  ))
  Sys.setenv(PATH = paste(essential_path, collapse = .Platform$path.sep))
  Sys.setenv(
    QUARTO_R = file.path(R.home("bin"), "R.exe"),
    R_USER = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep)
  )
  Sys.setenv(
    R_PROFILE_USER = normalizePath(file.path(getwd(), ".Rprofile"), winslash = "/", mustWork = FALSE),
    R_ENVIRON_USER = ""
  )
  for (target in format) {
    output_dir <- file.path("dist", target)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_name <- paste0("cv-", lang, "-", level, ".", target)
    args <- c(
      "render", "cv.qmd", "--to", target,
      "-P", paste0("lang:", lang),
      "-P", paste0("level:", level),
      "-M", paste0("lang:", lang),
      "--output", output_name,
      "--output-dir", output_dir
    )
    message("Rendering ", lang, "/", level, " -> ", target)
    status <- system2(quarto, args = args)
    if (!identical(status, 0L)) stop("Quarto rendering failed for ", lang, "/", level, "/", target, call. = FALSE)
  }
  invisible(TRUE)
}

parse_cli_args <- function(args) {
  result <- list()
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--") || i == length(args)) stop("Arguments must be provided as --name value", call. = FALSE)
    result[[sub("^--", "", key)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  result
}
