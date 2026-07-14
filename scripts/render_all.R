#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("scripts", "render_lib.R"))
args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
format <- args$format %||% "both"
targets <- if (identical(format, "both")) c("html", "pdf") else format
for (lang in c("it", "en")) {
  for (level in c("full", "short")) render_variant(lang, level, targets)
}
message("Generated all requested CV variants in dist/html and dist/pdf.")
