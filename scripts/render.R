#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("scripts", "render_lib.R"))
args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
lang <- args$lang %||% "it"
level <- args$level %||% "full"
format <- args$format %||% "both"
targets <- if (identical(format, "both")) c("html", "pdf") else format
render_variant(lang, level, targets)
