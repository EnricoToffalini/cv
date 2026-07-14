#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("R", "bibliography.R"))
source(file.path("R", "load_cv.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required", call. = FALSE)
raw <- load_cv_raw()
output_dir <- file.path("dist", "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

tables <- c("positions", "education", "grants", "teaching", "service", "presentations", "outreach")
for (name in tables) {
  write.csv(raw[[name]], file.path(output_dir, paste0(name, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

publications <- raw$publications
write.csv(publications, file.path(output_dir, "publications.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
jsonlite::write_json(
  list(schema_version = raw$schema_version, source_last_updated = raw$profile$source$last_updated, publications = publications),
  file.path(output_dir, "publications.json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null"
)

complete <- list(
  schema_version = raw$schema_version,
  source_last_updated = raw$profile$source$last_updated,
  profile = raw$profile,
  positions = raw$positions,
  education = raw$education,
  grants = raw$grants,
  teaching = raw$teaching,
  service = raw$service,
  presentations = raw$presentations,
  outreach = raw$outreach,
  publications = publications,
  variants = raw$variants
)
jsonlite::write_json(complete, file.path(output_dir, "cv.json"), pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
message("Generated interoperable exports in ", output_dir)
