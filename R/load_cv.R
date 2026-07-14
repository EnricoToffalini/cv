read_cv_csv <- function(path) {
  data <- read.csv(path, colClasses = "character", check.names = FALSE, na.strings = NULL, fileEncoding = "UTF-8")
  data[is.na(data)] <- ""
  logical_columns <- intersect(c("current", "include_short", "invited"), names(data))
  for (column in logical_columns) data[[column]] <- as_cv_flag(data[[column]])
  numeric_columns <- intersect(c("sort_order", "source_page", "amount_eur", "duration_months", "hours_per_year", "total_hours", "count"), names(data))
  for (column in numeric_columns) data[[column]] <- as_cv_number(data[[column]])
  data
}

load_cv_raw <- function(root = ".") {
  yaml_file <- function(...) yaml::read_yaml(file.path(root, ...))
  csv_file <- function(...) read_cv_csv(file.path(root, ...))
  list(
    schema_version = "1.0.0",
    profile = yaml_file("data", "profile.yml"),
    positions = csv_file("data", "positions.csv"),
    education = csv_file("data", "education.csv"),
    grants = csv_file("data", "grants.csv"),
    teaching = csv_file("data", "teaching.csv"),
    service = csv_file("data", "service.csv"),
    presentations = csv_file("data", "presentations.csv"),
    outreach = csv_file("data", "outreach.csv"),
    publications = read_publications(file.path(root, "data", "publications.bib")),
    variants = yaml_file("config", "variants.yml"),
    allowed = yaml_file("config", "allowed_values.yml"),
    translations = list(
      it = yaml_file("i18n", "it.yml"),
      en = yaml_file("i18n", "en.yml")
    )
  )
}

load_cv <- function(lang = "it", level = "full", root = ".") {
  select_cv(load_cv_raw(root), lang = lang, level = level)
}
