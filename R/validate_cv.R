canonical_files <- function() {
  c(
    "data/profile.yml", "data/positions.csv", "data/education.csv", "data/grants.csv",
    "data/teaching.csv", "data/service.csv", "data/presentations.csv", "data/outreach.csv",
    "data/publications.bib", "data/OPEN_SCIENCE_BADGES.md", "config/variants.yml", "config/allowed_values.yml",
    "i18n/it.yml", "i18n/en.yml", "cv.qmd"
  )
}

required_columns <- function() {
  list(
    positions = c("id", "category", "section", "start_date", "end_date", "current", "role_it", "role_en", "include_short", "source_page"),
    education = c("id", "award_date", "degree_it", "degree_en", "institution_original", "include_short", "source_page"),
    grants = c("id", "level", "role", "project_title_original", "start_date", "current", "amount_eur", "amount_scope", "include_short", "source_page"),
    teaching = c("id", "category", "academic_years", "role_it", "role_en", "course_title_original", "include_short", "source_page"),
    service = c("id", "category", "subsection", "current", "role_it", "role_en", "title_original", "include_short", "source_page"),
    presentations = c("id", "date", "authors", "title_original", "presentation_type", "include_short", "source_page"),
    outreach = c("id", "current", "category", "role_it", "role_en", "title_original", "include_short", "source_page")
  )
}

append_problem <- function(problems, message) unique(c(problems, message))

date_floor <- function(x) {
  x <- as.character(x)
  ifelse(nchar(x) == 4L, paste0(x, "-01-01"), ifelse(nchar(x) == 7L, paste0(x, "-01"), x))
}

validate_table <- function(name, data, required, allowed, last_updated) {
  errors <- character()
  warnings <- character()
  missing <- setdiff(required, names(data))
  if (length(missing)) errors <- append_problem(errors, paste0(name, ": missing columns ", paste(missing, collapse = ", ")))
  if (!"id" %in% names(data)) return(list(errors = errors, warnings = warnings))
  if (any(!nzchar(data$id))) errors <- append_problem(errors, paste0(name, ": empty id"))
  if (anyDuplicated(data$id)) errors <- append_problem(errors, paste0(name, ": duplicate id"))
  if (any(duplicated(data))) errors <- append_problem(errors, paste0(name, ": fully duplicated row"))

  date_columns <- intersect(c("date", "start_date", "end_date", "event_date", "award_date"), names(data))
  for (column in date_columns) {
    invalid <- !valid_iso_date(data[[column]])
    if (any(invalid)) errors <- append_problem(errors, paste0(name, ": invalid ISO date in ", column, " for ", paste(data$id[invalid], collapse = ", ")))
    future <- nzchar(data[[column]]) & date_floor(data[[column]]) > date_floor(last_updated)
    if (any(future)) errors <- append_problem(errors, paste0(name, ": date after source last_updated in ", column, " for ", paste(data$id[future], collapse = ", ")))
  }
  if (all(c("start_date", "end_date") %in% names(data))) {
    comparable <- nzchar(data$start_date) & nzchar(data$end_date)
    backwards <- comparable & date_floor(data$end_date) < date_floor(data$start_date)
    if (any(backwards)) errors <- append_problem(errors, paste0(name, ": end_date precedes start_date for ", paste(data$id[backwards], collapse = ", ")))
  }
  if (all(c("current", "end_date") %in% names(data))) {
    incoherent <- as_cv_flag(data$current) & nzchar(data$end_date)
    if (any(incoherent)) errors <- append_problem(errors, paste0(name, ": current=true with end_date for ", paste(data$id[incoherent], collapse = ", ")))
  }

  value_rules <- list(
    positions = c(category = "positions_category", section = "positions_section"),
    grants = c(level = "grant_level", role = "grant_role", amount_scope = "amount_scope"),
    teaching = c(category = "teaching_category"),
    service = c(category = "service_category", subsection = "service_subsection"),
    presentations = c(presentation_type = "presentation_type"),
    outreach = c(category = "outreach_category")
  )
  for (column in names(value_rules[[name]] %||% character())) {
    invalid <- !data[[column]] %in% allowed[[value_rules[[name]][[column]]]]
    if (any(invalid)) errors <- append_problem(errors, paste0(name, ": invalid ", column, " for ", paste(data$id[invalid], collapse = ", ")))
  }

  paired_fields <- intersect(c("role", "description", "degree", "field", "department", "employment_regime", "program", "school"), sub("_(it|en)$", "", grep("_(it|en)$", names(data), value = TRUE)))
  for (field in paired_fields) {
    it <- data[[paste0(field, "_it")]]
    en <- data[[paste0(field, "_en")]]
    missing_translation <- xor(nzchar(it), nzchar(en))
    if (any(missing_translation)) errors <- append_problem(errors, paste0(name, ": incomplete bilingual field ", field, " for ", paste(data$id[missing_translation], collapse = ", ")))
  }

  if ("source_page" %in% names(data) && any(is.na(data$source_page))) warnings <- append_problem(warnings, paste0(name, ": records without source_page"))
  list(errors = errors, warnings = warnings)
}

collect_profile_ids <- function(profile) {
  collections <- c("research_interests", "collaborations", "memberships", "research_groups", "supervision", "skills")
  ids <- character()
  errors <- character()
  for (collection in collections) {
    values <- profile[[collection]]
    current <- vapply(values, function(item) as.character(item$id %||% ""), character(1))
    if (any(!nzchar(current))) errors <- append_problem(errors, paste0("profile.", collection, ": missing id"))
    ids <- c(ids, current)
  }
  list(ids = ids, errors = errors)
}

collect_urls <- function(x) {
  result <- character()
  walk <- function(value, name = "") {
    if (is.list(value)) {
      for (child in names(value)) walk(value[[child]], child)
    } else if (grepl("url|homepage|github|slides|courses", name, ignore.case = TRUE)) {
      result <<- c(result, as.character(value))
    }
  }
  walk(x)
  result[nzchar(result)]
}

validate_cv_raw <- function(raw, root = ".") {
  errors <- character()
  warnings <- character()
  missing_files <- canonical_files()[!file.exists(file.path(root, canonical_files()))]
  if (length(missing_files)) errors <- append_problem(errors, paste("Missing canonical files:", paste(missing_files, collapse = ", ")))

  tables <- raw[c("positions", "education", "grants", "teaching", "service", "presentations", "outreach")]
  requirements <- required_columns()
  last_updated <- raw$profile$source$last_updated
  for (name in names(tables)) {
    result <- validate_table(name, tables[[name]], requirements[[name]], raw$allowed, last_updated)
    errors <- c(errors, result$errors)
    warnings <- c(warnings, result$warnings)
  }

  profile_ids <- collect_profile_ids(raw$profile)
  errors <- c(errors, profile_ids$errors)
  all_ids <- c(profile_ids$ids, unlist(lapply(tables, `[[`, "id"), use.names = FALSE))
  if (anyDuplicated(all_ids[nzchar(all_ids)])) errors <- append_problem(errors, "Non-bibliographic ids are not globally unique")

  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", raw$profile$person$email)) errors <- append_problem(errors, "Invalid profile email")
  urls <- c(collect_urls(raw$profile), unlist(lapply(tables, function(data) unlist(data[intersect(c("url", "slides_url"), names(data))], use.names = FALSE)), use.names = FALSE))
  bad_urls <- nzchar(urls) & !grepl("^https?://[^[:space:]]+$", urls)
  if (any(bad_urls)) errors <- append_problem(errors, paste("Invalid URLs:", paste(unique(urls[bad_urls]), collapse = ", ")))
  if (length(urls)) warnings <- append_problem(warnings, paste(length(unique(urls)), "URLs were validated syntactically but not all were checked online"))

  publication_result <- validate_bibliography(raw$publications)
  errors <- c(errors, publication_result$errors)
  warnings <- c(warnings, publication_result$warnings)

  it_keys <- flatten_keys(raw$translations$it)
  en_keys <- flatten_keys(raw$translations$en)
  if (!identical(it_keys, en_keys)) {
    missing_it <- setdiff(en_keys, it_keys)
    missing_en <- setdiff(it_keys, en_keys)
    errors <- append_problem(errors, paste0("i18n key mismatch; missing it=[", paste(missing_it, collapse = ", "), "], missing en=[", paste(missing_en, collapse = ", "), "]"))
  }
  modes <- unlist(raw$variants[c("full", "short")], use.names = FALSE)
  if (any(!modes %in% raw$allowed$section_mode)) errors <- append_problem(errors, "Invalid full/short section mode")
  if (!setequal(names(raw$variants$full), raw$variants$section_order) || !setequal(names(raw$variants$short), raw$variants$section_order)) errors <- append_problem(errors, "Variant sections do not match section_order")

  for (level in raw$allowed$level) {
    selected <- select_cv(raw, "en", level)
    for (section in selected$section_order) {
      data <- selected$sections[[section]]$data
      if (is.data.frame(data) && any(vapply(data, function(column) any(as.character(column) %in% c("NA", "NULL", "TODO", "XXX")), logical(1)))) {
        errors <- append_problem(errors, paste0("Placeholder value in selected section ", section, " (", level, ")"))
      }
    }
  }
  short <- select_cv(raw, "it", "short")
  full <- select_cv(raw, "it", "full")
  for (section in raw$variants$section_order) {
    short_data <- short$sections[[section]]$data
    full_data <- full$sections[[section]]$data
    if (is.data.frame(short_data) && is.data.frame(full_data)) {
      key <- if ("id" %in% names(short_data)) "id" else if ("citekey" %in% names(short_data)) "citekey" else NULL
      if (!is.null(key) && any(!short_data[[key]] %in% full_data[[key]])) errors <- append_problem(errors, paste0("Short is not a subset of full for ", section))
    }
  }

  list(errors = unique(errors), warnings = unique(warnings))
}

validate_loaded_cv <- function(cv, strict = TRUE) {
  errors <- character()
  if (!inherits(cv, "cv_data")) errors <- c(errors, "Object is not cv_data")
  if (!cv$lang %in% c("it", "en")) errors <- c(errors, "Invalid selected language")
  if (!cv$level %in% c("full", "short")) errors <- c(errors, "Invalid selected level")
  if (strict && length(errors)) stop(paste(errors, collapse = "\n"), call. = FALSE)
  invisible(errors)
}

validate_outputs <- function(root = ".", require_all = FALSE) {
  expected <- c(
    file.path("dist", "html", paste0("cv-", rep(c("it", "en"), each = 2), "-", rep(c("full", "short"), 2), ".html")),
    file.path("dist", "pdf", paste0("cv-", rep(c("it", "en"), each = 2), "-", rep(c("full", "short"), 2), ".pdf"))
  )
  paths <- file.path(root, expected)
  errors <- character()
  if (require_all && any(!file.exists(paths))) errors <- c(errors, paste("Missing outputs:", paste(expected[!file.exists(paths)], collapse = ", ")))
  existing <- paths[file.exists(paths)]
  empty <- existing[file.info(existing)$size <= 0]
  if (length(empty)) errors <- c(errors, paste("Empty outputs:", paste(empty, collapse = ", ")))
  html <- existing[grepl("[.]html$", existing)]
  for (path in html) {
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    cleaned <- gsub("XXXI{0,3} Congresso", "", text)
    # Embedded binary resources may coincidentally contain strings such as "NA".
    cleaned <- gsub('data:image/[^;]+;base64,[^"]+', "", cleaned, perl = TRUE)
    if (!grepl("Enrico Toffalini", text, fixed = TRUE)) errors <- c(errors, paste(basename(path), "does not contain the name"))
    if (grepl("\\b(?:NA|NULL|TODO|XXX)\\b", cleaned, perl = TRUE)) errors <- c(errors, paste(basename(path), "contains a forbidden placeholder"))
    if (grepl("Execution halted|Error in|Quitting from lines", text)) errors <- c(errors, paste(basename(path), "contains a rendering error"))
  }
  unique(errors)
}

report_validation <- function(result) {
  for (warning in result$warnings) message("WARNING: ", warning)
  if (length(result$errors)) {
    for (error in result$errors) message("ERROR: ", error)
    return(FALSE)
  }
  message("Validation passed with ", length(result$warnings), " warning(s).")
  TRUE
}
