`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (length(x) == 1L && is.na(x))) y else x
}

is_blank <- function(x) {
  is.null(x) || !length(x) || all(is.na(x) | !nzchar(trimws(as.character(x))))
}

as_cv_flag <- function(x) {
  if (is.logical(x)) return(x)
  toupper(trimws(as.character(x))) %in% c("TRUE", "T", "1", "YES")
}

is_latex_cv <- function() {
  knitr::is_latex_output()
}

is_html_cv <- function() {
  knitr::is_html_output()
}

as_cv_number <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  suppressWarnings(as.numeric(x))
}

interpolate_text <- function(template, values = list()) {
  result <- as.character(template)
  for (name in names(values)) {
    result <- gsub(paste0("{", name, "}"), as.character(values[[name]]), result, fixed = TRUE)
  }
  result
}

nested_value <- function(x, key) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  value <- x
  for (part in parts) {
    if (is.null(value[[part]])) return(NULL)
    value <- value[[part]]
  }
  value
}

t_cv <- function(translations, lang, key, ...) {
  dictionary <- translations[[lang]]
  if (is.null(dictionary)) stop("Unknown language: ", lang, call. = FALSE)
  value <- nested_value(dictionary, key)
  if (is.null(value) || length(value) != 1L) {
    stop("Missing i18n key '", key, "' for language '", lang, "'", call. = FALSE)
  }
  interpolate_text(value, list(...))
}

row_text <- function(row, field) {
  value <- row[[field]] %||% ""
  value <- as.character(value[[1]] %||% "")
  if (is.na(value)) "" else trimws(value)
}

localized_value <- function(row, field, lang, original_field = NULL, warn = FALSE) {
  primary <- row_text(row, paste0(field, "_", lang))
  if (nzchar(primary)) return(primary)
  if (!is.null(original_field)) {
    original <- row_text(row, original_field)
    if (nzchar(original)) return(original)
  }
  other_lang <- if (identical(lang, "it")) "en" else "it"
  fallback <- row_text(row, paste0(field, "_", other_lang))
  if (nzchar(fallback) && warn) {
    warning("Language fallback for field '", field, "' to ", other_lang, call. = FALSE)
  }
  fallback
}

valid_iso_date <- function(x) {
  x <- as.character(x)
  !nzchar(x) | grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", x)
}

format_cv_date <- function(x, lang, translations) {
  if (is_blank(x)) return("")
  x <- as.character(x[[1]])
  parts <- strsplit(x, "-", fixed = TRUE)[[1]]
  if (length(parts) == 1L) return(parts[[1]])
  month <- t_cv(translations, lang, paste0("months.", parts[[2]]))
  if (length(parts) == 2L) return(paste(month, parts[[1]]))
  day <- as.integer(parts[[3]])
  if (identical(lang, "it")) paste(day, month, parts[[1]]) else paste0(month, " ", day, ", ", parts[[1]])
}

format_date_range <- function(start_date, end_date, current, lang, translations) {
  start <- format_cv_date(start_date, lang, translations)
  finish <- if (isTRUE(as_cv_flag(current))) t_cv(translations, lang, "present") else format_cv_date(end_date, lang, translations)
  if (!nzchar(start)) return(finish)
  if (!nzchar(finish) || identical(start, finish)) return(start)
  paste(start, finish, sep = " – ")
}

format_euro <- function(value, lang = "it") {
  if (is_blank(value) || is.na(as_cv_number(value))) return("")
  number <- as_cv_number(value)
  mark <- if (identical(lang, "it")) "." else ","
  decimal <- if (identical(lang, "it")) "," else "."
  amount <- formatC(number, format = "f", digits = if (number %% 1 == 0) 0 else 2, big.mark = mark, decimal.mark = decimal)
  if (identical(lang, "it")) paste("€", amount) else paste0("€", amount)
}

normalize_doi <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x <- sub("(?i)^doi:\\s*", "", x, perl = TRUE)
  repeat {
    normalized <- sub("(?i)^https?://(?:dx\\.)?doi\\.org/", "", x, perl = TRUE)
    if (identical(normalized, x)) break
    x <- normalized
  }
  tolower(sub("[.)]+$", "", x))
}

format_person_list <- function(people, bold_name = TRUE) {
  people <- trimws(as.character(people))
  people <- people[nzchar(people)]
  if (bold_name) {
    people <- vapply(people, function(person) {
      if (identical(person, "others")) "…" else if (person %in% c("E. Toffalini", "Enrico Toffalini", "Toffalini, E.")) paste0("**", person, "**") else person
    }, character(1))
  }
  if (!length(people)) return("")
  if (length(people) == 1L) return(people)
  if (length(people) == 2L) return(paste(people, collapse = " & "))
  paste0(paste(people[-length(people)], collapse = ", "), ", & ", people[[length(people)]])
}

markdown_link <- function(label, url) {
  if (is_blank(url)) return(as.character(label))
  paste0("[", label, "](", url, ")")
}

nonempty <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- as.character(values)
  values[!is.na(values) & nzchar(trimws(values))]
}

sort_records <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(data)
  if ("sort_order" %in% names(data) && any(!is.na(data$sort_order))) {
    return(data[order(data$sort_order, na.last = TRUE), , drop = FALSE])
  }
  candidates <- intersect(c("date", "event_date", "award_date", "start_date", "year"), names(data))
  if (!length(candidates)) return(data)
  key <- data[[candidates[[1]]]]
  data[order(key, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

flatten_keys <- function(x, prefix = "") {
  if (!is.list(x)) return(prefix)
  result <- character()
  for (name in names(x)) {
    key <- if (nzchar(prefix)) paste(prefix, name, sep = ".") else name
    if (is.list(x[[name]]) && !is.null(names(x[[name]]))) result <- c(result, flatten_keys(x[[name]], key)) else result <- c(result, key)
  }
  sort(unique(result))
}
