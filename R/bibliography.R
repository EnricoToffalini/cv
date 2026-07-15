read_publications <- function(path = file.path("data", "publications.bib")) {
  if (!requireNamespace("bibtex", quietly = TRUE)) stop("Package 'bibtex' is required", call. = FALSE)
  bibliography <- bibtex::read.bib(path)
  entries <- unclass(bibliography)
  rows <- lapply(entries, function(entry) {
    field <- function(name) {
      value <- entry[[name]]
      if (is.null(value)) "" else paste(as.character(value), collapse = ";")
    }
    authors <- if (is.null(entry$author)) "" else paste(as.character(entry$author), collapse = ";")
    editors <- if (is.null(entry$editor)) "" else paste(as.character(entry$editor), collapse = ";")
    data.frame(
      citekey = attr(entry, "key") %||% "",
      entry_type = tolower(attr(entry, "bibtype") %||% ""),
      authors = authors,
      editors = editors,
      title = field("title"),
      year = field("year"),
      journal = field("journal"),
      booktitle = field("booktitle"),
      publisher = field("publisher"),
      address = field("address"),
      volume = field("volume"),
      number = field("number"),
      pages = field("pages"),
      eid = field("eid"),
      doi = normalize_doi(field("doi")),
      url = field("url"),
      preregistered = as_cv_flag(field("preregistered")),
      preregistration_url = field("preregistrationurl"),
      open_data = as_cv_flag(field("opendata")),
      data_url = field("dataurl"),
      open_materials = as_cv_flag(field("openmaterials")),
      materials_url = field("materialsurl"),
      open_code = as_cv_flag(field("opencode")),
      code_url = field("codeurl"),
      keywords = field("keywords"),
      source_page = suppressWarnings(as.integer(field("sourcepage"))),
      review_status = field("reviewstatus"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  publications <- do.call(rbind, rows)
  publications$category <- ifelse(grepl("cv-journal-international", publications$keywords, fixed = TRUE), "journal_international",
    ifelse(grepl("cv-journal-italian", publications$keywords, fixed = TRUE), "journal_italian",
      ifelse(grepl("cv-book-chapter", publications$keywords, fixed = TRUE), "book_chapter", "editorial")
    )
  )
  publications$include_short <- grepl("cv-short", publications$keywords, fixed = TRUE)
  publications <- publications[order(as.integer(publications$year), publications$citekey, decreasing = TRUE), , drop = FALSE]
  rownames(publications) <- NULL
  publications
}

open_science_badges <- function(row) {
  specs <- list(
    preregistered = c("Preregistered", "preregistration_url"),
    open_data = c("Open Data", "data_url"),
    open_materials = c("Open Materials", "materials_url"),
    open_code = c("Open Code", "code_url")
  )
  badges <- vapply(names(specs), function(flag) {
    if (!isTRUE(row[[flag]])) return("")
    label <- specs[[flag]][[1]]
    url <- row_text(row, specs[[flag]][[2]])
    if (nzchar(url)) paste0("[", label, "](", url, "){.os-badge}") else paste0("[", label, "]{.os-badge}")
  }, character(1))
  badges <- badges[nzchar(badges)]
  if (!length(badges)) return("")
  paste0(" [", paste(badges, collapse = " "), "]{.os-badges}")
}

format_publication <- function(row) {
  authors <- format_person_list(strsplit(row_text(row, "authors"), ";", fixed = TRUE)[[1]])
  year <- row_text(row, "year")
  title <- row_text(row, "title")
  if (row_text(row, "category") == "book_chapter") {
    editors <- format_person_list(strsplit(row_text(row, "editors"), ";", fixed = TRUE)[[1]], bold_name = FALSE)
    editor_label <- if (length(strsplit(row_text(row, "editors"), ";", fixed = TRUE)[[1]]) > 1L) "Eds." else "Ed."
    venue <- paste0("In ", editors, " (", editor_label, "), *", row_text(row, "booktitle"), "*")
    detail <- nonempty(
      if (nzchar(row_text(row, "volume"))) paste0("vol. ", row_text(row, "volume")) else "",
      if (nzchar(row_text(row, "pages"))) paste0("pp. ", row_text(row, "pages")) else "",
      row_text(row, "publisher")
    )
  } else {
    venue <- if (nzchar(row_text(row, "journal"))) paste0("*", row_text(row, "journal"), "*") else ""
    volume <- row_text(row, "volume")
    number <- row_text(row, "number")
    volume_number <- if (nzchar(volume)) paste0("**", volume, "**", if (nzchar(number)) paste0("(", number, ")") else "") else ""
    detail <- nonempty(volume_number, row_text(row, "pages"), row_text(row, "eid"))
  }
  venue_text <- paste(nonempty(venue, paste(detail, collapse = ", ")), collapse = ", ")
  doi <- row_text(row, "doi")
  doi_text <- if (nzchar(doi)) markdown_link(paste0("doi:", doi), paste0("https://doi.org/", doi)) else ""
  paste0(authors, " (", year, "). ", title, ". ", venue_text,
    if (nzchar(venue_text)) "." else "", if (nzchar(doi_text)) paste0(" ", doi_text, ".") else "", open_science_badges(row))
}

validate_bibliography <- function(publications) {
  errors <- character()
  warnings <- character()
  if (any(!nzchar(publications$citekey))) errors <- c(errors, "Publication citekeys must not be empty")
  if (anyDuplicated(publications$citekey)) errors <- c(errors, "Duplicate publication citekeys")
  dois <- publications$doi[nzchar(publications$doi)]
  if (anyDuplicated(dois)) errors <- c(errors, "Duplicate publication DOIs")
  malformed <- nzchar(publications$doi) & !grepl("^10\\.[0-9]{4,9}/[-._;()/:a-z0-9]+$", publications$doi, perl = TRUE)
  if (any(malformed)) errors <- c(errors, paste("Malformed DOI:", paste(publications$citekey[malformed], collapse = ", ")))
  required_missing <- !nzchar(publications$title) | !nzchar(publications$authors) | !grepl("^[0-9]{4}$", publications$year)
  if (any(required_missing)) errors <- c(errors, paste("Publication missing author/title/year:", paste(publications$citekey[required_missing], collapse = ", ")))
  known_categories <- c("journal_international", "journal_italian", "book_chapter", "editorial")
  if (any(!publications$category %in% known_categories)) errors <- c(errors, "Unknown publication category")
  badge_urls <- c(preregistered = "preregistration_url", open_data = "data_url", open_materials = "materials_url", open_code = "code_url")
  for (i in seq_len(nrow(publications))) {
    row <- as.list(publications[i, , drop = FALSE])
    for (flag in names(badge_urls)) {
      url <- row_text(row, badge_urls[[flag]])
      if (nzchar(url) && !isTRUE(row[[flag]])) errors <- c(errors, paste("Open-science URL without active", flag, "badge for", publications$citekey[[i]]))
      if (nzchar(url) && !grepl("^https?://[^[:space:]]+$", url)) errors <- c(errors, paste("Invalid open-science URL for", publications$citekey[[i]], ":", url))
    }
  }
  if (any(!nzchar(publications$doi))) warnings <- c(warnings, paste(sum(!nzchar(publications$doi)), "publications have no DOI"))
  list(errors = errors, warnings = warnings)
}
