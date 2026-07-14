#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("R", "bibliography.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required", call. = FALSE)
publications <- read_publications()
cache_dir <- file.path("migration", "cache", "crossref")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(timeout = 30)

plain <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(gsub("[^a-z0-9]+", " ", x))
}

similarity <- function(a, b) {
  a <- plain(a)
  b <- plain(b)
  if (!nzchar(a) || !nzchar(b)) return(NA_real_)
  1 - adist(a, b)[[1]] / max(nchar(a), nchar(b))
}

crossref_year <- function(message) {
  fields <- c("published-print", "published-online", "published", "issued")
  for (field in fields) {
    value <- message[[field]][["date-parts"]] %||% NULL
    if (!is.null(value) && length(value)) return(as.character(value[[1]][[1]]))
  }
  ""
}

crossref_authors <- function(message) {
  authors <- message$author
  if (is.null(authors) || !length(authors)) return("")
  paste(vapply(authors, function(author) paste0(author$family %||% "", ", ", author$given %||% ""), character(1)), collapse = ";")
}

fetch_json <- function(url, cache_path) {
  if (file.exists(cache_path)) return(jsonlite::read_json(cache_path, simplifyVector = FALSE))
  result <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  jsonlite::write_json(result, cache_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  Sys.sleep(0.08)
  result
}

verify_one <- function(row) {
  doi <- row_text(row, "doi")
  citekey <- row_text(row, "citekey")
  tryCatch({
    if (nzchar(doi)) {
      url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE), "?mailto=enrico.toffalini@unipd.it")
      response <- fetch_json(url, file.path(cache_dir, paste0(citekey, ".json")))
      message <- response$message
      status <- "doi_verified"
    } else if (row_text(row, "category") != "book_chapter") {
      query <- utils::URLencode(row_text(row, "title"), reserved = TRUE)
      url <- paste0("https://api.crossref.org/works?query.title=", query, "&rows=3&mailto=enrico.toffalini@unipd.it")
      response <- fetch_json(url, file.path(cache_dir, paste0(citekey, "-title-search.json")))
      items <- response$message$items
      scores <- vapply(items, function(item) similarity(row_text(row, "title"), item$title[[1]]), numeric(1))
      message <- items[[which.max(scores)]]
      status <- "title_search"
    } else {
      return(data.frame(citekey = citekey, source_doi = doi, status = "no_doi_not_queried", candidate_doi = "", title_similarity = NA_real_, source_year = row_text(row, "year"), crossref_year = "", source_title = row_text(row, "title"), crossref_title = "", crossref_container = "", crossref_authors = "", error = "", stringsAsFactors = FALSE))
    }

    first_value <- function(x) if (is.null(x) || !length(x)) "" else as.character(x[[1]])
    title_parts <- c(first_value(message$title), first_value(message$subtitle))
    title <- paste(title_parts[nzchar(title_parts)], collapse = ": ")
    container <- message$`container-title`[[1]] %||% ""
    candidate_doi <- normalize_doi(message$DOI %||% doi)
    data.frame(
      citekey = citekey,
      source_doi = doi,
      status = status,
      candidate_doi = candidate_doi,
      title_similarity = round(similarity(row_text(row, "title"), title), 4),
      source_year = row_text(row, "year"),
      crossref_year = crossref_year(message),
      source_title = row_text(row, "title"),
      crossref_title = title,
      crossref_container = container,
      crossref_authors = crossref_authors(message),
      error = "",
      stringsAsFactors = FALSE
    )
  }, error = function(error) {
    data.frame(citekey = citekey, source_doi = doi, status = "error", candidate_doi = "", title_similarity = NA_real_, source_year = row_text(row, "year"), crossref_year = "", source_title = row_text(row, "title"), crossref_title = "", crossref_container = "", crossref_authors = "", error = conditionMessage(error), stringsAsFactors = FALSE)
  })
}

results <- do.call(rbind, lapply(seq_len(nrow(publications)), function(i) verify_one(as.list(publications[i, , drop = FALSE]))))
write.csv(results, file.path("migration", "doi_verification.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Crossref verification complete: ", sum(results$status == "doi_verified"), " DOI records verified; ", sum(results$status == "title_search"), " title searches; ", sum(results$status == "error"), " errors.")
