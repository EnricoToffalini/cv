#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("scripts", "render_lib.R"))

args <- parse_cli_args(commandArgs(trailingOnly = TRUE))
doi <- normalize_doi(args$doi %||% "")
if (!grepl("^10\\.[0-9]{4,9}/[-._;()/:a-z0-9]+$", doi, perl = TRUE)) stop("Provide a valid DOI with --doi", call. = FALSE)

category <- args$category %||% ""
allowed_categories <- c("journal_international", "journal_italian", "book_chapter", "editorial")
if (!nzchar(category) && interactive()) category <- readline(paste0("Category [", paste(allowed_categories, collapse = "/"), "]: "))
if (!category %in% allowed_categories) stop("Provide --category with one of: ", paste(allowed_categories, collapse = ", "), call. = FALSE)

bib_path <- file.path("data", "publications.bib")
existing <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (grepl(doi, tolower(existing), fixed = TRUE)) stop("DOI already exists in publications.bib: ", doi, call. = FALSE)

endpoint <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE))
response <- tryCatch(jsonlite::fromJSON(endpoint), error = function(error) stop("Crossref lookup failed: ", conditionMessage(error), call. = FALSE))
work <- response$message
title <- work$title[[1]] %||% ""
year <- as.character(work$published[["date-parts"]][[1]][[1]])
journal <- work$`container-title`[[1]] %||% ""

authors <- character()
if (!is.null(work$author) && nrow(work$author)) {
  authors <- vapply(seq_len(nrow(work$author)), function(i) {
    family <- work$author$family[[i]] %||% ""
    given <- work$author$given[[i]] %||% ""
    paste0(family, ", ", given)
  }, character(1))
}
if (!length(authors) || !nzchar(title) || !nzchar(year)) stop("Crossref returned incomplete author/title/year metadata", call. = FALSE)

first <- tolower(gsub("[^a-z0-9]", "", iconv(sub(",.*$", "", authors[[1]]), to = "ASCII//TRANSLIT")))
word <- strsplit(tolower(gsub("[^a-zA-Z0-9]+", " ", iconv(title, to = "ASCII//TRANSLIT"))), " +")[[1]]
word <- word[!word %in% c("a", "an", "and", "for", "from", "in", "of", "on", "the", "to", "with") & nchar(word) > 2][[1]]
citekey <- paste0(first, year, word)
if (grepl(paste0("[@][^{]+[{]", citekey, ","), existing)) stop("Proposed citekey already exists: ", citekey, call. = FALSE)

keyword <- switch(category,
  journal_international = "cv-journal-international",
  journal_italian = "cv-journal-italian",
  book_chapter = "cv-book-chapter",
  editorial = "cv-editorial"
)
entry_type <- if (identical(category, "book_chapter")) "incollection" else "article"
fields <- c(
  paste0("  author = {", paste(authors, collapse = " and "), "}"),
  paste0("  title = {", title, "}"),
  paste0("  year = {", year, "}"),
  if (nzchar(journal)) paste0("  journal = {", journal, "}") else character(),
  if (!is.null(work$volume) && nzchar(work$volume)) paste0("  volume = {", work$volume, "}") else character(),
  if (!is.null(work$issue) && nzchar(work$issue)) paste0("  number = {", work$issue, "}") else character(),
  if (!is.null(work$page) && nzchar(work$page)) paste0("  pages = {", work$page, "}") else character(),
  paste0("  doi = {", doi, "}"),
  paste0("  keywords = {", keyword, "}"),
  "  reviewstatus = {needs_manual_review}"
)
entry <- c("", paste0("@", entry_type, "{", citekey, ","), paste0(fields, c(rep(",", length(fields) - 1L), "")), "}")
write(entry, file = bib_path, append = TRUE, useBytes = TRUE)
message("Added ", citekey, ". Verify authors, title, venue, pages/article number, category, and citekey before committing. The entry was not marked cv-short.")
