#!/usr/bin/env Rscript

# One-time, source-faithful extraction of the two longest lists in the CV.
# The generated BibTeX/CSV files are reviewed canonical data; this script is
# retained only to make the initial migration auditable.

source_path <- file.path("migration", "extracted-layout.txt")
if (!file.exists(source_path)) stop("Missing ", source_path, call. = FALSE)

raw <- readChar(source_path, nchars = file.info(source_path)$size, useBytes = TRUE)
Encoding(raw) <- "UTF-8"
pages <- strsplit(raw, "\f", fixed = TRUE)[[1]]
pages <- pages[nzchar(pages)]

trim <- function(x) trimws(gsub("\r", "", x, fixed = TRUE))

join_wrapped <- function(lines) {
  lines <- trim(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  out <- lines[[1]]
  if (length(lines) > 1) {
    for (line in lines[-1]) {
      if (grepl("-$", out)) out <- paste0(out, line) else out <- paste(out, line)
    }
  }
  gsub("[[:space:]]+", " ", out)
}

normalize_doi <- function(x) {
  x <- trimws(x)
  x <- sub("(?i)^doi:\\s*", "", x, perl = TRUE)
  repeat {
    next_x <- sub("(?i)^https?://(?:dx\\.)?doi\\.org/", "", x, perl = TRUE)
    if (identical(next_x, x)) break
    x <- next_x
  }
  x <- sub("[.)]+$", "", x, perl = TRUE)
  tolower(x)
}

split_authors <- function(x) {
  tokens <- strsplit(gsub("[[:space:]]+", " ", x), ",\\s*", perl = TRUE)[[1]]
  result <- character()
  i <- 1L
  initials <- "^(?:[[:upper:]][.]?\\s*)+$"
  while (i <= length(tokens)) {
    token <- sub("^&\\s*", "", trimws(tokens[[i]]))
    if (i < length(tokens) && grepl(initials, trimws(tokens[[i + 1L]]), perl = TRUE)) {
      result <- c(result, paste0(token, ", ", trimws(tokens[[i + 1L]])))
      i <- i + 2L
    } else {
      if (token == "...") token <- "others"
      result <- c(result, token)
      i <- i + 1L
    }
  }
  result[nzchar(result)]
}

slug <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- "record"
  x <- tolower(gsub("[^a-zA-Z0-9]+", "-", x))
  gsub("(^-|-$)", "", x)
}

make_citekeys <- function(authors, years, titles) {
  stopwords <- c("a", "an", "and", "are", "for", "from", "in", "is", "of", "on", "the", "to", "with")
  keys <- character(length(titles))
  used <- character()
  for (i in seq_along(titles)) {
    first <- strsplit(authors[[i]], " and ", fixed = TRUE)[[1]][[1]]
    first <- sub(",.*$", "", first)
    words <- strsplit(slug(titles[[i]]), "-", fixed = TRUE)[[1]]
    word <- words[!words %in% stopwords & nchar(words) > 2][[1]]
    base <- paste0(slug(first), years[[i]], word)
    key <- base
    suffix <- 0L
    while (key %in% used) {
      suffix <- suffix + 1L
      key <- paste0(base, letters[[suffix]])
    }
    keys[[i]] <- key
    used <- c(used, key)
  }
  keys
}

pub_headings <- c(
  "Articoli su riviste scientifiche internazionali con peer-review" = "journal_international",
  "Pubblicazioni su riviste italiane con peer-review" = "journal_italian",
  "Capitoli in libri/volumi nazionali e internazionali" = "book_chapter"
)

pub_records <- list()
state <- NULL
current <- character()
current_page <- NA_integer_

flush_publication <- function() {
  if (length(current)) {
    pub_records[[length(pub_records) + 1L]] <<- list(
      category = state,
      source_page = current_page,
      citation = join_wrapped(current)
    )
  }
  current <<- character()
  current_page <<- NA_integer_
}

for (page_number in seq_along(pages)) {
  lines <- strsplit(gsub("\r", "", pages[[page_number]], fixed = TRUE), "\n", fixed = TRUE)[[1]]
  for (line in lines) {
    value <- trim(line)
    if (value %in% names(pub_headings)) {
      flush_publication()
      state <- unname(pub_headings[[value]])
      next
    }
    if (identical(value, "Attività editoriale e di referaggio")) {
      flush_publication()
      state <- NULL
      next
    }
    if (!is.null(state) && grepl("^•", value)) {
      flush_publication()
      current <- sub("^•\\s*", "", value)
      current_page <- page_number
    } else if (!is.null(state) && length(current) && nzchar(value)) {
      current <- c(current, value)
    }
  }
}
flush_publication()

parse_publication <- function(record) {
  citation <- record$citation
  doi_match <- regexpr("(?i)10\\.[0-9]{4,9}/[-._;()/:A-Z0-9]+", citation, perl = TRUE)
  doi <- if (doi_match[[1]] > 0) normalize_doi(regmatches(citation, doi_match)) else ""
  link_match <- regexpr("(?i)(?:https?://(?:dx\\.)?doi\\.org/|doi:)", citation, perl = TRUE)
  main <- if (link_match[[1]] > 0) substr(citation, 1L, link_match[[1]] - 1L) else citation
  main <- sub("[.]$", "", trimws(main))

  year_match <- regexpr("\\([0-9]{4}\\)[.]", main, perl = TRUE)
  if (year_match[[1]] < 1) stop("Cannot parse publication year: ", citation, call. = FALSE)
  year <- substr(main, year_match[[1]] + 1L, year_match[[1]] + 4L)
  authors_source <- trimws(substr(main, 1L, year_match[[1]] - 1L))
  after_year <- trimws(substr(main, year_match[[1]] + attr(year_match, "match.length"), nchar(main)))

  split_result <- gregexpr("[.?!]\\s+(?=[[:upper:]À-ÖØ-Þ])", after_year, perl = TRUE)[[1]]
  split_lengths <- attr(split_result, "match.length")
  valid_splits <- split_result > 0
  split_matches <- split_result[valid_splits]
  split_lengths <- split_lengths[valid_splits]
  if (!length(split_matches)) stop("Cannot split title and venue: ", citation, call. = FALSE)
  split_index <- if (record$category == "book_chapter") 1L else length(split_matches)
  split_position <- split_matches[[split_index]]
  split_length <- split_lengths[[split_index]]
  delimiter <- substr(after_year, split_position, split_position)
  title_end <- if (delimiter == ".") split_position - 1L else split_position
  title <- trimws(substr(after_year, 1L, title_end))
  venue <- trimws(substr(after_year, split_position + split_length, nchar(after_year)))

  author_parts <- split_authors(authors_source)
  author_bib <- vapply(author_parts, function(author) {
    if (author == "others" || grepl(",", author, fixed = TRUE)) author else paste0("{", author, "}")
  }, character(1))

  result <- list(
    category = record$category,
    source_page = record$source_page,
    citation = citation,
    author = paste(author_bib, collapse = " and "),
    authors_source = authors_source,
    title = title,
    year = year,
    journal = "",
    booktitle = "",
    editor = "",
    publisher = "",
    address = "",
    volume = "",
    number = "",
    pages = "",
    eid = "",
    doi = doi,
    verification = ""
  )

  if (record$category == "book_chapter") {
    if (identical(title, "La memoria")) {
      result$editor <- "Cherubini, P. and Bricolo, E. and Reverberi, C."
      result$booktitle <- "Psicologia Generale"
      result$address <- "Milano, Italia"
      result$publisher <- "Raffello Cortina Editore"
    } else if (identical(title, "Environment learning in individuals with Down syndrome")) {
      result$editor <- "Hodapp, R. M. and Fidler, D."
      result$booktitle <- "International Review of Research in Developmental Disabilities"
      result$volume <- "56"
      result$pages <- "123-167"
      result$publisher <- "Academic Press"
    } else if (identical(title, "La dislessia evolutiva")) {
      result$editor <- "Cornoldi, C."
      result$booktitle <- "I Disturbi dell’Apprendimento"
      result$pages <- "107-131"
      result$address <- "Bologna, Italia"
      result$publisher <- "Il Mulino"
    } else {
      stop("Unmapped book chapter: ", citation, call. = FALSE)
    }
  } else {
    venue_parts <- strsplit(venue, ",\\s*", perl = TRUE)[[1]]
    result$journal <- venue_parts[[1]]
    detail <- if (length(venue_parts) > 1) paste(venue_parts[-1], collapse = ", ") else ""
    if (nzchar(detail)) {
      colon <- regexec("^([^:]+):(.+)$", detail, perl = TRUE)
      colon_parts <- regmatches(detail, colon)[[1]]
      comma <- regexec("^([^,]+),\\s*(.+)$", detail, perl = TRUE)
      comma_parts <- regmatches(detail, comma)[[1]]
      if (length(colon_parts)) {
        result$volume <- trimws(colon_parts[[2]])
        result$eid <- trimws(colon_parts[[3]])
      } else if (length(comma_parts)) {
        first <- trimws(comma_parts[[2]])
        result$pages <- trimws(comma_parts[[3]])
        volnum <- regexec("^([^()]+)\\(([^)]+)\\)$", first, perl = TRUE)
        volnum_parts <- regmatches(first, volnum)[[1]]
        if (length(volnum_parts)) {
          result$volume <- trimws(volnum_parts[[2]])
          result$number <- trimws(volnum_parts[[3]])
        } else {
          result$volume <- first
        }
      } else {
        volnum <- regexec("^([^()]+)\\(([^)]+)\\)$", detail, perl = TRUE)
        volnum_parts <- regmatches(detail, volnum)[[1]]
        if (length(volnum_parts)) {
          result$volume <- trimws(volnum_parts[[2]])
          result$eid <- trimws(volnum_parts[[3]])
        } else {
          result$volume <- trimws(detail)
        }
      }
    }
  }
  result
}

publications <- lapply(pub_records, parse_publication)

# Targeted corrections supported by Crossref metadata captured in
# migration/doi_verification.csv. Source-only discrepancies remain in issues.csv.
set_publication <- function(title, update) {
  index <- which(vapply(publications, function(item) identical(item$title, title), logical(1)))
  if (length(index) != 1L) stop("Expected one publication for correction: ", title, call. = FALSE)
  publications[[index]] <<- update(publications[[index]])
}

crossref_dois <- c(
  "Math anxiety and math achievement in primary school children: Longitudinal relationship and predictors" = "10.1016/j.learninstruc.2024.101906",
  "Sex differences in cognition: A meta-analysis of variance ratios in the Wechsler Intelligence Scales for Children" = "10.1016/j.paid.2024.112776",
  "Learning disorders and difficulties: From a categorical to a dimensional perspective" = "10.1016/j.lindif.2024.102490"
)
for (publication_title in names(crossref_dois)) {
  set_publication(publication_title, function(item) {
    item$doi <- crossref_dois[[publication_title]]
    item$verification <- "verified_crossref"
    if (identical(publication_title, "Sex differences in cognition: A meta-analysis of variance ratios in the Wechsler Intelligence Scales for Children")) {
      item$author <- sub("Espostito, L[.]", "Esposito, L.", item$author)
      item$authors_source <- sub("Espostito, L[.]", "Esposito, L.", item$authors_source)
    }
    item
  })
}

set_publication("Stability of math anxiety and its relation with math performance over time: A meta-analysis of longitudinal studies", function(item) {
  item$author <- sub("[{]Martín-Puga[}]", "Martín-Puga, M. E.", item$author)
  item$verification <- "authors_verified_crossref_source_year_preserved"
  item
})
set_publication("Sex/gender differences in general cognitive abilities: an investigation using the Leiter‑3", function(item) {
  item$volume <- "25"; item$number <- "4"; item$pages <- "663-672"; item$eid <- ""; item$verification <- "verified_crossref"; item
})
set_publication("Environmental learning in a virtual environment: Do gender, spatial self-efficacy, and visuospatial abilities matter?", function(item) {
  item$eid <- "101704"; item$pages <- ""; item$verification <- "verified_crossref"; item
})
set_publication("Memory sensitivity and its relationship with the behavioural inhibitory and activation systems and the presence of internalising symptoms in a group of 9th to 13th graders", function(item) {
  item$eid <- "110638"; item$pages <- ""; item$verification <- "verified_crossref"; item
})
set_publication("Auditory and cognitive performance in elderly musicians and nonmusicians", function(item) {
  item$journal <- "PLOS ONE"; item$volume <- "12"; item$number <- "11"; item$eid <- "e0187881"; item$pages <- ""; item$verification <- "verified_crossref"; item
})
set_publication("Is the age of developmental milestones a predictor for future development in Down syndrome?", function(item) {
  item$author <- paste(c(
    "Locatelli, Chiara", "Onnivello, Sara", "Antonaros, Francesca", "Feliciello, Agnese", "Filoni, Sonia",
    "Rossi, Sara", "Pulina, Francesca", "Marcolin, Chiara", "Vianello, Renzo", "Toffalini, Enrico",
    "Ramacieri, Giuseppe", "Martelli, Anna", "Procaccini, Giulia", "Sperti, Giacomo", "Caracausi, Maria",
    "Pelleri, Maria Chiara", "Vitale, Lorenza", "Pirazzoli, Gian Luca", "Strippoli, Pierluigi",
    "Cocchi, Guido", "Piovesan, Allison", "Lanfranchi, Silvia"
  ), collapse = " and ")
  item$verification <- "authors_verified_crossref"
  item
})
set_publication("Gender differences at the WISC in a large group of Italian children with ADHD", function(item) {
  item$title <- "Gender Differences in the Wechsler Intelligence Scale for Children in a Large Group of Italian Children with Attention Deficit Hyperactivity Disorder"
  item$verification <- "title_verified_crossref"
  item
})
set_publication("Learning a second language: can music training or music aptitude have a role?", function(item) {
  item$title <- "Learning a second language: Can music aptitude or music training have a role?"
  item$verification <- "title_verified_crossref"
  item
})
set_publication("Positive events lead children to fewer causal false memories for scripted events", function(item) {
  item$title <- "Positive events protect children from causal false memories for scripted events"
  item$verification <- "title_verified_crossref"
  item
})
for (i in seq_along(publications)) {
  if (tolower(gsub("[[:space:]]+", "", publications[[i]]$journal)) == "plosone") publications[[i]]$journal <- "PLOS ONE"
}
authors <- vapply(publications, `[[`, character(1), "author")
years <- vapply(publications, `[[`, character(1), "year")
titles <- vapply(publications, `[[`, character(1), "title")
citekeys <- make_citekeys(authors, years, titles)

bib_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x
}

bib_lines <- character()
for (i in seq_along(publications)) {
  pub <- publications[[i]]
  first_is_toffalini <- grepl("^Toffalini,", pub$authors_source)
  selected <- (pub$category == "journal_international" && as.integer(pub$year) >= 2022 &&
                 (first_is_toffalini || grepl("PECANS statement", pub$title, fixed = TRUE))) ||
    (pub$category == "journal_italian" && first_is_toffalini && as.integer(pub$year) >= 2024)
  keyword <- switch(pub$category,
    journal_international = "cv-journal-international",
    journal_italian = "cv-journal-italian",
    book_chapter = "cv-book-chapter"
  )
  if (selected) keyword <- paste(keyword, "cv-short", sep = ",")
  needs_review <- !nzchar(pub$doi) || grepl("others", pub$author, fixed = TRUE) ||
    grepl("Espostito|0123456789|10174", pub$citation, fixed = FALSE)
  fields <- list(
    author = pub$author,
    title = pub$title,
    year = pub$year
  )
  if (pub$category == "book_chapter") {
    fields$editor <- pub$editor
    fields$booktitle <- pub$booktitle
    fields$publisher <- pub$publisher
    if (nzchar(pub$address)) fields$address <- pub$address
  } else {
    fields$journal <- pub$journal
  }
  for (field in c("volume", "number", "pages", "eid", "doi")) {
    if (nzchar(pub[[field]])) fields[[field]] <- pub[[field]]
  }
  fields$keywords <- keyword
  fields$sourcepage <- as.character(pub$source_page)
  fields$reviewstatus <- if (nzchar(pub$verification)) pub$verification else if (needs_review) "needs_review" else "verified_from_source"
  type <- if (pub$category == "book_chapter") "incollection" else "article"
  bib_lines <- c(bib_lines, paste0("@", type, "{", citekeys[[i]], ","))
  field_names <- names(fields)
  for (j in seq_along(fields)) {
    comma <- if (j < length(fields)) "," else ""
    bib_lines <- c(bib_lines, sprintf("  %s = {%s}%s", field_names[[j]], bib_escape(fields[[j]]), comma))
  }
  bib_lines <- c(bib_lines, "}", "")
}
writeLines(bib_lines, file.path("data", "publications.bib"), useBytes = TRUE)

month_number <- c(
  Gennaio = "01", Febbraio = "02", Marzo = "03", Aprile = "04",
  Maggio = "05", Giugno = "06", Luglio = "07", Agosto = "08",
  Settembre = "09", Ottobre = "10", Novembre = "11", Dicembre = "12"
)

presentation_blocks <- list()
active <- FALSE
for (page_number in seq_along(pages)) {
  page <- gsub("\r", "", pages[[page_number]], fixed = TRUE)
  if (grepl("Presentazioni e simposi in convegni scientifici", page, fixed = TRUE)) {
    active <- TRUE
    page <- sub("(?s)^.*Presentazioni e simposi in convegni scientifici nazionali e internazionali \\(relatore dove 1° nome\\)\\s*", "", page, perl = TRUE)
  }
  if (active && grepl("Terza missione, trasferimento e valorizzazione delle conoscenze", page, fixed = TRUE)) {
    page <- sub("(?s)Terza missione, trasferimento e valorizzazione delle conoscenze.*$", "", page, perl = TRUE)
    active <- FALSE
  }
  if (nzchar(page) && (active || page_number == 18L)) {
    blocks <- strsplit(page, "\\n[[:space:]]*\\n", perl = TRUE)[[1]]
    blocks <- vapply(blocks, function(block) join_wrapped(strsplit(block, "\n", fixed = TRUE)[[1]]), character(1))
    pattern <- paste0("\\((", paste(names(month_number), collapse = "|"), ") [0-9]{4}\\)")
    blocks <- blocks[grepl(pattern, blocks, perl = TRUE)]
    for (block in blocks) presentation_blocks[[length(presentation_blocks) + 1L]] <- list(source_page = page_number, text = block)
  }
}

known_cities <- c(
  "Reggio Emilia", "Victoria BC", "Civitanova Marche", "Bressanone", "Conegliano",
  "Edimburgo", "Dresda", "Vienna", "Atlanta", "Roma", "Milano", "Padova", "Pisa",
  "Foggia", "Arezzo", "Pavia", "Bologna", "Torino", "Modena", "Budapest", "Lille",
  "Chieti", "Rimini"
)
country_codes <- c(Italia = "IT", Germania = "DE", Austria = "AT", USA = "US", Scozia = "GB", Francia = "FR", Ungheria = "HU", Canada = "CA")

parse_presentation <- function(record, index) {
  pattern <- paste0("^(.*?) \\((", paste(names(month_number), collapse = "|"), ") ([0-9]{4})\\)[.]\\s*(.*)$")
  match <- regexec(pattern, record$text, perl = TRUE)
  parts <- regmatches(record$text, match)[[1]]
  if (!length(parts)) stop("Cannot parse presentation: ", record$text, call. = FALSE)
  authors_source <- trimws(parts[[2]])
  month <- parts[[3]]
  year <- parts[[4]]
  body <- trimws(parts[[5]])

  if (grepl("^Simposio organizzato", body)) {
    colon <- regexpr(":\\s*", body, perl = TRUE)
    title <- if (colon[[1]] > 0) trimws(substr(body, colon[[1]] + attr(colon, "match.length"), nchar(body))) else body
    event <- if (colon[[1]] > 0) trimws(substr(body, 1L, colon[[1]] - 1L)) else body
    presentation_type <- "symposium_organized"
  } else {
    split <- regexpr("[.?!]\\s+(?=(?:Presentazione|Poster|Articolo))", body, perl = TRUE)
    if (split[[1]] < 1) stop("Cannot split presentation title: ", record$text, call. = FALSE)
    delimiter <- substr(body, split[[1]], split[[1]])
    title_end <- if (delimiter == ".") split[[1]] - 1L else split[[1]]
    title <- trimws(substr(body, 1L, title_end))
    event <- trimws(substr(body, split[[1]] + attr(split, "match.length"), nchar(body)))
    presentation_type <- if (grepl("^Poster", event)) "poster" else if (grepl("^Articolo", event)) "paper" else if (grepl("Presentazione su invito", event)) "invited" else if (grepl("Simposio", event)) "symposium_contribution" else "oral"
  }

  title <- sub("[.]$", "", title)
  event <- sub("[.]$", "", event)
  author_parts <- split_authors(authors_source)
  city <- ""
  for (candidate in known_cities) {
    if (grepl(candidate, event, ignore.case = TRUE)) {
      city <- candidate
      break
    }
  }
  country <- ""
  for (candidate in names(country_codes)) {
    if (grepl(paste0(candidate, "$"), event)) {
      country <- unname(country_codes[[candidate]])
      break
    }
  }
  first_author <- author_parts[[1]]
  data.frame(
    id = paste0("presentation-", year, "-", sprintf("%02d", index), "-", substr(slug(title), 1L, 42L)),
    date = paste(year, month_number[[month]], sep = "-"),
    authors = paste(author_parts, collapse = ";"),
    title_original = title,
    presentation_type = presentation_type,
    event_original = event,
    symposium_original = "",
    city = city,
    country_code = country,
    invited = presentation_type == "invited",
    presenting_author = if (grepl("^Toffalini,", first_author)) "Enrico Toffalini" else "",
    slides_url = "",
    language = "",
    include_short = FALSE,
    sort_order = index,
    source_page = record$source_page,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

presentations <- do.call(rbind, Map(parse_presentation, presentation_blocks, seq_along(presentation_blocks)))
write.csv(presentations, file.path("data", "presentations.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

category_counts <- table(vapply(publications, `[[`, character(1), "category"))
message("Generated ", length(publications), " publications: ", paste(names(category_counts), category_counts, sep = "=", collapse = ", "))
message("Generated ", nrow(presentations), " presentations")
