md_div <- function(content, class_name) {
  paste0("::: {.", class_name, "}\n", content, "\n:::\n")
}

cv_entry <- function(date, body) {
  paste0(
    "::: {.cv-entry}\n",
    "::: {.cv-date}\n", date, "\n:::\n",
    "::: {.cv-body}\n", body, "\n:::\n",
    ":::\n"
  )
}

render_variant_navigation <- function(cv) {
  items <- c(
    markdown_link("EN full", "index.html"),
    markdown_link("EN short", "cv-en-short.html"),
    markdown_link("IT full", "cv-it-full.html"),
    markdown_link("IT short", "cv-it-short.html"),
    markdown_link("PDF", paste0("pdf/cv-", cv$lang, "-", cv$level, ".pdf"))
  )
  paste0("[", paste(items, collapse = " · "), "]{.cv-variants}\n")
}

section_markdown <- function(cv, key, body) {
  if (is_blank(body)) return("")
  title <- t_cv(cv$translations, cv$lang, paste0("sections.", key))
  paste0("\n## ", title, "\n\n", body, "\n")
}

render_header <- function(cv) {
  profile <- cv$profile
  person <- profile$person
  name <- paste(person$given_name, person$family_name)
  title <- person$title[[cv$lang]]
  affiliation <- paste(profile$affiliation$department[[cv$lang]], profile$affiliation$organization[[cv$lang]], sep = ", ")
  links <- c(
    markdown_link("Web", profile$links$homepage),
    markdown_link("GitHub", profile$links$github),
    markdown_link(t_cv(cv$translations, cv$lang, "slides_available"), profile$links$slides)
  )
  updated <- format_cv_date(profile$source$last_updated, cv$lang, cv$translations)
  content <- paste0(
    "::: {.cv-identity}\n",
    "::: {.cv-name}\n", name, "\n:::\n",
    "::: {.cv-details}\n",
    "**", title, "**  \n",
    affiliation, "  \n",
    person$email, " · ", paste(links, collapse = " · "), "  \n",
    "[", t_cv(cv$translations, cv$lang, "updated"), ": ", updated, "]{.updated}\n",
    render_variant_navigation(cv),
    ":::\n:::\n",
    "![](imgs/profile.jpg){.cv-photo}\n"
  )
  md_div(content, "cv-header")
}

render_positions <- function(cv, data) {
  if (!nrow(data)) return("")
  entries <- apply(data, 1L, function(row) {
    row <- as.list(row)
    date <- format_date_range(row$start_date, row$end_date, row$current, cv$lang, cv$translations)
    role <- localized_value(row, "role", cv$lang)
    department <- localized_value(row, "department", cv$lang)
    organization <- row_text(row, "organization_original")
    place <- row_text(row, "location")
    regime <- localized_value(row, "employment_regime", cv$lang)
    description <- localized_value(row, "description", cv$lang)
    supervisors <- row_text(row, "supervisors")
    detail <- paste(nonempty(department, regime, place), collapse = " · ")
    extra <- nonempty(description, if (nzchar(supervisors)) paste0(t_cv(cv$translations, cv$lang, "supervisor"), ": ", gsub(";", ", ", supervisors, fixed = TRUE)) else "")
    cv_entry(date, paste0(
      "**", role, "**", if (nzchar(organization)) paste0(", ", organization) else "", "  \n",
      if (nzchar(detail)) paste0(detail, "  \n") else "",
      paste(extra, collapse = "  \n")
    ))
  })
  paste(entries, collapse = "\n")
}

render_education <- function(cv, data) {
  if (!nrow(data)) return("")
  entries <- apply(data, 1L, function(row) {
    row <- as.list(row)
    degree <- localized_value(row, "degree", cv$lang)
    field <- localized_value(row, "field", cv$lang)
    thesis <- localized_value(row, "thesis_title", cv$lang, original_field = "thesis_title_original")
    extras <- nonempty(
      if (nzchar(row_text(row, "grade"))) paste0(t_cv(cv$translations, cv$lang, "grade"), ": ", row_text(row, "grade")) else "",
      if (nzchar(thesis)) paste0(t_cv(cv$translations, cv$lang, "thesis"), ": *", thesis, "*") else "",
      if (nzchar(row_text(row, "supervisor"))) paste0(t_cv(cv$translations, cv$lang, "supervisor"), ": ", row_text(row, "supervisor")) else "",
      if (nzchar(row_text(row, "co_supervisor"))) paste0(t_cv(cv$translations, cv$lang, "co_supervisor"), ": ", row_text(row, "co_supervisor")) else ""
    )
    cv_entry(format_cv_date(row$award_date, cv$lang, cv$translations), paste0(
      "**", degree, if (nzchar(field)) paste0(" in ", field) else "", "**, ", row_text(row, "institution_original"), "  \n",
      paste(extras, collapse = " · ")
    ))
  })
  paste(entries, collapse = "\n")
}

metric_value <- function(cv, metric) {
  qualifier <- metric$qualifier %||% "exact_as_declared"
  key <- paste0("metric_", qualifier)
  value <- t_cv(cv$translations, cv$lang, key, value = metric$value)
  as_of <- format_cv_date(metric$as_of, cv$lang, cv$translations)
  paste0(value, " (", metric$source, "; ", t_cv(cv$translations, cv$lang, "metric_as_of", date = as_of), ")")
}

render_research_profile <- function(cv, mode) {
  profile <- cv$profile
  interests <- vapply(profile$research_interests, function(item) item$label[[cv$lang]], character(1))
  metrics <- c(
    paste0(t_cv(cv$translations, cv$lang, "metric_articles"), ": ", metric_value(cv, profile$metrics$peer_reviewed_articles)),
    paste0(t_cv(cv$translations, cv$lang, "metric_coauthors"), ": ", metric_value(cv, profile$metrics$coauthors))
  )
  blocks <- c(
    paste0("**", t_cv(cv$translations, cv$lang, "research_interests"), ":** ", paste(interests, collapse = "; "), "."),
    paste0("**", t_cv(cv$translations, cv$lang, "source_metrics"), ":** ", paste(metrics, collapse = "; "), ".")
  )
  if (!identical(mode, "compact")) {
    memberships <- vapply(profile$memberships, function(item) paste0(item$role[[cv$lang]], ": ", item$organization_original, " (", format_date_range(item$start_date, "", TRUE, cv$lang, cv$translations), ")"), character(1))
    groups <- vapply(profile$research_groups, function(item) paste0(item$role[[cv$lang]], ": ", markdown_link(item$name_original, item$url), " – ", item$description[[cv$lang]], "."), character(1))
    collaborations <- vapply(profile$collaborations, function(item) item$organization_original, character(1))
    registration <- profile$person$professional_registration
    blocks <- c(blocks,
      paste0("**", t_cv(cv$translations, cv$lang, "collaborations"), ":** ", paste(collaborations, collapse = "; "), "."),
      paste0("**", t_cv(cv$translations, cv$lang, "sections.memberships"), ":** ", paste(memberships, collapse = "; "), "."),
      paste0("**", t_cv(cv$translations, cv$lang, "sections.research_groups"), ":** ", paste(groups, collapse = " ")),
      paste0("**", t_cv(cv$translations, cv$lang, "professional_registration"), ":** ", registration$body_original, ", n. ", registration$registration_number, " (", format_cv_date(registration$since, cv$lang, cv$translations), ").")
    )
  }
  paste(blocks, collapse = "\n\n")
}

render_grants <- function(cv, data) {
  if (!nrow(data)) return("")
  entries <- apply(data, 1L, function(row) {
    row <- as.list(row)
    role_key <- if (row_text(row, "role") == "PI") "principal_investigator" else if (row_text(row, "role") == "Co-PI") "co_principal_investigator" else "team_member"
    amount_key <- paste0("amount_", row_text(row, "amount_scope"))
    amount <- if (nzchar(row_text(row, "amount_eur"))) paste0(t_cv(cv$translations, cv$lang, amount_key), ": ", format_euro(row$amount_eur, cv$lang)) else ""
    date <- format_date_range(row$start_date, row$end_date, row$current, cv$lang, cv$translations)
    codes <- nonempty(if (nzchar(row_text(row, "code"))) paste0("code ", row_text(row, "code")) else "", if (nzchar(row_text(row, "cup"))) paste0("CUP ", row_text(row, "cup")) else "")
    cv_entry(date, paste0(
      "**", row_text(row, "project_title_original"), "**  \n",
      row_text(row, "scheme_original"), " (", row_text(row, "call_year"), ") · ", t_cv(cv$translations, cv$lang, role_key),
      paste(nonempty(paste(codes, collapse = " · "), amount), collapse = " · ")
    ))
  })
  paste(entries, collapse = "\n")
}

render_service <- function(cv, data) {
  if (!nrow(data)) return("")
  entries <- apply(data, 1L, function(row) {
    row <- as.list(row)
    date <- if (nzchar(row_text(row, "event_date"))) format_cv_date(row$event_date, cv$lang, cv$translations) else format_date_range(row$start_date, row$end_date, row$current, cv$lang, cv$translations)
    role <- localized_value(row, "role", cv$lang)
    description <- localized_value(row, "description", cv$lang)
    title <- row_text(row, "title_original")
    if (nzchar(row_text(row, "url"))) title <- markdown_link(title, row_text(row, "url"))
    details <- nonempty(row_text(row, "organization_original"), row_text(row, "location"), description,
      if (nzchar(row_text(row, "count"))) paste0(t_cv(cv$translations, cv$lang, "count"), ": ", row_text(row, "count")) else "")
    cv_entry(date, paste0("**", role, "** – ", title, "  \n", paste(details, collapse = " · ")))
  })
  paste(entries, collapse = "\n")
}

render_publications <- function(cv, data, mode) {
  if (!nrow(data)) return("")
  note <- if (identical(mode, "selected")) paste0("*", t_cv(cv$translations, cv$lang, "selected_publications_note"), ".*\n\n") else ""
  groups <- c("journal_international", "journal_italian", "book_chapter")
  blocks <- character()
  for (group in groups) {
    subset <- data[data$category == group, , drop = FALSE]
    if (!nrow(subset)) next
    title <- t_cv(cv$translations, cv$lang, paste0("sections.", group))
    entries <- apply(subset, 1L, function(row) md_div(format_publication(as.list(row)), "publication"))
    blocks <- c(blocks, paste0("### ", title, "\n\n", paste(entries, collapse = "\n")))
  }
  paste0(note, paste(blocks, collapse = "\n\n"))
}

render_editorial <- function(cv, data, mode) {
  service <- render_service(cv, data)
  journals <- cv$profile$reviewing$journals
  reviewing <- if (identical(mode, "selected") || identical(mode, "compact")) {
    t_cv(cv$translations, cv$lang, "reviewing_summary", journals = length(journals))
  } else {
    paste0("**", t_cv(cv$translations, cv$lang, "reviewer_for"), ":** ", paste(journals, collapse = "; "), ".")
  }
  paste(nonempty(service, reviewing), collapse = "\n\n")
}

render_teaching <- function(cv, data, mode) {
  if (!nrow(data)) return("")
  if (identical(mode, "summary")) {
    hours <- sum(data$total_hours, na.rm = TRUE)
    return(t_cv(cv$translations, cv$lang, "teaching_summary", records = nrow(data), hours = hours))
  }
  categories <- c("phd", "degree", "supplementary", "master")
  blocks <- character()
  for (category in categories) {
    subset <- data[data$category == category, , drop = FALSE]
    if (!nrow(subset)) next
    entries <- apply(subset, 1L, function(row) {
      row <- as.list(row)
      role <- localized_value(row, "role", cv$lang)
      program <- localized_value(row, "program", cv$lang)
      hours <- if (nzchar(row_text(row, "hours_per_year"))) paste0(row_text(row, "hours_per_year"), " ", t_cv(cv$translations, cv$lang, if (length(strsplit(row_text(row, "academic_years"), ";", fixed = TRUE)[[1]]) > 1L) "hours_per_year" else "hours")) else ""
      cv_entry(row_text(row, "academic_years"), paste0(
        "**", role, ": ", row_text(row, "course_title_original"), "**  \n",
        paste(nonempty(program, row_text(row, "institution_original"), hours, localized_value(row, "description", cv$lang)), collapse = " · ")
      ))
    })
    blocks <- c(blocks, paste0("### ", t_cv(cv$translations, cv$lang, paste0("sections.teaching_", category)), "\n\n", paste(entries, collapse = "\n")))
  }
  paste(blocks, collapse = "\n\n")
}

render_supervision <- function(cv, data, mode) {
  counts <- vapply(data, function(item) as.numeric(item$count), numeric(1))
  if (identical(mode, "summary")) return(t_cv(cv$translations, cv$lang, "supervision_summary", records = length(data), people = sum(counts)))
  entries <- vapply(data, function(item) {
    category <- t_cv(cv$translations, cv$lang, paste0("supervision_categories.", item$category))
    level <- t_cv(cv$translations, cv$lang, paste0("supervision_levels.", item$level))
    qualifier <- if (identical(item$qualifier %||% "", "over")) t_cv(cv$translations, cv$lang, "over_prefix") else if (identical(item$qualifier %||% "", "at_least")) t_cv(cv$translations, cv$lang, "at_least_prefix") else ""
    paste0("- **", category, " – ", level, ":** ", qualifier, item$count, ".")
  }, character(1))
  paste(entries, collapse = "\n")
}

presentation_event <- function(event) {
  sub("^(Presentazione (?:orale |su invito )?(?:presso |al |alla |alle )?|Poster presentato (?:presso |al |alla |alle )?|Articolo presentato (?:presso |al |alla |alle )?)", "", event, perl = TRUE)
}

render_presentations <- function(cv, data) {
  if (!nrow(data)) return("")
  years <- unique(substr(data$date, 1L, 4L))
  blocks <- character()
  for (year in years) {
    subset <- data[substr(data$date, 1L, 4L) == year, , drop = FALSE]
    entries <- apply(subset, 1L, function(row) {
      row <- as.list(row)
      authors <- format_person_list(strsplit(row_text(row, "authors"), ";", fixed = TRUE)[[1]])
      event <- presentation_event(row_text(row, "event_original"))
      type <- t_cv(cv$translations, cv$lang, paste0("presentation_types.", row_text(row, "presentation_type")))
      md_div(paste0(authors, " (", format_cv_date(row$date, cv$lang, cv$translations), "). **", row_text(row, "title_original"), ".** ", type, ": ", event, "."), "presentation")
    })
    blocks <- c(blocks, paste0("### ", year, "\n\n", paste(entries, collapse = "\n")))
  }
  paste0(markdown_link(t_cv(cv$translations, cv$lang, "slides_available"), cv$profile$links$slides), "\n\n", paste(blocks, collapse = "\n\n"))
}

render_outreach <- function(cv, data) {
  if (!nrow(data)) return("")
  entries <- apply(data, 1L, function(row) {
    row <- as.list(row)
    date <- if (nzchar(row_text(row, "date"))) format_cv_date(row$date, cv$lang, cv$translations) else format_date_range(row$start_date, row$end_date, row$current, cv$lang, cv$translations)
    role <- localized_value(row, "role", cv$lang)
    title <- row_text(row, "title_original")
    if (nzchar(row_text(row, "url"))) title <- markdown_link(title, row_text(row, "url"))
    cv_entry(date, paste0(
      "**", role, " – ", title, "**  \n",
      paste(nonempty(row_text(row, "organization_original"), row_text(row, "location"), localized_value(row, "description", cv$lang)), collapse = " · ")
    ))
  })
  paste(entries, collapse = "\n")
}

render_skills <- function(cv, data, mode) {
  entries <- vapply(data, function(item) {
    description <- if (identical(mode, "compact")) "" else item$description[[cv$lang]]
    paste0("- **", item$name_original, " – ", item$level[[cv$lang]], ".**", if (nzchar(description)) paste0(" ", description) else "")
  }, character(1))
  paste(entries, collapse = "\n")
}

render_section <- function(cv, key) {
  section <- cv$sections[[key]]
  if (is.null(section) || identical(section$mode, "omit")) return("")
  data <- section$data
  body <- switch(key,
    academic_positions = render_positions(cv, data),
    education = render_education(cv, data),
    research_profile = render_research_profile(cv, section$mode),
    grants = render_grants(cv, data),
    scientific_organization = render_service(cv, data),
    publications = render_publications(cv, data, section$mode),
    editorial = render_editorial(cv, data, section$mode),
    teaching = render_teaching(cv, data, section$mode),
    supervision = render_supervision(cv, data, section$mode),
    institutional_service = render_service(cv, data),
    international_experience = render_positions(cv, data),
    professional_experience = render_positions(cv, data),
    presentations = render_presentations(cv, data),
    outreach = render_outreach(cv, data),
    skills = render_skills(cv, data, section$mode),
    stop("Unknown section: ", key, call. = FALSE)
  )
  section_markdown(cv, key, body)
}

render_footer <- function(cv) {
  paste0("\n[", cv$profile$gdpr[[cv$lang]], "]{.gdpr}\n")
}
