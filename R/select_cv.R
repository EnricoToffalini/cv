select_by_mode <- function(data, mode) {
  if (identical(mode, "omit")) return(data[0, , drop = FALSE])
  if (identical(mode, "selected") && "include_short" %in% names(data)) data <- data[as_cv_flag(data$include_short), , drop = FALSE]
  sort_records(data)
}

select_cv <- function(raw, lang = "it", level = "full") {
  if (!lang %in% raw$allowed$lang) stop("Invalid lang '", lang, "'. Allowed: ", paste(raw$allowed$lang, collapse = ", "), call. = FALSE)
  if (!level %in% raw$allowed$level) stop("Invalid level '", level, "'. Allowed: ", paste(raw$allowed$level, collapse = ", "), call. = FALSE)
  modes <- raw$variants[[level]]
  order <- raw$variants$section_order
  unknown <- setdiff(unlist(modes, use.names = FALSE), raw$variants$allowed_modes)
  if (length(unknown)) stop("Invalid section mode(s): ", paste(unique(unknown), collapse = ", "), call. = FALSE)

  section_data <- list(
    academic_positions = raw$positions[raw$positions$section == "academic", , drop = FALSE],
    education = raw$education,
    research_profile = raw$profile,
    grants = raw$grants,
    scientific_organization = raw$service[raw$service$subsection == "scientific_organization", , drop = FALSE],
    publications = raw$publications,
    editorial = raw$service[raw$service$subsection == "editorial", , drop = FALSE],
    teaching = raw$teaching,
    supervision = raw$profile$supervision,
    institutional_service = raw$service[raw$service$subsection == "institutional_service", , drop = FALSE],
    international_experience = raw$positions[raw$positions$section == "international_experience", , drop = FALSE],
    professional_experience = raw$positions[raw$positions$section == "professional_experience", , drop = FALSE],
    presentations = raw$presentations,
    outreach = raw$outreach,
    skills = raw$profile$skills
  )

  sections <- list()
  for (name in order) {
    data <- section_data[[name]]
    mode <- modes[[name]]
    if (is.data.frame(data)) data <- select_by_mode(data, mode)
    sections[[name]] <- list(mode = mode, data = data)
  }

  structure(list(
    schema_version = raw$schema_version,
    lang = lang,
    level = level,
    profile = raw$profile,
    translations = raw$translations,
    section_order = order,
    sections = sections
  ), class = "cv_data")
}
