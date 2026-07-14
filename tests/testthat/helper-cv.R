test_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(test_root, "R", "utils.R"))
source(file.path(test_root, "R", "bibliography.R"))
source(file.path(test_root, "R", "load_cv.R"))
source(file.path(test_root, "R", "select_cv.R"))
source(file.path(test_root, "R", "validate_cv.R"))

translations <- list(
  it = yaml::read_yaml(file.path(test_root, "i18n", "it.yml")),
  en = yaml::read_yaml(file.path(test_root, "i18n", "en.yml"))
)
