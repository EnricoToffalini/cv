testthat::test_that("a small BibTeX fixture renders", {
  publications <- read_publications(file.path(test_root, "tests", "fixtures", "sample.bib"))
  testthat::expect_equal(nrow(publications), 1)
  rendered <- format_publication(as.list(publications[1, ]))
  testthat::expect_match(rendered, "\\*\\*E[.] Toffalini\\*\\*")
  testthat::expect_match(rendered, "https://doi.org/10.1234/example")
})

testthat::test_that("open-science badges use one shared specification", {
  specs <- open_science_badge_specs()
  testthat::expect_named(specs, c("preregistered", "open_data", "open_materials", "open_code"))
  testthat::expect_true(all(vapply(specs, function(spec) file.exists(file.path(test_root, "imgs", spec$image)), logical(1))))
})
