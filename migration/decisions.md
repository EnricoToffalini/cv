# Migration decisions

## Canonical source and generated files

- The root PDF remains untouched. `source/CV_2026_Toffalini.pdf` is a byte-identical stable copy; both hashes are recorded in `source_manifest.yml`.
- The nine canonical data files are `profile.yml`, seven CSV files, and `publications.bib`. `cv.qmd` contains only document structure and rendering calls.
- `dist/` is generated and ignored by Git. CI rebuilds the eight CV variants and exports.

## Data modelling

- The two type-A and two type-B research fellowships remain two aggregate records because the source presents each pair as one period. The two type-B project descriptions remain structured in the record.
- The two non-contiguous City University visits were split into two records so the date model does not imply continuous residence.
- Reviewing journals, research groups, memberships, metrics, supervision counts and skills live in `profile.yml`; they are small hierarchical collections rather than artificial tables.
- The three source bullets on student supervision were decomposed into nine count-bearing records. Counts are not deduplicated because the source does not establish whether people overlap.
- The declared metric “over 100 peer-reviewed articles” is distinct from the 104 article records currently present in BibTeX. It retains its Scopus source and February 2026 reference date.

## Bibliography and verification

- Citekeys are stable and generated once by the migration script. Crossref never overwrites the bibliography wholesale.
- Crossref was checked on 2026-07-14. Ninety-four DOI records were verified; eleven DOI records were unavailable through Crossref; two chapters have no DOI in the source.
- Corrections supported by Crossref are recorded individually in `issues.csv`. Source-year values were retained where current Crossref issue years differ from the February 2026 CV.
- Publication and presentation titles remain in their original language. The English CV translates labels and descriptions, not official titles.

## Full and short variants

- Full includes every migrated record.
- Short omits presentations, summarizes teaching and supervision, compacts the research profile and skills, and uses `include_short`/`cv-short` for selected sections.
- Six publications are initially marked `cv-short`: recent Toffalini-led work, the PECANS statement, and the 2024 Italian article. This editorial choice is intentionally easy to revise in `publications.bib`.

## Toolchain

- R 4.5.1 is used because it already contains the required packages. The registry-default R 4.3.2 lacks `knitr` and `rmarkdown`.
- The render wrapper supplies Quarto with R 4.5.1 and a short, project-local R home. This avoids a machine-specific invalid multibyte OneDrive home path while retaining installed library paths.
- LuaLaTeX is used for PDF. HTML and PDF are rendered from the same Markdown-producing R functions.
