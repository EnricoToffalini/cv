# Reconciliation with the source PDF

Reconciliation date: 2026-07-14. Source: 19-page `_CV_2026_Toffalini.pdf`, SHA-256 recorded in `source_manifest.yml`.

The Italian full PDF was converted back to UTF-8 text in `rendered-it-full.txt`. `check_reconciliation.R` normalizes typography and checks every canonical publication title and DOI, every presentation title, and the identifying title/role of all other repeated records. The final run found **320 of 320** canonical values.

## Section reconciliation

| Source section | Pages | Source items | Structured records | Rendered it-full | Discrepancies / mapping | Status |
|---|---:|---:|---:|---:|---|---|
| Personal data | 1 | 1 | 1 | 1 | Header plus professional registration | Reconciled |
| Academic positions | 1 | 5 | 5 | 5 | Two fellowship pairs remain aggregate records | Reconciled |
| Education | 1-2 | 3 | 3 | 3 | None | Reconciled |
| Research interests, collaborations, affiliations | 2 | 4 bullets | 17 | 17 | Decomposed into interests, collaborations, memberships and metrics | Reconciled |
| Active research groups | 2 | 2 | 2 | 2 | None | Reconciled |
| Funded projects | 2-3 | 4 | 4 | 4 | Total versus department contributions kept distinct | Reconciled |
| Scientific organization | 3 | 5 | 5 | 5 | None | Reconciled |
| International journal articles | 3-10 | 93 | 93 | 93 | Crossref-supported corrections listed in `issues.csv` | Reconciled |
| Italian journal articles | 10-11 | 11 | 11 | 11 | One author list remains abbreviated and explicitly open | Reconciled with issue |
| Book chapters | 11 | 3 | 3 | 3 | Two source entries have no DOI; one publisher spelling needs review | Reconciled with issues |
| Editorial and reviewing | 12 | 4 bullets | 3 service + 18 journals | 3 service + journal list | Reviewer bullet decomposed into a journal list | Reconciled |
| Teaching | 12-13 | 14 | 14 | 14 | Repeated academic years stay in one record; `2024/2005` preserved | Reconciled with issue |
| Supervision and tutoring | 13 | 3 bullets | 9 | 9 | Aggregate counts decomposed without deduplicating people | Reconciled |
| Institutional service and committees | 13-14 | 11 | 11 | 11 | Fifteen Master’s committees stored as one count-bearing record with dates | Reconciled |
| International experience | 14 | 2 | 3 | 3 | Two non-contiguous City University visits split | Reconciled |
| Clinical and professional experience | 14 | 3 | 3 | 3 | None | Reconciled |
| Presentations and symposia | 14-18 | 43 | 43 | 43 | `3th` source wording preserved and flagged | Reconciled with issue |
| Public engagement and knowledge transfer | 18-19 | 10 | 10 | 10 | Repeated dates aggregated as in source | Reconciled |
| Research computing skills | 19 | 4 | 4 | 4 | None | Reconciled |
| Update date and GDPR | 19 | 2 | 2 | 2 | Localized in both variants | Reconciled |

## Publications by year and category

| Year | International articles | Italian articles | Chapters | Total |
|---:|---:|---:|---:|---:|
| 2026 | 1 | 0 | 0 | 1 |
| 2025 | 6 | 0 | 0 | 6 |
| 2024 | 9 | 1 | 0 | 10 |
| 2023 | 9 | 0 | 0 | 9 |
| 2022 | 11 | 2 | 0 | 13 |
| 2021 | 15 | 0 | 1 | 16 |
| 2020 | 10 | 2 | 0 | 12 |
| 2019 | 7 | 3 | 2 | 12 |
| 2018 | 7 | 1 | 0 | 8 |
| 2017 | 11 | 2 | 0 | 13 |
| 2016 | 2 | 0 | 0 | 2 |
| 2015 | 3 | 0 | 0 | 3 |
| 2014 | 2 | 0 | 0 | 2 |
| **Total** | **93** | **11** | **3** | **107** |

The source’s aggregate “over 100 peer-reviewed articles” is a Scopus metric as of February 2026. It is not replaced by the derived BibTeX count of 104 journal articles.

## Presentations by year

| Year | Count | Year | Count |
|---:|---:|---:|---:|
| 2025 | 2 | 2018 | 7 |
| 2024 | 2 | 2017 | 5 |
| 2023 | 2 | 2016 | 7 |
| 2022 | 6 | 2015 | 2 |
| 2021 | 2 | 2014 | 2 |
| 2020 | 1 | 2013 | 2 |
| 2019 | 2 | 2012 | 1 |
| **Total** | **43** |  |  |

## Rendered outputs

| Variant | HTML | PDF pages | Text extractable | Essential sections |
|---|---|---:|---|---|
| Italian full | Generated | 16 | Yes | Present |
| Italian short | Generated | 4 | Yes | Present |
| English full | Generated | Pending final recount | Yes | Present |
| English short | Generated | 4 | Yes | Present |

Open factual questions are not hidden: see `issues.csv`. The unresolved items do not correspond to silently omitted records.
