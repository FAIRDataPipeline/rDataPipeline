context("Testing upload_paper")

# get the token
token <- Sys.getenv("SCRC_API_TOKEN")

doi <- "10.24272/j.issn.2095-8137.2020.053"
release_date <- "2020-01-01T12 <-00 <-00Z"
title <- "COVID-19-like symptoms observed in Chinese tree shrews infected with SARS-CoV-2"
abstract <- "The coronavirus disease 2019 (COVID-19) pandemic continues to pose a global threat to the human population. Identifying animal species susceptible to infection with the SARS-CoV-2/HCoV-19 pathogen is essential for controlling the outbreak and for testing valid prophylactics or therapeutics based on animal model studies. Here, different aged Chinese tree shrews (adult group, 1 year old; old group, 5-6 years old), which are close relatives to primates, were infected with SARS-CoV-2. X-ray, viral shedding, laboratory, and histological analyses were performed on different days post-inoculation (dpi). Results showed that Chinese tree shrews could be infected by SARS-CoV-2. Lung infiltrates were visible in X-ray radiographs in most infected animals. Viral RNA was consistently detected in lung tissues from infected animals at 3, 5, and 7 dpi, along with alterations in related parameters from routine blood tests and serum biochemistry, including increased levels of aspartate aminotransferase (AST) and blood urea nitrogen (BUN). Histological analysis of lung tissues from animals at 3 dpi (adult group) and 7 dpi (old group) showed thickened alveolar septa and interstitial hemorrhage. Several differences were found between the two different aged groups in regard to viral shedding peak. Our results indicate that Chinese tree shrews have the potential to be used as animal models for SARS-CoV-2 infection."
journal_name <- "Zoological research"
journal_abr <- "Zool Res"
keywords <- "tree shrews"
website <- ""
authors <- "Yong-Tang Zheng"

test_that("Existing Paper returns existing paper", {
  expect_message(upload_paper(title, authors, journal_name, journal_abr, website, release_date, abstract, keywords, doi, key = token))
  expect_true(is.character(upload_paper(title, authors, journal_name, journal_abr, website, release_date, abstract, keywords, doi, key = token)))
})
