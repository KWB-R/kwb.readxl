author <- list(
  name = "Hauke Sonnenberg", 
  email = "hauke.sonnenberg@kompetenz-wasser.de",
  orcid = "0000-0001-9134-2871",
  url = "https://github.com/hsonne"
)

pkg <- list(
  name = "kwb.readxl", 
  title = "Read Data From Excel Files", 
  desc  = paste(
    "This package is based on the package readxl. It provides functions that",
    "read all Excel sheets as pure text and then try to split each sheet into",
    "a set of line ranges that are assumed to represent single tables."
  )
)

kwb.pkgbuild::use_pkg(
  author, 
  pkg, 
  version = "0.1.0.9000",
  stage = "experimental"
)
