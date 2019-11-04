# Package itself with all dependencies (including suggested packages)
install.packages("devtools")
install.packages("tidyr") # manual install to get newer version
devtools::install_github("isoverse/isoreader", ref = "binder", dependencies = TRUE)
devtools::install_github("isoverse/isoprocessor", ref = "binder", dependencies = TRUE)

# Packages for knitting
install.packages(c("rmarkdown", "caTools", "bitops"))
