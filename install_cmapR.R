## Installing known dependencies: data.table & rhdf5
install.packages("data.table")

install.packages("BiocManager") # to install rhdf5
BiocManager::install(c("rhdf5", "prada"))

install.packages("devtools")    # to install cmapR from this GitHub repo

## Installing cmapR
devtools::install_github("cmap/cmapR", dependencies=T, force=T)
