#.libPaths( c( "R/libs", .libPaths()) )
dir.create(Sys.getenv(c('R_LIBS_USER')), recursive=TRUE)
.libPaths( c( Sys.getenv(c('R_LIBS_USER')) , .libPaths()) )

options(renv.consent = TRUE)

# Install renv (if it is needed!)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Enable this only when BioConductor packages are involved
#renv::restore(packages = c('BiocManager'), prompt = FALSE)
#renv::restore(repos = BiocManager::repositories(), prompt = FALSE)

# Comment this only when BioConductor packages are involved
renv::restore(prompt = FALSE)
renv::isolate()
