# This file is named zzz by convention
# Handles package load tasks and cleanup after unload
# Package on load
.onLoad <- function(libname, pkgname) {
}

.onUnLoad <- function(libname, pkgname) {
  if (exists(ari.env)) rm(ari.env)
}
