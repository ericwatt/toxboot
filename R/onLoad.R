.onLoad <- function(libname, pkgname) {

  pkgd <- system.file(package = "toxboot")
  conf_file <- file.path(pkgd, "toxboot.config")

  if (file.exists(conf_file)) {

    toxbootConfLoad()

  } else {

    toxbootConfReset()
    toxbootConfLoad()

  }

}
