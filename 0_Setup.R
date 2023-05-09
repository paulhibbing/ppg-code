## NOTE: The list of CRAN packages may need to be expanded, to account for
## downstream dependencies of what's immediately required in this analysis. The
## most trouble is likely to occur with the packages being installed from
## GitHub. I've set `dependencies = FALSE` and `force = TRUE`, because in my
## experience GitHub installation is finicky and the downstream dependencies are
## likely unnecessary in the functions we'll actually be calling from those
## packages. Nevertheless, it's important to note that this may be an issue, so
## that that code in this repository can be updated as necessary to help it work
## on everyone's machines.

# Clear memory ------------------------------------------------------------


  rm(list = ls())


# Set up directory structure ----------------------------------------------


  folders <- c(
    "data-raw",
    "data-raw/physionet_wrist",
    "data-raw/physionet_wrist/rds",
    "data-raw/8s_epochs"
  )
  
  for (folder in folders) {
    if (!dir.exists(folder)) dir.create(folder)
  }


# Package installation function -------------------------------------------

  
  installer <- function(
    package_name,
    method = c("cran", "github"),
    ...
  ) {

    if (!basename(package_name) %in% installed.packages()) {
      
      method <- match.arg(method)
      
      switch(
        method,
        "cran" = install.packages(package_name, ...),
        "github" = remotes::install_github(package_name, ...),
        stop("Error selecting an installation procedure")
      )
      
    }
    
    invisible()
    
  }


# Package installations ---------------------------------------------------

  
  #* CRAN ####
  
    cran_packages <- c(
      "caret", "cowplot", "devtools", "dplyr", "flextable", "ggpmisc",
      "gsignal", "gtsummary", "janitor", "magrittr", "nnet", "officer",
      "purrr", "remotes", "reshape2", "rstudioapi", "tibble", "tidyr",
      "tuneR"
    )
    
    sapply(cran_packages, installer)
    
    
  #* GitHub ####
  
    github_packages <- c(
      "paulhibbing/AGread",
      "paulhibbing/PAutilities",
      "paulhibbing/wfdb"
    )
    
    sapply(
      github_packages, installer,
      method = "github", dependencies = FALSE,
      force = TRUE
    )
    