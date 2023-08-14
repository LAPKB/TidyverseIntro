
#' Open PDF in tutorial data folder.
#'
#' Opens a pdf file shippped with a given tutorial.
#'
#' @param file The filename of the pdf.
#'
#' @export
pkgPDF <- function(file) {

  system(paste0("open data/",file))
}




#' Launch tutorials in the package
#'
#' Provides a list of current tutorials and asks user which one to launch.
#'
#' @param browser Launch in browser, rather than Rstudio window. Default is `TRUE`.
#'
#' @export
#'
launch <- function(browser = TRUE){
  pwd <- tryCatch(keyring::key_get("Launcher"), error = function(e) NULL)
  while(is.null(pwd)){
    pwd <- readline("Enter password: ")
    if(pwd != keyring::key_get("Launcher_pwd")){
      cat(crayon::red("Password incorrect.\n"))
      pwd <- NULL
    } else {
      keyring::key_set_with_value("Launcher", password = pwd)
    }
  }
  avail <- learnr::available_tutorials("TidyverseIntro")
  n_tut <- length(avail$name)
  cat(paste0("The following options are available.\n\n"))
  purrr::walk(1:n_tut,\(x){
    cat(paste0(x,". ",crayon::blue(avail$title[x]),"\n"))
  })
  cat(paste0(n_tut+1,". ",crayon::red("None")," - abort.\n"))
  cat("\n")
  ans <- as.numeric(readline("Which option would you like? "))
  if(ans<=n_tut){
    #options(shiny.launch.browser = browser)
    learnr::run_tutorial(avail$name[ans], "TidyverseIntro",
                         # shiny_args = list(launch.browser = TRUE),
                         as_rstudio_job = browser)
  } else {
    return(invisible(NULL))
  }

}

#' Clean text
#'
#' Remove spaces, leading zeros, trailing zeros after decimals and numbers and trailing zeros after decimals
#' @param x the string to process
#' @export
tidyup <- function(x){
  x2 <- stringr::str_replace_all(x, "\\s+", "") %>% #remove spaces
    stringr::str_replace_all("0+(\\d*\\.)","\\1") %>% #remove leading zeros
    stringr::str_replace_all("(\\.0*[1-9]+)0+","\\1") %>% #remove trailing zeros after numbers
    stringr::str_replace_all("(\\d+)\\.0+([^1-9])+","\\1\\2") #remove trailing zeros after decimal
  return(x2)
}

