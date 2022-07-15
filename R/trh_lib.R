#' Loads all library into R
#'
#' @return
#' @export
#'
#' @examples trh_lib()
trh_lib = function(){
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(svglite))
}
