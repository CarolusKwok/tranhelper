#' Loads all library into R
#'
#' @return
#' @export
#'
#' @examples trh_lib()
trh_lib = function(){
  suppressPackageStartupMessages(library(scales))
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(svglite))
  suppressPackageStartupMessages(library(rnaturalearth))
}
