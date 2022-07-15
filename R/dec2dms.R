#' Title NOT COMPLETED
#'
#' @param brng
#'
#' @return
#' @export
#'
#' @examples dec2dms(999999)
dec2dms_lon = function(brng){
  brng_ex = floor(brng / 360)
  brng = brng - brng_ex * 360
  brng
}
