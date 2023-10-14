#' Find geoloction of transect
#'
#' @param df Dataframe, that consist of results from find_twidth or find_theight
#' @param SP Starting Point, in the format of c(Lat, Lon)
#' @param BRNG Bearing of SP to EP, only accepts numeric values
#' @param EP Ending Point (expected), in the format of c(Lat, Lon). Ignored if BRNG is not NA.
#'
#' @return
#' @export
#'
#' @examples trs_geo(df, SP = c(10, 10))
trs_geo = function(df, SP, BRNG, EP){
  trh_lib()
  #SP = c(lat, lon)
  if(is.list(df)){
    detail = df$detail
    df = df$data
  }
  if(hasArg(BRNG)){
    if(hasArg(EP)){
      if(is.numeric(EP[1]) & is.numeric(EP[2])){
        BRNG = vic_finddis(p1 = SP, p2 = EP)[2]
      }
      else {
        message("EP is incorrect!")
      }
    }
  }

  df = df %>%
    mutate(S_x = as.numeric(S_x), S_y = as.numeric(S_y)) %>%
    mutate(S_lat = 0,
           S_lon = 0) %>%
    mutate(S_brn = S_brn + BRNG)
  for(i in 1:nrow(df)){
    df$S_lat[i] = suppressWarnings(vic_findloc(SP, df$S_brn[i], df$S_dis[i]))[1]
    df$S_lon[i] = suppressWarnings(vic_findloc(SP, df$S_brn[i], df$S_dis[i]))[2]
  }
  if(exists("detail")){
    df = list(data = df,
              detail = detail)
    df$detail$service = paste0(df$detail$service, " + geo")
  }
  df
}
