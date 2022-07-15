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
#' @examples find_loc(df, SP = c(10, 10))
find_loc = function(df, SP = NA, BRNG = NA, EP = NA){
  #SP = c(lat, lon)
  if(is.na(BRNG)){
    if(!is.na(EP[1]) & !is.na(EP[2])){
      BRNG = vic_finddis(p1 = SP, p2 = EP)[2]
    }
  }

  df = df %>%
    mutate(S_x = as.numeric(S_x), S_y = as.numeric(S_y))

  Min_x = min(df$S_x)
  Max_x = max(df$S_x)
  L_x = abs(Min_x - Max_x)

  df = df %>%
    mutate(Type = "Site") %>%
    relocate(Type, .before = S_num) %>%
    mutate(S_x = S_x - L_x/2) %>%
    mutate(S_dis = sqrt((S_y)^2 + (S_x)^2),
           S_ang = rad2deg(atan(abs(S_y)/abs(S_x))),
           S_lon = 0,
           S_lat = 0) %>%
    mutate(S_ang = ifelse(S_x < 0, S_ang + 270, ifelse(S_x > 0, 90 - S_ang, ifelse(S_x == 0, 90, NA)))) %>%
    mutate(S_ang = S_ang + BRNG)
  for(i in 1:nrow(df)){
    df$S_lat[i] = suppressWarnings(vic_findloc(SP, df$S_ang[i], df$S_dis[i]))[1]
    df$S_lon[i] = suppressWarnings(vic_findloc(SP, df$S_ang[i], df$S_dis[i]))[2]
  }

  Min_y = min(df$S_y)
  Max_y = max(df$S_y)
  L_y = ifelse(abs(Min_y) > abs(Max_y), Min_y, Max_y)

  TInfo = data.frame(Type   = c("Ref-SP", "Ref-EP"),
                     S_num  = c("SP" ,     "EP"),
                     T_num  = c(  NA ,       NA),
                     T_pair = c(  NA ,       NA),
                     S_y    = c(   0 ,      L_y),
                     S_x    = c(   0 ,        0),
                     S_dis  = c(  NA , abs(L_y)),
                     S_ang  = c(  NA ,     BRNG)) %>%
    mutate(S_lon  = c(SP[2], vic_findloc(SP, BRNG, L_y)[2]),
           S_lat  = c(SP[1], vic_findloc(SP, BRNG, L_y)[1]),
           S_num = as.factor(S_num))

  df = df %>%
    mutate(S_num = as.factor(S_num))
  df = bind_rows(TInfo, df) %>%
    relocate(S_lat, .before = S_lon) %>%
    relocate(S_x, .before = S_y)
  df
}
