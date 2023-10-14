#' Find X, Y coordinate after rotation
#'
#' Rotates around 0,0 for tranhelper object
#'
#' @param obj tranhelper list object
#' @param rot rotation angle (in deg)
#'
#' @return
#' @export
#'
#' @examples trs_rot(cre_twid(), 90)
trs_rot = function(obj, rot){
  trh_lib()
  if(is.list(obj)){
    detail = obj$detail
    df = obj$data
  }

  #rotation formula
  #x = x0 * cos - y0 * sin
  #y = x0 * sin + y0 * cos
  rot = deg2rad(rot + 180)
  df = df %>%
    mutate(S_x_new = S_x * cos(rot) - S_y * sin(rot)) %>%
    mutate(S_y_new = S_x * sin(rot) + S_y * cos(rot)) %>%
    #mutate(S_x = S_x_new) %>%
    #mutate(S_y = S_y_new) %>%
    mutate(S_x = ifelse(log(abs(S_x_new), 10) <= -10, 0, S_x_new))%>%
    mutate(S_y = ifelse(log(abs(S_y_new), 10) <= -10, 0, S_y_new))%>%
    select(-S_x_new, -S_y_new) %>%
    mutate(S_dis = sqrt(S_x^2 + S_y^2)) %>%
    mutate(S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA))))

  if(grepl("geo",detail$service,fixed = T)){
    for(i in 1:nrow(df)){
      df$S_lat[i] = suppressWarnings(vic_findloc(c(df$S_lat[1], df$S_lon[1]), df$S_brn[i], df$S_dis[i]))[1]
      df$S_lon[i] = suppressWarnings(vic_findloc(c(df$S_lat[1], df$S_lon[1]), df$S_brn[i], df$S_dis[i]))[2]
    }
  }

  list(data = df, detail = detail)
}
