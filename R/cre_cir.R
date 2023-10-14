#' Hi
#'
#' @param T_num hi
#' @param S_num hi
#' @param S_len hi
#' @param angle hi
#'
#' @return
#' @export
#'
#' @examples cre_cir()
cre_cir = function(T_num = 5, S_num = 25, S_len = 2, angle = 360){
  trh_lib()

  df = cre_obj(T_Num = T_num, S_Num = S_num)
  df = df$data
  angle_dit = 360/(T_num)

  #formula x1 = cos(x) * y
  #formula y1 = sin(x) * y
  #remember to add 90


  df = df %>%
    mutate(S_x_cir = angle_dit*(T_num - 1),
           S_y_cir = S_len * T_pair)

  median = median(df$S_x_cir)
  df = df %>%
    mutate(S_x_cir = S_x_cir - median)
  #angle compression based on angle
  range = max(df$S_x_cir) - min(df$S_x_cir)
  if(angle > range & angle != 360){
    message("Angle is too large, which will create an angle between 2 of the transects compress")
    return(message("ABORT"))
  }

  if(angle != 360){
    factor = angle / range
    df = df %>%
      mutate(S_x_cir = (S_x_cir * factor))
  }

  #polar conversion and distance bearing calculation
  df = df %>%
    mutate(S_x_cir = S_x_cir + 90) %>%
    mutate(S_x = cos(deg2rad(S_x_cir)) * S_y_cir,
           S_y = sin(deg2rad(S_x_cir)) * S_y_cir) %>%
    mutate(S_x_cir = S_x_cir - 90)

  #perparing to export!
  df = df %>%
    mutate(Type = "Site") %>%
    relocate(Type, .before = everything())
  TInfo = data.frame(Type   = c("Ref_SP", "Ref_EP"),
                     S_num  = NA,
                     T_num  = NA,
                     T_pair = NA,
                     S_x    = 0,
                     S_y    = c(0, max(df$S_y)))
  df = bind_rows(TInfo, df) %>%
    mutate(S_dis = sqrt(S_x^2 + S_y^2),
           S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA)))) %>%
    relocate(S_dis, S_brn, S_x_cir, S_y_cir, .after = everything())

  list(data = df,
       detail = data.frame(service = "cre_cir",
                           T_num = T_num,
                           S_num = S_num,
                           S_len = S_len,
                           angle = angle))
}
