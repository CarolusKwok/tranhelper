#' Hi
#'
#' @param T_len Hi
#' @param S_num HI
#'
#' @return
#' @export
#'
#' @examples cre_cran()
cre_cran = function(T_len = 2, T_wid = 2, S_num = 18, angle = 360, seed){
  trh_lib()

  if(!hasArg(seed)){
    seed = sample(-2^30:2^30,1)
  }
  set.seed(seed)

  df = cre_obj(T_Num = 1, S_Num = S_num)
  df = df$data %>%
    mutate(S_x_cir = runif(S_num, 0, angle + 0.0000000000000000000000001),
           S_y_cir = runif(S_num, 0, T_len + 0.0000000000000000000000001)) %>%
    mutate(S_x_cir = S_x_cir + ((180 - angle) / 2)) %>%
    mutate(S_x = cos(deg2rad(S_x_cir)) * S_y_cir,
           S_y = sin(deg2rad(S_x_cir)) * S_y_cir) %>%
    mutate(S_x = S_x * T_wid / T_len) %>% #compresses it into an ellipse if neccessary
    mutate(S_x_cir = rad2deg(atan(S_y/S_x)),
           S_y_cir = sqrt(S_x^2 + S_y^2)) %>%
    mutate(S_x_cir = S_x_cir - ((180 - angle) / 2)) %>%
    mutate(Type = "Site")

  #TInfo
  TInfo = data.frame(Type   = c("Ref_SP", "Ref_EP"),
                     S_num  = NA,
                     T_num  = NA,
                     T_pair = NA,
                     S_x    = 0,
                     S_y    = c(0, T_len))

  df = bind_rows(TInfo, df) %>%
    mutate(S_dis = S_y_cir,
           S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA)))) %>%
    relocate(S_dis, S_brn, S_x_cir, S_y_cir, .after = everything())

  list(data = df,
       detail = data.frame(service = "cre_cran",
                           T_num = 1,
                           S_num = S_num,
                           angle = angle,
                           seed = seed))
}
