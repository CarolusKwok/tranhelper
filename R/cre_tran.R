#' Title Hi
#'
#' @param T_len Hi
#' @param T_wid Hi
#' @param S_num Hi
#'
#' @return
#' @export
#'
#' @examples cre_tran()
cre_tran = function(T_len = 60, T_wid = 7, S_num = 18, seed){
  trh_lib()

  if(!hasArg(seed)){
    seed = sample(-2^30:2^30,1)
  }
  set.seed(seed)

  df = cre_obj(T_Num = 1, S_Num = S_num)
  df = df$data %>%
    mutate(S_x = runif(S_num, 0 + 0.0000000000000000000000001, T_wid + 0.0000000000000000000000001),
           S_y = runif(S_num, 0 + 0.0000000000000000000000001, T_len + 0.0000000000000000000000001)) %>%
    mutate(S_x = S_x - T_wid/2) %>%
    mutate(Type = "Site")

  TInfo = data.frame(Type   = c("Ref_SP", "Ref_EP"),
                     S_num  = NA,
                     T_num  = NA,
                     T_pair = NA,
                     S_x    = 0,
                     S_y    = c(0, T_len))

  df = bind_rows(TInfo, df) %>%
    mutate(S_dis = sqrt(S_x^2 + S_y^2),
           S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA)))) %>%
    relocate(S_dis, S_brn, .after = everything())

  list(data = df,
       detail = data.frame(service = "cre_cran",
                           T_num = 1,
                           T_len = T_len,
                           T_wid = T_wid,
                           S_num = S_num,
                           seed = seed))
}
