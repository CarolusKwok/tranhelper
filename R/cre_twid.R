#' Find minimum transect width
#'
#' @param T_len Length of transect (in m)
#' @param T_num Number of transects in a group
#' @param S_num Number of sites
#' @param S_wid Width of each site (in m)
#' @param S_len Length of each site (in m)
#' @param S_bor Border of each site (in m)
#' @param flip_x Flips the pattern on the x axis
#' @param flip_y Flips the pattern on the y axis
#' @param shape shape of the site, either "rec" (rectangle) or "ell" (ellipse)
#'
#' @return
#' @export
#'
#' @examples cre_twid()
cre_twid = function(shape = "rec", T_len = 60, T_num = 2, S_num = 18, S_wid = 2, S_len = 2, S_bor = 5, flip_x = F, flip_y = F){
  trh_lib()

  #test if a transect can insert that many sites
  S_num_max = floor((T_len/(S_len + S_bor))+1) * T_num
  message("You can have ", S_num_max, " sites in 1 transect group.")
  if(S_num_max < S_num){
    message("The input is not possible!")
    return("---Calculation Failed---")
  }


  df = cre_obj(T_Num = T_num, S_Num = S_num)
  df = df$data
  df$S_y = (T_len / (S_num-1)) * (df$S_num-1)







  lookup = df %>%
    filter(T_pair == 1) %>%
    distinct()
                             #        x         y
  temp_df1 = data.frame(BL = c(-S_wid/2, -S_len/2),
                        C = c(        0,        0),
                        TR = c(+S_wid/2, +S_len/2))
  temp_df2 = data.frame(BL = c(0, lookup$S_y[2] - S_len/2),
                         C = c(0, lookup$S_y[2]),
                        TR = c(0, lookup$S_y[2] + S_len/2))
  #start drawing! (rectangle)
  if(shape == "rec"){
    #equation of circle (x-x0)^2 + (y-y0)^2 = r^2
    temp_df2$BL[1] = tryCatch({sqrt((S_bor+temp_df2$BL[2]-temp_df1$TR[2])*(S_bor-temp_df2$BL[2]+temp_df1$TR[2]))+temp_df1$TR[1]},
                              warning = function(w){0-S_wid/2})
    temp_df2$TR[1] = temp_df2$BL[1] + S_wid
    temp_df2$ C[1] = temp_df2$BL[1] + S_wid/2

    S_x_dither = temp_df2$C[1] - temp_df1$C[1]
    df$S_x = S_x_dither*(as.numeric(df$T_num)-1)
  }

  #start drawing (ellipse)
  if(shape == "ell"){
    #equation of ellipse ((x^2 - x0) / a2) + ((y^2 - y0) / b2) = 1
    #a = S_wid / 2
    #b = S_len / 2




  }

  #fix starting point at 0,0
  Max_T_num = as.numeric(max(df$T_num))
  if(Max_T_num %% 2 == 1){
    S_x_dither = median(1:Max_T_num)
    S_x_dither = as.numeric(df$S_x[S_x_dither])
  }
  if(Max_T_num %% 2 == 0){
    S_x_dither = as.numeric(max(as.numeric(df$S_x)))/2
  }

  df = df %>%
    mutate(Type = "Site", S_y = as.numeric(S_y), S_x = as.numeric(S_x)) %>%
    mutate(S_x = S_x - S_x_dither) %>%
    relocate(Type, .before = S_num) %>%
    relocate(S_x, .before = S_y)

  if(flip_x == T){
    df$S_x = -df$S_x
  }
  if(flip_y == T){
    df = df %>%
      mutate(S_y = -S_y) %>%
      mutate(S_y = -S_y + abs(min(S_y)))
  }
  TInfo = data.frame(Type   = c("Ref_SP", "Ref_EP"),
                     S_num  = NA,
                     T_num  = NA,
                     T_pair = NA,
                     S_x    = 0,
                     S_y    = c(0, max(df$S_y)))
  df = bind_rows(TInfo, df) %>%
    mutate(S_dis = sqrt(S_x^2 + S_y^2)) %>%
    mutate(S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA))))

  list(data = df,
       detail = data.frame("service" = "cre_twid",
                           "shape" = shape,
                           "T_len" = T_len,
                           "T_num" = T_num,
                           "S_num" = S_num,
                           "S_wid" = S_wid,
                           "S_len" = S_len,
                           "S_bor" = S_bor))
}
