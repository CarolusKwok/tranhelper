#' Find minimum transect width
#'
#' @param Shape Shape of the site, either "rec" (rectangle) or "cir" (circle)
#' @param T_Len Length of transect (in m)
#' @param T_num Number of transects in a group
#' @param S_num Number of sites
#' @param S_Wid Width of each site (in m)
#' @param S_Len Length of each site (in m)
#' @param S_Bor Border of each site (in m)
#'
#' @return
#' @export
#'
#' @examples find_twidth()
find_twidth = function(Shape = "rec", T_Len = 60, T_num = 2, S_num = 18, S_Wid = 2, S_Len = 2, S_Bor = 5){
  #test if a transect can insert that many sites
  if(Shape == "rec"){
    S_num_max = floor((T_Len/(S_Len + S_Bor))+1) * T_num
    message("You can only have ", S_num_max, " sites in 1 transect group.")
    if(S_num_max < S_num){
      message("The input is not possible!")
      return("---Calculation Failed---")
    }
    df = data.frame(S_num = 1:S_num)
    df$T_num = ((df$S_num-1) %% T_num) + 1
    df$T_pair = ceiling(df$S_num / T_num)
    df$S_y = (T_Len / (S_num-1)) * (df$S_num-1)
    df$S_x = NA
    temp_df1 = data.frame(Cor= c("x", "y"),
                          C  = c(         0,        0) ,
                          TL = c(-(S_Wid/2),+(S_Len/2)),
                          TR = c(+(S_Wid/2),+(S_Len/2)),
                          BL = c(-(S_Wid/2),-(S_Len/2)),
                          BR = c(+(S_Wid/2),-(S_Len/2)))
    temp_df2 = data.frame(Cor= c("x","y"),
                          C  = c(  0,  0),
                          TL = c(  0,  0),
                          TR = c(  0,  0),
                          BL = c(  0,  0),
                          BR = c(  0,  0))
    x_list = list(0)
    for(i in 1:(T_num-1)){
      temp_df2$BL[2] = df$S_y[2]-((S_Len)/2)
      temp_df2$BR[2] = temp_df2$BL[2]
      temp_df2$TL[2] = temp_df2$BL[2] + S_Len
      temp_df2$TR[2] = temp_df2$TL[2]
      temp_df2$C [2] = (temp_df2$TR[2] + temp_df2$BR[2])/2

      temp_df2$BL[1] = sqrt(S_Bor^2 - (temp_df2$BL[2] - temp_df1$TR[2])^2)+temp_df1$TR[1]
      temp_df2$BR[1] = temp_df2$BL[1] + S_Wid
      temp_df2$TL[1] = temp_df2$BL[1]
      temp_df2$TR[1] = temp_df2$BR[1]
      temp_df2$C [1] = (temp_df2$TL[1] + temp_df2$TR[1])/2

      temp_df1 = temp_df2
      x_list = append(x_list, temp_df2$C[1])
    }
    for(i in 1:S_num){
      df$S_x[i] = x_list[df$T_num[i]]
    }
  }
  if(Shape == "cir"){

  }
  df
}
