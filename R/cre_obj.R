#' Hi
#'
#' @param T_num Hi
#' @param S_num Hi
#'
#' @return
#' @export
#'
#' @examples cre_obj()
cre_obj = function(T_Num = 2, S_Num = 18){
  trh_lib()

  df = data.frame(S_num = 1:S_Num) %>%
    mutate(T_num = ((S_num - 1) %% T_Num) + 1,
           T_pair = ceiling(S_num / T_Num),
           S_x = NA, S_y = NA)

  detail = data.frame(service = "custom",
                      T_num = T_Num,
                      S_num = S_Num)

  list(data = df, detail = detail)
}
