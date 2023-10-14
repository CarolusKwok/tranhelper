#' Exports transhelper calculated results as dataframe or csv
#'
#' @param obj transhelper object
#' @param file file name
#'
#' @return
#' @export
#'
#' @examples write_obj(find_twidth())
write_obj = function(obj, file){
  trh_lib()

  data = obj$data
  detail = as.data.frame(obj$detail) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = everything(), names_to = "detail", values_to = "value")
  data$detail = NA
  data$value  = NA

  if(nrow(data) < nrow(detail)){
    data[(nrow(data)+1):(nrow(detail)),] = NA
  }

  for (i in 1:nrow(detail)){
    data$detail[i] = detail$detail[i]
  }
  for (i in 1:nrow(detail)){
    data$value[i] = detail$value[i]
  }
  if(hasArg(file)){
    write_csv(data, paste0(file,".csv"))
  }
  remove(detail)
  data
}
