order_to_var = function(df, vars, desc = FALSE) {
  
  ordered = df
  
  for (var in vars) {
    
    if (desc) {
      ordered = ordered %>%
        dplyr::arrange(desc(!!sym(var)))
    } else {
      ordered = ordered %>%
        dplyr::arrange(!!sym(var))
    }
    
    ordered = ordered %>%
      mutate(!!paste("order", var, sep = "_") := 1:nrow(df))
  }
  
  return(ordered)
  
}

