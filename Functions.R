encode_column <- function(data, column_name) {
  column_name <- enquo(column_name)
  column_name_encoded <- paste0(quo_name(column_name), "_encoded")
  
  unique_mapping <- data %>%
    distinct(!!column_name) %>%
    mutate(!!column_name_encoded := 1:n())
  
  data <- data %>%
    left_join(unique_mapping, by = quo_name(column_name))
  
  return(data)
}
#train <- encode_column(train, family)
