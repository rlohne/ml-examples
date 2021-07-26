train_cor <- train %>%
  select(is.numeric) %>%
  cor() %>%
  as_tibble() %>%
  mutate(rowname = names(.)) %>%
  mutate_all(~ replace(.,. == 1 , NA)) %>% # Replace 1's with NA
  filter(if_any(everything(), ~ .x > 0.75 ))