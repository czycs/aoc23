library(tidyverse)
day1_input <- read_csv("day1/day1_input.txt", col_names = FALSE, trim_ws = FALSE)
test_input <- read_csv("day1/test.txt", col_names = FALSE, trim_ws = FALSE)

day1_input %>% mutate_all(str_replace_all, "[^0-9.-]", "") -> numbersonly

numbersonly %>%
  mutate(start = str_sub(X1, end = 1L), end = (str_sub(X1, start = -1L))) %>%
  mutate(twodidgit = as.numeric(paste0(start, end))) -> collapsed

sum(collapsed$twodidgit)

# part 2
# added other edge cases
test[8, 1] <- "eighthree"
test[9, 1] <- "sevenine"

# hacky solution
day1_input %>%
  mutate_all(str_replace_all, "one", "o1e") %>%
  mutate_all(str_replace_all, "two", "t2o") %>%
  mutate_all(str_replace_all, "three", "th3ee") %>%
  mutate_all(str_replace_all, "four", "fo4r") %>%
  mutate_all(str_replace_all, "five", "fi5e") %>%
  mutate_all(str_replace_all, "six", "s6x") %>%
  mutate_all(str_replace_all, "seven", "se7en") %>%
  mutate_all(str_replace_all, "eight", "ei8ht") %>%
  mutate_all(str_replace_all, "nine", "ni9e") %>%
  mutate_all(str_replace_all, "[^0-9.-]", "") -> numbersonly

numbersonly %>%
  mutate(start = str_sub(X1, end = 1L), end = (str_sub(X1, start = -1L))) %>%
  mutate(twodidgit = as.numeric(paste0(start, end))) -> collapsed

sum(collapsed$twodidgit)

# regex lookahead solution
df <- day1_input$X1 %>%
  str_extract_all("[0-9]|(on(?=e))|(tw(?=o))|(thre(?=e))|(four)|(fiv(?=e))|(six)|(seve(?=n))|(eigh(?=t))|(nin(?=e))", simplify = TRUE) %>%
  as.data.frame() %>%
  mutate_all(str_replace_all, "on", "1") %>%
  mutate_all(str_replace_all, "tw", "2") %>%
  mutate_all(str_replace_all, "thre", "3") %>%
  mutate_all(str_replace_all, "four", "4") %>%
  mutate_all(str_replace_all, "fiv", "5") %>%
  mutate_all(str_replace_all, "six", "6") %>%
  mutate_all(str_replace_all, "seve", "7") %>%
  mutate_all(str_replace_all, "eigh", "8") %>%
  mutate_all(str_replace_all, "nin", "9")


df1 <- as.data.frame(do.call(paste0, df))
colnames(df1) <- "X1"
df1 %>%
  mutate(start = str_sub(X1, end = 1L), end = (str_sub(X1, start = -1L))) %>%
  mutate(twodidgit = as.numeric(paste0(start, end))) -> collapsed

sum(collapsed$twodidgit)

