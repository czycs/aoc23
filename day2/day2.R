day2_input <- read_delim(
  "day2/day2_input.txt",
  escape_double = FALSE,
  col_names = FALSE,
  trim_ws = TRUE
)

str_split(day2_input$X2, "; ") -> day2
lapply(day2, str_split, ", ") -> day2

0 -> counter
for (i in 1:100) {
  game <- day2[[i]]
  status <- T
  for (j in 1:length(game)) {
    move <- game[[j]]
    df <- str_split(move, " ", simplify = T)
    df <- column_to_rownames(as.data.frame(df), var = "V2")
    as.data.frame(t(df)) -> df
    if ("red" %in% colnames(df)) {
      if (as.numeric(df$red) > 12) {
        status <- F
      }
    }
    if ("blue" %in% colnames(df)) {
      if (as.numeric(df$blue) > 14) {
        status <- F
      }
    }
    if ("green" %in% colnames(df)) {
      if (as.numeric(df$green) > 13) {
        status <- F
      }
    }
  }
  if (status) {
    counter <- counter + i
  }
}
counter
# part2
0 -> counter
for (i in 1:100) {
  game <- day2[[i]]
  lowestR <- lowestB <- lowestG <- 0
  for (j in 1:length(game)) {
    move <- game[[j]]
    df <- str_split(move, " ", simplify = T)
    df <- column_to_rownames(as.data.frame(df), var = "V2")
    as.data.frame(t(df)) -> df

    if ("red" %in% colnames(df)) {
      if (as.numeric(df$red) > lowestR) {
        lowestR <- as.numeric(df$red)
      }
    }
    if ("blue" %in% colnames(df)) {
      if (as.numeric(df$blue) > lowestB) {
        lowestB <- as.numeric(df$blue)
      }
    }
    if ("green" %in% colnames(df)) {
      if (as.numeric(df$green) > lowestG) {
        lowestG <- as.numeric(df$green)
      }
    }
  }
  power <- lowestB * lowestG * lowestR
  counter <- counter + power
}
counter
