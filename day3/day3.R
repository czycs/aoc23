day3_input <- read_table("day3/day3_input.txt",
                         col_names = FALSE)

day3 <- str_split_fixed(day3_input[[1]], "", 140)

day3 <- as.data.frame(day3)

ma <- as.data.frame(matrix(".", nrow = 142, ncol = 142))
ma[2:141, 2:141] <- day3

counter <- 0
for (i in 0:1000) {
  paste0("(?<=\\D)", i, "(?=\\D)") -> pat
  for (j in 1:142) {
    row <- str_c(ma[j, ], collapse = "")
    location <- as.data.frame(str_locate_all(row, pat))
    if (nrow(location) != 0) {
      print(paste(i,j,location[k, 1]))
      for (k in seq_along(nrow(location))) {
        status <- F
        start <- location[k, 1] - 1
        end <- location[k, 2] + 1
        ma[(j - 1):(j + 1), start:end] -> tera
        tera <- paste(as.matrix(tera), collapse = "")
        status <- str_detect(tera, pattern = "[^\\d.]")
        if (status) {
          counter <- counter + i
          print(paste("digit",i, "Counter:", counter))
        }
      }
    }
  }
}

counter

str_locate_all("..+77..123../13..7..", "(?<=\\D)7(?=\\D)")

str_locate_all("..77..123../13.%7...", "(?<=\\D)7(?=\\D)")

#538046


file_name <- 'day3/day3_input.txt'
width <- readLines(file_name, n = 1) |> nchar()

input <- as.matrix(read.fwf(file_name, widths = rep(1, width), comment.char = ''))

mat <- rbind('.', cbind('.', input, '.'), '.')


R <- nrow(input)
C <- ncol(input)

nums <- as.character(0:9)

x <- 2;
p1 <- 0
found <- c()
star_numbers <- list()
while(x <= R+1) {
  y <- 2
  while(y <= C+1) {

    if(!mat[x,y] %in% nums){
      y <- y+1
      next
    }

    end_y <- y
    while(mat[x,end_y] %in% nums){
      end_y <- end_y + 1
      if(end_y > C+1) break
    }

    nbrs <- mat[x + (-1:1), (y-1):(end_y)]
    if (length(grep("\\d|\\.", nbrs, invert = TRUE)) > 0) {
      n <- as.numeric(paste0(mat[x, y:(end_y-1)], collapse = ''))
      found <- c(found, n)
      p1 <- p1 + n

      star_location <- which(nbrs == '*', arr.ind = TRUE)
      if(length(star_location) > 1){
        for(i in seq_along(nrow(star_location))){
          star_x <- x + (star_location[i, 1] - 2) # since nbrs is from x-1
          star_y <- y + (star_location[i, 2] - 2) # since nbrs is from y-1
          star_numbers[[paste0(star_x, ':', star_y)]] <- c(

            star_numbers[[paste0(star_x, ':', star_y)]],
            n
          )
        }
      }
    }
    y <- end_y
  }
  x <- x+1
}

# Part 1
p1

# Part 2
Filter(\(x) length(x) == 2, star_numbers) |>
  sapply(prod) |>
  sum()
