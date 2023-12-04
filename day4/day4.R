day4_input <- read_table("day4/day4_input.txt",
                         col_names = FALSE)
pt1<-0
for (i in 4:4) {
  day4_input[i,3:12]-> winning
  day4_input[i,14:38]-> numbers
  won<-match(winning,numbers)
  power <- sum(!is.na(won))
  if(power>0){
  score <- 2**(power-1)
  pt1 <- pt1+score
  }
}

pt1

#part2
day4_input->day4

powervec <- c()
for (counter in 1:190) {
  power<-0
  day4[counter, 3:12] -> winning
  day4[counter, 14:38] -> numbers
  won <- match(winning, numbers)
  power <- sum(!is.na(won))
  powervec <- append(powervec, power)
}

day4<-cbind(day4, "count" = rep(1,190))
day4<-cbind(day4, "power" = powervec)
df<-day4[,39:40]

for (i in 1:190) {
  if (df[i,2]>0) {
    step<-df[i,2]
    df[i+(1:step),1]<-df[i+(1:step),1]+df[i,1]
  }

}

pt2<-sum(df$count)
pt2
