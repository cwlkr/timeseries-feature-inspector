if(!require("pacman")){
  install.packages("pacman")
}
l = list.files(path = "modules", pattern = "", full.names = T)
lapply(l, source)

lf = list.files(path = "functions", pattern = "", full.names = T)
lapply(lf, source)
