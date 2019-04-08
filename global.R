
l = list.files(path = "modules", pattern = "", full.names = T)
lapply(l, source)
