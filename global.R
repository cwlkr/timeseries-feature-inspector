
l = list.files(path = "shiny-applet/modules", pattern = "", full.names = T)
lapply(l, source)
