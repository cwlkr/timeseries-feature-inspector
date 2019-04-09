create_plot = function(data, ci.lvl, stimulus.rug, stim.var, erk.ratio.var, time.var, nuc.erk, cyto.erk,stim.color = "red",  alpha = 0.1, xlab = "Real Time (min)", ylabel = "ERK-KTR Cytoplasmic ot Nuclear Ratio", vlines = F, ribbon){
  a = ci.lvl
  #create unique id for each track..
  #data[,unique := paste(Image_Metadata_Site, objNuc_TrackObjects_Label, sep = "_")]
  #calc erk ratio
  # for each grouping calc mean, min, max, ci upper lower
  data.summary <- data %>% ungroup() %>% group_by(.dots = c(stim.var, time.var)) %>% summarise(ymin_ctn = min(get(erk.ratio.var)) , ymax_ctn = max(get(erk.ratio.var)), mean_ctn = mean(get(erk.ratio.var)),
                                                                                               lower = mean(get(erk.ratio.var)) - (qnorm(1-(a/2)) * sd(get(erk.ratio.var))/sqrt(n())) , upper =  mean(get(erk.ratio.var)) + (qnorm(1-(a/2)) * sd(get(erk.ratio.var))/sqrt(n())))
  #get also rug of pulses
  
  #check if CTRL and treat differently and maybe check if everything is ctrl.
  ctrl = data.summary %>% filter(get(stim.var) %like% "CTRL")
  #ctrl.neg = data.summary %>% filter(get(stim.var) %like% "\\-CTRL")
  #ctrl.pos = data.summary %>% filter(get(stim.var) %like% "\\+CTRL")
  ctrl = ctrl %>% ungroup %>% mutate(Stimulation_treatment =  forcats::fct_rev(as.factor(get(stim.var))))
  
  if(nrow(ctrl) == nrow(data.summary)){
    data.plot = ctrl
  }else{
    data.plot = data.summary %>% filter(!(get(stim.var) %like% "CTRL"))
  }
  ggp = ggplot(data.plot, aes(x = get(time.var), y = mean_ctn, group = get(stim.var)))+ 
    geom_line(aes(color = get(stim.var)), size = 1.2) + 
    labs(x = xlab, y = ylabel, legend = "stim.var")  + ggplotTheme() + theme(legend.title = element_blank()) #+
    #geom_rug(data = data.frame(stim.times = stimulus.rug), aes(x=stim.times,y =NULL, group = NULL), color = stim.color)
  
  if(!(nrow(ctrl) == nrow(data.summary))){
    ggp = ggp + geom_line(data = ctrl, aes( group = get(stim.var), linetype=get(stim.var)),  color = "black",  size = 1.2) +
      #geom_line(data = ctrl.neg, aes( group = get(stim.var)), linetype="dashed", color = "black",  size = 1.2) +
      #scale_linetype_manual(values = c("pos", "neg")) +
      scale_color_brewer(palette = "Set1")
    if (ribbon){
      ggp = ggp + geom_ribbon(data = ctrl,alpha = alpha, mapping = aes(ymin = lower, ymax = upper)) 
    }
  }
  if(ribbon){
    ggp = ggp  + geom_ribbon(alpha = alpha, mapping = aes(ymin = lower, ymax = upper))
  }
  if(vlines){
    ggp = ggp + geom_vline(data = data.frame(stim.times = stimulus.rug), aes(xintercept=stim.times, group = NULL), color = stim.color, linetype="dotted")
  }
  
  return(ggp)
}

#stolen from dmattek tca-package
ggplotTheme = function(in.font.base = 12,
                       in.font.axis.text = 12,
                       in.font.axis.title = 12,
                       in.font.strip = 14,
                       in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base, base_family = "Helvetica") +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"))
  
  return(loc.theme)
}
create_pdf = function(data, 
                      ci.lvl, stimulus.rug,
                      stim.var, erk.ratio.var,
                      time.var, nuc.erk, cyto.erk,
                      stim.color = "red",  alpha = 0.1,
                      xlab = "Real Time (min)", ylabel = "ERK-KTR Cytoplasmic ot Nuclear Ratio",
                      vlines = F, ngroups, pdf.filename, meta.grouping){
  
  
  pdf(file = pdf.filename)
  
  for(i in (0:(ngroups-1))){
    
    #stim_vec = as.numeric(pull(stim_vec))
    gg = create_plot(data = setDT(data)[get(meta.grouping) == i],
                     ci.lvl = 0.05,
                     stimulus.rug = stimulus.rug,
                     nuc.erk = nuc.erk,
                     cyto.erk = cyto.erk,
                     time.var = time.var,
                     stim.var = stim.var,
                     erk.ratio.var = erk.ratio.var,
                     vlines = vlines)
    plot(gg)
  }
  gg = create_plot(data = setDT(data)[get(stim.var) %like% "CTRL"],
                   ci.lvl = 0.05,
                   stimulus.rug = stimulus.rug,
                   nuc.erk = nuc.erk,
                   cyto.erk = cyto.erk,
                   time.var = time.var,
                   stim.var = stim.var,
                   erk.ratio.var = erk.ratio.var,
                   vlines = vlines)
  plot(gg)
  dev.off()
  
}
