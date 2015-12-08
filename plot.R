library("ggplot2")

benchmark.plot = function(df, scenario, artifacts, title, facet, scale, ncol, width = 210, height = 297) {
  if (ncol > 1) {
    evens = seq(2, nrow(artifacts), by=2)
    artifacts = artifacts[-evens, ]
  }
  
  # x axis labels
  artifacts.scenario = artifacts[artifacts$Scenario == scenario, "Triples"]
  artifacts
  
  xbreaks = artifacts[artifacts$Scenario == scenario, "Artifact"]
  xlabels = paste(xbreaks, "\n", artifacts.scenario, sep = "")
  
  # y axis labels
  ys = -10:10
  ybreaks = 10^ys
  ylabels = parse(text = paste("10^", ys, sep = ""))
  
  plot.filename = gsub(" ", "-", title)
  
  facet = as.formula(paste("~", facet))
  
  p = ggplot(df) +
    labs(title = paste(scenario, " scenario, ", title, sep = ""), x = "model size\nnumber of model elements", y = "execution time [s]") +
    geom_point(aes(x = as.factor(Artifact), y = time, col = Tool, shape = Tool), size = 1.5) +
    geom_line(aes(x = as.factor(Artifact), y = time, col = Tool, group = Tool), size = 0.4) +
    scale_shape_manual(values = seq(0,24)) +
    scale_x_discrete(breaks = xbreaks, labels = xlabels) +
    scale_y_log10(breaks = ybreaks, labels = ylabels) +
    facet_wrap(facet, ncol = ncol, scale = scale) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") + #, axis.text.x = element_text(size=8)) +
    guides(shape = guide_legend(ncol = 2))
  print(p)
  
  ggsave(file = paste("diagrams/", scenario, "-", plot.filename, ".pdf", sep = ""), width = width, height = height, units = "mm")
}

benchmark.plot2 = function(df, scenario, artifacts, title, scale, ncol, width = 210, height = 145) {
  if (ncol > 1) {
    evens = seq(2, nrow(artifacts), by=2)
    artifacts = artifacts[-evens, ]
  }
  
  # x axis labels
  artifacts.scenario = artifacts[artifacts$Scenario == scenario, "Triples"]
  artifacts
  
  xbreaks = artifacts[artifacts$Scenario == scenario, "Artifact"]
  xlabels = paste(xbreaks, "\n", artifacts.scenario, sep = "")
  
  # y axis labels
  ys = -10:10
  ybreaks = 10^ys
  ylabels = parse(text = paste("10^", ys, sep = ""))
  
  plot.filename = gsub(" ", "-", title)
  
  p = ggplot(df) +
    labs(title = paste(scenario, " scenario, ", title, sep = ""), x = "model size\nnumber of model elements", y = "execution time [s]") +
    geom_point(aes(x = as.factor(Artifact), y = time, col = Case, shape = Case), size = 1.5) +
    geom_line(aes(x = as.factor(Artifact), y = time, col = Case, group = Case), size = 0.4) +
    scale_shape_manual(values = seq(0,24)) +
    scale_x_discrete(breaks = xbreaks, labels = xlabels) +
    scale_y_log10(breaks = ybreaks, labels = ylabels) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") #, axis.text.x = element_text(size=8))
  print(p)
  
  ggsave(file = paste("diagrams/", scenario, "-", plot.filename, ".pdf", sep = ""), width = width, height = height, units = "mm")
}

benchmark.plot.by.phase = function(df, scenario, artifacts, phase, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Phase == phase, ]
  df$Phase = factor(df$Phase)
  benchmark.plot2(df, scenario, artifacts, title, "free_y", ncol)
}

benchmark.plot.by.case = function(df, scenario, artifacts, levels, phase, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Phase == phase, ]
  df$Case = factor(df$Case, levels = levels)
  benchmark.plot(df, scenario, artifacts, title, "Case", "fixed", ncol)
}
