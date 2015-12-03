library("ggplot2")

benchmark.plot = function(df, scenario, modelsizes, title, facet, scale, ncol, width = 210, height = 297) {

}

benchmark.plot.by.phase = function(df, scenario, modelsizes, levels, case, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Case == case, ]
  df$Phase = factor(df$Phase, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "Phase", "free_y", ncol)
}

benchmark.plot.by.case = function(df, scenario, modelsizes, levels, phase, title, ncol = 2) {
  df = df[df$Scenario == scenario & df$Phase == phase, ]
  df$Case = factor(df$Case, levels = levels)
  benchmark.plot(df, scenario, modelsizes, title, "Case", "fixed", ncol)
}
