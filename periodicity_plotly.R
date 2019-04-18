library(plotly)

count<-read.table("/partage/bioinfo/VIH/run/qualitativeAnalysis/periodicity_vih/VIH.merge.count2.-.txt")

plot_ly(count, x = ~V1, y = ~V2, type = 'bar', 
             marker = list(line = list(width = 0))) %>%
  layout(title = "Periodicity",
         xaxis = list(title = "Position"),
         yaxis = list(title = "Count"))
