library(plotly)

count<-read.table("/partage/bioinfo/VIH/run/qualitativeAnalysis/periodicity_vih/VIH.merge.count2.-.txt")

couleur<-NULL
for(i in 1:nrow(count)){
	un<-(count$V1[i]-1)
	deux<-(count$V1[i]-2)
	if(count$V1[i]==1) {
		couleur<-c(couleur,"green")
	} 
	else if(count$V1[i]==2) {
		couleur<-c(couleur,"red")
	} 
	else if(count$V1[i]==3) {
		couleur<-c(couleur,"blue")
	}
	else if(un%%3==0) {
		couleur<-c(couleur,"green")
	}
	else if(deux%%3==0) {
		couleur<-c(couleur,"red")
	} 
	else
		couleur<-c(couleur,"blue")
}
count<-cbind(count,couleur)


plot_ly(count, x = ~V1, y = ~V2, type = 'bar', 
             marker = list(color= ~couleur, line = list(width = 0))) %>%
  layout(title = "Periodicity",
         xaxis = list(title = "Position"),
         yaxis = list(title = "Count"))
