library(plotly)

#Import des donnees de comptage
count<-read.table(params$data)

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
countTable <-cbind(count,couleur)

bleu <- subset(countTable, couleur == "blue")
rouge <- subset(countTable, couleur == "red")
vert <- subset(countTable, couleur == "green")

#import des donnees gff
gff<-read.table(params$gff3,sep="\t",header=F)
annot<-params$annotation
if(annot=="CDS"){
  gff_soft<-subset(gff,gff$V3=="CDS")
} else if(annot=="gene"){
  gff_soft<-subset(gff,gff$V3=="gene")
} else if(annot=="five_prime_UTR and three_prime_UTR"){
  gff_soft<-subset(gff,gff$V3=="five_prime_UTR" | gff$V3=="three_prime_UTR")
} else if(annot=="transcript"){
  if("transcript" %in% gff$V3){
    gff_soft<-subset(gff,gff$V3=="transcript")
  }
  else{
    gff_soft<-subset(gff,gff$V3=="five_prime_UTR" | gff$V3=="three_prime_UTR" | gff$V3=="CDS")
  }
} else{
  gff_soft<-gff
}

stage<-NULL
for (i in 1:nrow(gff_soft)){
  un<-(gff_soft$V4[i]-1)
  deux<-(gff_soft$V4[i]-2)
  if(gff_soft$V1[i]==1) {
    phase<-1
  } 
  else if(gff_soft$V1[i]==2) {
    phase<-2
  } 
  else if(gff_soft$V1[i]==3) {
    phase<-3
  }
  else if(un%%3==0) {
    phase<-1
  }
  else if(deux%%3==0) {
    phase<-2
  } 
  else
    phase<-3
  
  if(gff_soft$V7[i]=="-"){
    if(phase==1){
      phase<-3
    }
    else if (phase==3){
      phase<-1
    }
  }
  
  stage<-c(stage,phase)
}

gff_soft<-cbind(gff_soft,stage)

p <- plot_ly() %>%
  add_trace(x = vert$V1, y = vert$V2, type = 'bar', name = 'Phase 1', 
            marker = list(color = 'green',
                          line = list(width = 0))) %>%
  add_trace(x = rouge$V1, y = rouge$V2, type = 'bar', name = 'Phase 2', 
            marker = list(color = 'red',
                          line = list(width = 0))) %>%
  add_trace(x = bleu$V1, y = bleu$V2, type = 'bar',name = 'Phase 3', 
            marker = list(color = 'blue',  
                          line = list(width = 0))) %>%
  layout(title = "Periodicity",
         xaxis = list(title = "Position"),
         yaxis = list(title = "Count", range = c(0,2000)))

p2 <- plot_ly()   %>%
  layout(yaxis = list(title = 'Annotation', 
                      fixedrange=T,
                      range = c(-1.5,1.5),
                      zeroline = F,
                      showline = F,
                      showticklabels = F,
                      showgrid = F),
         xaxis = list( range = c(count$V1[1], count$V1[nrow(count)]),
                       title = "Position",
                       zeroline = T,
                       showline = T,
                       showticklabels = T,
                       showgrid = F)) 
for(i in 1:nrow(gff_soft)){
  if(gff_soft$stage[i]==1){
    p2 <- add_trace(p2, 
                    x = c(gff_soft$V4[i],gff_soft$V5[i]),
                    y = rep(1,2) ,
                    type="scatter",
                    mode = 'lines+markers',
                    text = gsub(";","\n",gff_soft$V9[i]) ,
                    hoverinfo = 'text',
                    showlegend = F,
                    marker = list(color= "green", width = 4),
                    line=list(color="green")
    )
  }
  else if(gff_soft$stage[i]==2)	
    p2 <- add_trace(p2, 
                    x = c(gff_soft$V4[i],gff_soft$V5[i]),
                    y = rep(0,2) ,
                    type="scatter",
                    mode = 'lines+markers',
                    text = gsub(";","\n",gff_soft$V9[i]) ,
                    hoverinfo = 'text',
                    showlegend = F,
                    marker = list(color= "red", width = 4),
                    line=list(color="red")
    )
  else if(gff_soft$stage[i]==3)	
    p2 <- add_trace(p2, 
                    x = c(gff_soft$V4[i],gff_soft$V5[i]),
                    y = rep(-1,2) ,
                    type="scatter",
                    mode = 'lines+markers',
                    text = gsub(";","\n",gff_soft$V9[i]) ,
                    hoverinfo = 'text',
                    showlegend = F,
                    marker = list(color= "blue", width = 4),
                    line=list(color="blue")
    )
}

subplot(p, p2, shareX=TRUE,nrows = 2, heights = c(0.8, 0.2))