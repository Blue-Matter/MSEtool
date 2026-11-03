# ==================================================================
# == Code for Marine Stewardship Council - DLMtool application =====
# ==================================================================


MSC_status_zones<-function(){
 
  SG60<-'chartreuse'
  SG80<-'green2'
  SG100<-'green3'
  
  textcol='white'
  textcex<-0.9
  
  polygon(c(70,80,80,70),c(0,0,100,100),col=SG60,border=SG60)
  polygon(c(80,100,100,80),c(0,0,50,50),col=SG60,border=SG60)
  polygon(c(80,95,95,80),c(50,50,100,100),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(50,50,100,100),col=SG100,border=SG100)
  
  
  text(97.5,75,"SG100",col=textcol,font=2,srt=90,cex=textcex)
  text(87.5,75,"SG80",col=textcol,font=2,srt=90,cex=textcex)
  text(85,35,"SG60",col=textcol,font=2,cex=textcex)
 
}  
 
MSC_status_zones_LTL<-function(){
  
  SG60<-'chartreuse'
  SG80<-'green2'
  SG100<-'green3'
  
  textcol='white'
  textcex<-0.9
  
  polygon(c(70,80,80,70),c(0,0,100,100),col=SG60,border=SG60)
  polygon(c(80,100,100,80),c(0,0,50,50),col=SG60,border=SG60)
  polygon(c(80,95,95,80),c(50,50,100,100),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(50,50,95,95),col=SG80,border=SG80)
  polygon(c(95,100,100,95),c(95,95,100,100),col=SG100,border=SG100)
  
  
  text(97.5,97.5,"SG100",col=textcol,font=2,cex=textcex*0.6)
  text(90,72.5,"SG80",col=textcol,font=2,cex=textcex)
  text(85,35,"SG60",col=textcol,font=2,cex=textcex)
  
}   

  
MSC_rebuilding_zones<-function(){
  
  SG60<-'chartreuse'
  SG80<-'green2'
  SG100<-'green3'
  
  textcol='white'
  textcex<-0.9
  
  polygon(c(0,100,100,0),c(0,0,100,100),col=SG60,border=SG60)
  polygon(c(70,100,100,70),c(0,0,100,100),col=SG80,border=SG80)
  polygon(c(0,100,100,0),c(95,95,100,100),col=SG100,border=SG100)
  
  
  text(50,97.5,"SG100",col=textcol,font=2,cex=textcex*0.6)
  text(85,55,"SG80",col=textcol,font=2,cex=textcex)
  text(35,55,"SG60",col=textcol,font=2,cex=textcex)
  
} 
