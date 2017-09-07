#angles between the vectors
calcang<-function(dado,num){ #dado=matriz ou data.frame, num=réplicas
  source("~/Dropbox/papers/fabiano/bifoveolatum/angu.R")
  source("~/Dropbox/papers/fabiano/bifoveolatum/vealeat.R")
  install.packages('ppls')
  library('ppls')
  PC12.rad<-angulo(as.vector(dado[,1]),as.vector(dado[,2]))
  #print(PC12)
  PC12<-as.numeric(PC12.rad/pi*180) #em graus
  prob.PC12<-(2*PC12.rad)/(2*(2*pi))  #probabilidade do vetor cair sobre aquele ângulo
  #ve.temp<-matrix(nr=dim(dado)[1],nc=num)
  resul.temp<-NULL
  dado.normal<-normalize.vector(dado[,1])
  for (di in 1:num){
    ve.temp<-vetores(dado[,2])
    #ve.temp<-normalize.vector(ve.tempo)
    ang.temp<-angulo(as.vector(dado.normal),as.vector(ve.temp))
    resul.temp[di]<-(2*ang.temp)/(2*(2*pi)) #probabilidade do vetor aleatório cair na capa da hemi-esfera
    #print(ang.temp)
    #print(resul.temp)
  }
  parcial<-sum(ifelse(resul.temp<=prob.PC12,1,0))
  pval.12<-parcial/num #probabilidade de ser igual ao acaso
  return(data.frame(angulo=PC12,pvalor=pval.12))
}
