scree.plot.semi<-function(resultado,pseudo){ #Choosing the ideal number os PC axes...
  #pseudo = curvslide and resultado = an array of shape variables
  library(geomorph)
  forma.proc<-gpagen(resultado,ProcD=F,curves=pseudo,Proj=F)
  escores<-forma.proc$coords
  resul<-plotTangentSpace(escores,verb=T)
  escores.pc<-resul$pc.scores
  
  #Procrustes distances
  k<-dim(escores)[2]
  p<-dim(escores)[1]
  n<-dim(escores)[3]
  dist.proc<-dist(t(matrix(escores,k*p,n)))
  
  #Euclidian distances and correlation...
  correl<-0
  for (di in 1:dim(escores.pc)[2]){
    dist.ma<-cbind(escores.pc[,1:di])
    dista<-dist(dist.ma,met='euclidean')
    dists<-as.dist(dista)
    correl[di]<-cor(dists,as.dist(dist.proc),met='pearson',use='pairwise.complete.obs')
  }
  
  perce<-0
  soma<-0
  for (valor in seq(along=correl)){
    perce<-correl[valor]
    if (perce<max(correl)){
      #estimando a percentagem de explicação dos dados:
      soma[valor]<-resul$pc.summary$importance[2,valor]}
    else {soma[valor]<-0}
  }
  t.perc<-sum(soma)
  
  #plotando a correlação em função do número de PCs:
  PCs<-c(seq(1:dim(escores.pc)[2]))
  plot(PCs,correl,type='n',xlab='número de PCs',ylab='correlação',
       main='Correlação entre matrizes de distância')
  esc<-seq(from=0,to=1,by=0.05)
  for (li in seq(along=esc)){
    if (esc[li]<=1.0){
      abline(h=esc[li],lty='dotted')
    }
        else {stop}
  }
  points(correl,pch=19,col='red')
  return(list(percentual=t.perc,escores=escores.pc))
}