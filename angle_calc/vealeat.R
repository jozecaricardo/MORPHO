#Randomizing vectors based on a beta distribution
vetores<-function(ve){
  library('zipfR')
  na<-length(ve)
  u<-rbeta(na,(na-1)/2,1/2) #gerando "u" de uma distribuição beta com parâmetros (n-1)/2 e 1/2
  norma.u<-sqrt(sum(u*u))
  raio<-1
  alea<-rnorm((na-1),0,1) #gerando um vetor aleatório com n-1 dimensões
  alea<-rbind(alea)
  alea.n<-alea[,-1] #exceto o valor "a"
  soma<-sum(alea.n^2)
  #function by Li
  fu<-function(soma){sqrt(abs((raio^2)*norma.u-soma))} #produzindo valores de b a partir de a
  fu.valor<-fu(soma)
  vet.menos1<-c(fu.valor,alea.n) #vetor aleatório de uma hemisfera de raio r*raiz-quadrada de u  
  vet.u<-c(vet.menos1,(raio^2)*sqrt(abs(1-norma.u)))
}