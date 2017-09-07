#Calculating angles
angulo<-function(a1,b1){
  norma.a1<-normalize.vector(a1)
  norma.b1<-normalize.vector(b1)
  temp1<-acos(abs(sum(a1*b1))/(sqrt(sum(a1*a1))*sqrt(sum(b1*b1))))
}
