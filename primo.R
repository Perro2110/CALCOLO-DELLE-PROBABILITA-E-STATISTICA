Eta=c(19,912,21,23,22,20)
Peso=c(19,22,21,23,22,20)
h=c(1.64,1.78,1.71,1.72,1.81,1.68)
hCento=h*100
BMI=Peso/h^2
round(BMI,2) #arrotonda
Sesso=c("F","M","M","F","M","F")
factor(Sesso)
Peso<20
Eta[Peso<20]

x=20:30

dati=data.frame(Eta,Peso,h,hCento,BMI,Sesso)
View(dati)
str(dati)
DAAD=dati$Eta
pollo=mean(dati$Eta)
print("media:")
print(pollo)
pollo=median(dati$Eta)
print("mediana:")
print(pollo)
tapply(Peso,Sesso,mean)

