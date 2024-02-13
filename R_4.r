#introduzione Gaussiane

x=seq(from = -6, to = 6, by = 0.1)
y=dnorm(x, mean = 0, sd = 1, log = FALSE) #crea gaussiana con valor medio 0 e
plot(x,y)                        #varianza 1 da valutare per i valori definiti in x. 
z=rnorm(1, mean = 0, sd = 1)
n=rnorm(1000, mean = 0, sd = 1)
hist(n)
hist(n,breaks =70,freq=FALSE)
g=seq(-3,3,0.1)
lines(g,dnorm(g,mean=0,sd=1),type="l", col="green")

                                        #ES FIGHISSIMO GAUSSIANE#
#IN una popolazione di ragazze di età compresa tra i 18 e i 25 anni, la concentrazione di
#emoglobina nel sangue X approssima una distribuzione gaussiana con media =13.1 g/dl e
#deviazione standard =0.7 g/dl.
#In base a queste informazioni, si calcoli tramite l’utilizzo di R quante ragazze hanno
#emoglobina inclusa tra 12.26 e 13.52 g/dl.

# Punto 1. Plottare la distribuzione di emoglobina nel sangue delle ragazze tra 18 e 25 anni.

mu=13.1 # in grammi/dl media
sigma=0.7 # standard deviation

x=seq(11, 15.2, 0.1) #dato testo 

f=(1/sqrt(2*pi*sigma^2))*exp(-(x-mu)^2/(2*sigma^2)) # = a funzione: dnorm (x,mu,sd)
plot(x,f, type="l", col="red")
                                          #per l'appunto:
y=dnorm(x, mean=mu, sd=sigma, log=FALSE)
lines(x,y,type="l", lty=2, col="green")

#Punto 2: Usare la funzione pnorm per calcolare la CDF
                           #NB pnorm()
#La funzione pnorm(q) restituisce l’integrale da -Inf a q della pdf
#della normale. Pnorm() ha gli stessi valori di default di mu e
#sigma (mean e sd) della funzione dnorm()

Fb=pnorm(13.52, mean=mu, sd=sigma) #funzione
Fzb=pnorm((13.52-mu)/sigma) #formula

Fa=pnorm(12.26, mean=mu, sd=sigma)
Fza=pnorm((12.26-mu)/sigma)

#Punto 3: Calcolare la probabilità di avere l'emoglobina compresa tra 12.26 g/dl e 13.52 g/dl:
P=(Fb-Fa)*100
Pz=(Fzb-Fza)*100

#introduzione Esponenziali

# help rexp #dexp #pexp #qexp
rm(list=ls())
x<-seq(-1,10, 0.1)
lambda=2
plot(x, dexp(x, lambda), type='l')

# generiamo un vettore di dati esponenziali
n=1000000
dataExp<-rexp(n, rate=lambda)
histData <- hist(dataExp, breaks = 70, freq = FALSE)
#plot(histData$counts/n, type='l')

h=histData$breaks[2]
xx=seq(min(dataExp),max(dataExp),h)
lines(xx,histData$counts/n/(h), type='l', col="blue")
lines(x, dexp(x, lambda), type='l', col="red")

#hist(dataExp, breaks = 70, freq = FALSE)
#lines(x, dexp(x, lambda), type='l', col="red")

