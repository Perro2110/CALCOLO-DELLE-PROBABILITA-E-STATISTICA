#Calcolare il fattoriale di un numero n (funzione factorial(n))
n = 7
n=factorial(n)
#Calcolare il coefficiente binomiale di n su k (funzione choose(n,k))
k = 2
x=choose(n,k)



#In un gruppo di N persone, la probabilità che non ci sia nemmeno una coppia con il
#compleanno lo stesso giorno è:
    #formula mistica 
#Calcolare p(e) per N fissato
N=10
pe=(factorial(N)*choose(365,N))/365^N
#la not p(e)
NN=1-pe
#Cosa succede per N=23?
N=23
pe23=(factorial(N)*choose(365,N))/365^N
NN23=1-pe
#Studiare l’andamento di �(�) e �(�&) al variare di � e generarne il grafico (a linee). Per
#studiare l’andamento delle probabilità al variare di � si consiglia di utilizzare un ciclo “for”:
Nvecto=rep(0,100)
si=rep(0,100)
fr=rep(0,100)
fr2=rep(0,100)
i=0
for (i in 1:length(Nvecto)){
  N=i
  si[i]=((factorial(N)*choose(365,N))/365^N)
  Nvecto[i]=1-si[i] #non accade
  
  conta=0
  conta2=vector()
  for (j in 1:N){
    y=sample(1:365, N, replace=TRUE)
    #print(y)
    if (anyDuplicated(y)>0) {
      conta=conta+1
    }
    conta2[j]=anyDuplicated(y)
  }
  
  fr[i]=conta/N
  fr2[i]=sum(conta2>0)/N
}

plot(Nvecto,type="l", col="red")
lines(si,type="l", col="blue")
lines(fr, type="p", col="green")
lines(fr2, type="p", col="magenta")

####################


