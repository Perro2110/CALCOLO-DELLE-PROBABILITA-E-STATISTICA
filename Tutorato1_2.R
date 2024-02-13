

#ES 1:

print("******************")
print("******ES 1*******")
print("******************")
dati=c(137,139,141,137,144,141,139,137,144,141,143,143,141)
tab=table(dati)
print(tab)
tabFreq=prop.table(tab)
print(tabFreq)
plot(tabFreq,type = "h")

#ES 2:

#
#Stati Uniti 387.4
#Sud America 506.3
#Messico 318.0
#Canada 200.8
#

print("******************")
print("****ES 2******")
print("******************")
nomidatEs=c("Stati Uniti","Sud America","Messico","Canada")
print(nomidatEs)
datEs=c(387.4,506.3,318.0,200.8)
pie(datEs,labels=nomidatEs)
print(datEs)
#ES 3:
#HW
print("******************")
print("*****#ES 3:HW*****")
print("******************")
numGuast=c(4,2,4,3,11,6,4,4,1,4,2,3,3,1,2,2,6,0,2,1,3,2)
tabGuasti=table(numGuast)
print(tabGuasti)
tabFGuasti=prop.table(tabGuasti)
print(tabFGuasti)
plot(tabFGuasti,type="l")
freqCom=cumsum(tabFGuasti)
print(freqCom)
barplot(freqCom)
#Calcolare la media, mediana, moda e deviazione standard campionaria.
media=mean(numGuast)
print(media)
mediana=median(numGuast)
print(mediana)

      #script per moda
y=0
for(x in 1:7)
{
  if(tabGuasti[x]>=y)
  {
    y=tabGuasti[x]
  }
}  
print("moda: numero m per n volte:")
print(y)
print(var(numGuast))
print(sd(numGuast))

#SW
print("******************")
print("*****SW*****")
print("******************")
numGuastSW=c(197,5,231,285,278,39,62,33,1,239,166,342,3,1,12,89,531,0,22,13,22,50)
barplot(numGuastSW)
stem(numGuastSW)


#Calcolare la media, mediana, moda e deviazione standard campionaria.
tabGuastiSW=table(numGuastSW)
print(tabGuastiSW)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++")
mediaSW=mean(numGuastSW)
print(mediaSW)
medianaSW=median(numGuastSW)
print(medianaSW)

#script per moda rustica (non ho voglia di gestire il caso di pari mode)
ySW=0

for(x2 in 1:20)
{
  if(tabGuastiSW[x]>=ySW)
  {
    ySW=tabGuastiSW[x]
  }
}  
print("moda: numero m per n volte:")
print(ySW)
print(var(numGuastSW))
print(sd(numGuastSW))

#
print("******************")
print("****ES 4:****")
print("******************")

tran=c(112,121,126,108,141,104,136,134,121,118,143,116,108,122,127,140,113,117,126,130,134,120,131,133,118,125,151,147,137,140,132,119,110,124,132,152,135,130,136,128)
Pollo=table(tran)
t=boxplot(tran)
print(t$stats)
print("modo Figo")
print(quantile(tran))
#####################################################

mediaTr=mean(tran)
print(mediaTr)
medianaTr=median(tran)
print(medianaTr)

#script per moda rustica (non ho voglia di gestire il caso di pari mode)
ytr=0

for(x3 in 1:40)
{
  if(Pollo[x]>=ytr)
  {
    ytr=Pollo[x]
  }
}  
print("moda: numero m per n volte:")
print(ytr)
#deviazione standard campionaria ta faccio bll
print(var(tran))
print(sd(tran))
plot(cumsum(tran))
#e) La distribuzione dei dati è approssimativamente normale? BHO 

#
# pollo) Che percentuale dei dati cade dentro "x±s (s: deviazione standard, "x media
#   campionaria)? quindi k=1
#
k=1
n=(1-((40-1)/40))*100
print("il")
print(n)
print("percento")

#ES 5:

print("******************")
print("***ES 5:***")
print("******************")

stud=c(3.46,3.72,3.95,3.55,3.62,3.80,3.86,3.71,3.56,3.49,3.96,3.90,3.70,3.61,3.72,3.65,3.48,3.87,3.82,3.91,3.69,3.67,3.72,3.66,3.79,3.75,3.93,3.74,3.50,3.83)
stem(stud)
mediaSt=mean(stud)
DerSt=sd(stud)
print(mediaSt)
print(DerSt)
#1.5 poi 2

n=(1-(1/(1.5)^2))*100
print("il")
print(n)
print("percento")

n=(1-(1/(2)^2))*100
print("il")
print(n)
print("percento")



plot(cumsum(prop.table(tran)))

x <- c(64,65,66,67,69,70,72,72,74,74,75,76)
y <- c(91,94,88,103,77,96,105,88,122,102,90,114)
datiPollo=data.frame(x,y)
print(datiPollo)
plot(datiPollo,type="p")
POLLO=cor(datiPollo)
x=mode(numGuast)
print(POLLO[2])