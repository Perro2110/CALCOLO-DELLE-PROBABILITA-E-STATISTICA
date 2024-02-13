p=0.6
N=10
K=seq(0,N)
fk=seq(0,N)
R=seq(0,N)


for (i in 0:length(K)){
  print(K[i])
}
print("______________________________________")
for (i in 0:length(K)){
  R[i]=(K[i]^2)-7*K[i]
  print(K[i])
}
plot(K,R,type = "b")

for (i in 0:length(fk)){
  fk[i]=choose(10,K[i])*(p^K[i])*(1-p)^(10-K[i]) #PMF
}
plot(K,fk,type = "b")
FK=cumsum(fk) #CDF
plot(K,FK,type = "b")

#8. Si calcoli la PMF e la CDF di K usando dbinom e pbinom
D=dbinom(K,size=N,prob = p)
P=pbinom(K,size=N,prob = p)
#confronto ok
new=rbinom(N,1,0.5)
Dnew=dbinom(new,size=N,prob = 0.5)
media=mean(new)
Enew=sum(new*Dnew)

expR=sum(K*FK)




