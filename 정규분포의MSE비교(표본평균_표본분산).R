#모의실험 : 가상의 모집단을 생성후 표본 추출하여 점추정량 생성후 확인

Pop=rnorm(1000000,0,1) #정규분포 N(0,1) 100만개생성 모집단으로 가정
mu=mean(Pop);mu
sigma=sd(Pop);sigma

n=20
Sample=Pop[sample(1:length(Pop),n,replace=F)] # 비복원추출 20개를 샘플추추
Sample
xbar=mean(Sample);xbar
xmedi=median(Sample);xmedi
sort(Sample)

head(Sample)

#확률변수 X ~ N(0,1) 일때 표본평균xbar의 MSE vs 표본중앙값median MSE 비교 n=20고정
Pop=rnorm(1000000,0,1) #정규분포 N(0,1) 100만개생성 모집단으로 가정
mu=mean(Pop);mu
n=20 #sample 20
m=1000
sum1=0;sum2=0
for (i in 1:m)
{
  Sample=Pop[sample(1:length(Pop),n,replace=F)]
  xbar=mean(Sample)
  xmedi=median(Sample)
  sum1 = sum1 + (xbar-mu)^2
  sum2 = sum2 + (xmedi-mu)^2
}

msexbar = sum1/m
msexmedi = sum2/m
msexbar;msexmedi
hist(Sample)

#확률변수 X ~ N(0,1) 일때 표본평균xbar의 MSE vs 표본중앙값median MSE 비교 n=20, 100, 1000
Pop=rnorm(1000000,0,1) #정규분포 N(0,1) 100만개생성 모집단으로 가정
mu=mean(Pop);mu
#n=20 #sample 20
ni=c(20,100,1000)   #sample 20, 100, 1000
for (n in ni){
  m=1000
  sum1=0;sum2=0
  for (i in 1:m)
  {
    Sample=Pop[sample(1:length(Pop),n,replace=F)]
    xbar=mean(Sample)
    xmedi=median(Sample)
    sum1 = sum1 + (xbar-mu)^2
    sum2 = sum2 + (xmedi-mu)^2
  }
  msexbar = sum1/m
  msexmedi = sum2/m
  cat ("n =", n ,"\n")
  cat("MSE(xbar)  :", msexbar ,"\n")
  cat("MSE(median):", msexmedi,"\n")
  #print(msexbar);print(msexmedi)
}

hist(Sample)




#확률변수 X ~ N(0,1) 일때 표본분산S(n-1) MSE vs 표본분산S(n-2) MSE 비교 n=20고정
Pop=rnorm(1000000,0,1) #정규분포 N(0,1) 100만개생성 모집단으로 가정
sigma=sd(Pop);sigma
var1=var(Pop);var1;sigma^2
n=20    #sample 20
Sample=Pop[sample(1:length(Pop),n,replace=F)]
Sample
xbar=mean(Sample);xbar
sn_1 = sum((Sample-xbar)^2)/(n-1)
sn_1
sum((sn_1-sigma^2)^2)

2*sigma^4/19



#확률변수 X ~ N(0,1) 일때 표본분산S(n-1) MSE vs 표본분산S(n-2) MSE 비교 n=20고정
m=1000  #반복수
sum1=0;sum2=0
for (i in 1:m)
{
  Sample=Pop[sample(1:length(Pop),n,replace=F)]
  xbar=mean(Sample)
  sigma=sd(Pop)
  sn_1 = sum((Sample-xbar)^2)/(n-1)
  sn_2 = sum((Sample-xbar)^2)/(n-2)
  
  sum1 = sum1 + (sn_1-sigma^2)^2
  sum2 = sum2 + (sn_2-sigma^2)^2
}

msesn_1 = sum1/m
msesn_2 = sum2/m
msesn_1;msesn_2
hist(Sample)


#확률변수 X ~ N(0,1) 일때 표본분산S(n-1) MSE vs 표본분산S(n-2) MSE 비교 n=20, 100, 1000
Pop=rnorm(1000000,0,1) #정규분포 N(0,1) 100만개생성 모집단으로 가정
ni=c(20,100,1000)   #sample 20, 100, 1000
for (n in ni){
  m=1000  #반복수
  sum1=0;sum2=0
  for (i in 1:m)
  {
    Sample=Pop[sample(1:length(Pop),n,replace=F)]
    xbar=mean(Sample)
    sigma=sd(Pop)
    sn_1 = sum((Sample-xbar)^2)/(n-1)
    sn_2 = sum((Sample-xbar)^2)/(n-2)
    
    sum1 = sum1 + (sn_1-sigma^2)^2
    sum2 = sum2 + (sn_2-sigma^2)^2
  }
  
  msesn_1 = sum1/m
  msesn_2 = sum2/m
  #msesn_1;msesn_2
  cat ("n =", n ,"\n")
  cat("MSE(S(n-1)) :", msesn_1 ,"\n")
  cat("MSE(S(n-2)) :", msesn_2,"\n")
}

hist(Sample)


