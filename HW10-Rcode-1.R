library(tswge)
#Question 7.1 (a)

R0 = c(0.79,0.22,0.22,0.61)
R1 = c(1,0.69,0.22,0.22)
P = toeplitz(R1)
P
solve(P)%*%R0

phi = matrix(0,4,4)
phi[1,1]=R0[1]

S1=0;S2=0;

solve(P)%*%R0


for(kk in 2:4)
{
  for(ii in 1:(kk-1))
  {
    S1 = phi[(kk-1),ii]*R0[kk-ii]+S1
    S2 = phi[(kk-1),ii]*R0[ii]+S2
  }
  phi[kk,kk]=(R0[kk]-S1)/(1-S2)
  S1=0;S2=0;
  
  for(jj in 1:(kk-1))
    
    phi[kk,jj]=phi[(kk-1),jj]-phi[kk,kk]*phi[(kk-1),(kk-jj)]
  
}

round(solve(P*8.193),2)
round(solve(P*8.25),2)

#Question 7:3

data(sunspot.classic)
yw1=est.ar.wge(sunspot.classic,p=2,type='yw')
burg1=est.ar.wge(sunspot.classic,p=2,type='burg')
mle1=est.ar.wge(sunspot.classic,p=2,type='mle')


yw18=est.ar.wge(sunspot.classic,p=8,type='yw')
burg18=est.ar.wge(sunspot.classic,p=8,type='burg')
mle18=est.ar.wge(sunspot.classic,p=8,type='mle')


pro72 = c(26.8,27.8,30,31.6,33,34.2,34.1,33.2,31.7,31.5,31.9,31.5,30,29.4,29.9)
plot(1:15,pro72,pch=8)


yw721=est.ar.wge(pro72,p=1,type='yw')
burg721=est.ar.wge(pro72,p=1,type='burg')
mle721=est.ar.wge(pro72,p=1,type='mle')



yw72=est.ar.wge(pro72,p=2,type='yw')
burg72=est.ar.wge(pro72,p=2,type='burg')
mle72=est.ar.wge(pro72,p=2,type='mle')


##################################333
#Question 7.4
factor.wge(phi=c(1.3,-0.6))
plotts.true.wge(n=100,phi=c(1.3,-0.6))

xt = gen.arma.wge(n=100,phi=c(1.3,-0.6),sn=2)-20
plotts.sample.wge(xt)


plot(1:length(xt),xt,xlab='Time',sub='Realization',type = 'l')

yw.a = est.ar.wge(xt,p=2,type='yw')
burg.a = est.ar.wge(xt,p=2,type='burg')
mle.a = est.ar.wge(xt,p=2,type='mle')


#########3
factor.wge(phi=c(2.69,-2.583,0.891))
plotts.true.wge(n=100,phi=c(2.69,-2.583,0.891))

yt = gen.arma.wge(n=100,phi=c(2.69,-2.583,0.891),sn=2)+250
plotts.sample.wge(yt)


plot(1:length(yt),yt,xlab='Time',sub='Realization',type = 'l')

yw.c = est.ar.wge(yt,p=3,type='yw')
burg.c = est.ar.wge(yt,p=3,type='burg')
mle.c = est.ar.wge(yt,p=3,type='mle')


data.frame(yw.c$phi,burg.c$phi,mle.c$phi)
data.frame(yw.c$avar,burg.c$avar,mle.c$avar)

################################33
#Question 7.5

ss = sunspot.classic
plotts.parzen.wge(ss)

ss.mle=est.ar.wge(ss,p=2,type='mle')

true.arma.spec.wge(phi=ss.mle$phi)

ss.mle=est.ar.wge(ss,p=8,type='mle')

true.arma.spec.wge(phi=ss.mle$phi)

factor.wge(phi=ss.mle$phi)

################################33
#Question 7.6

yt = gen.arma.wge(n=100,phi=c(2.69,-2.583,0.891),sn=2)+250

plotts.true.wge(phi=c(2.69,-2.583,0.891))


plot(1:length(yt),yt,xlab='Time',sub='Realization',type = 'l')

plotts.parzen.wge(yt)
yw.c = est.ar.wge(yt,p=3,type='yw')
burg.c = est.ar.wge(yt,p=3,type='burg')
mle.c = est.ar.wge(yt,p=3,type='mle')
true.arma.spec.wge(phi=yw.c$phi)
true.arma.spec.wge(phi=burg.c$phi)
true.arma.spec.wge(phi=mle.c$phi)

############Question 8.1####################
data(prob8.1a)
data(prob8.1b)
data(prob8.1c)

p1=plotts.sample.wge(prob8.1a,arlimits = T)
round(p1$autplt,3)
n = length(prob8.1a)
2*(1/sqrt(n))

p2=plotts.sample.wge(prob8.1b,arlimits = T)
round(p2$autplt,3)
n = length(prob8.1b)
2*(1/sqrt(n))


p3=plotts.sample.wge(prob8.1c,arlimits = T)
round(p3$autplt,3)
n = length(prob8.1c)
2*(1/sqrt(n))









