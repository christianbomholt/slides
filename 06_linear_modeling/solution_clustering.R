library(readr)

pwi=read_csv("../data/brain/pwi.csv",col_names = FALSE)
pwi = as.matrix(pwi)
dwi=read_csv("../data/brain/dwi.csv", col_names=FALSE)
dwi = as.matrix(dwi)
mask=read_csv("../data/brain/mask.csv", col_names=FALSE)
mask = as.matrix(mask)

# Show images

par(mfrow=c(1,2))
image(pwi,col=gray(seq(0,1,length=100)))
image(dwi,col=gray(seq(0,1,length=100)))

par(mfrow=c(1,2))
mdwi <-dwi
mdwi[mdwi<0]<-0
library(plotly)

p1<-plot_ly(z=~mdwi, type = "contour") 
p2<-plot_ly(z=~pwi, type = "contour") 

subplot(p1,p2) %>% layout(title = "Lesion core, high DWI, high MTT")

N = sum(mask==1)
Y = matrix(0, nrow=N, ncol=2)
Y[,1] = pwi[mask==1]
Y[,2] = dwi[mask==1]


W = rep(0,8)      	
V = W
for (i in 2:9){
  km = kmeans(Y,i)
  W[i-1]=sum(km$withinss)
  V[i-1] <- round(km$betweenss / km$totss,5)
}

plot(2:9,W, type = 'l')
plot(2:9,V, type = 'l')

K=3
km = kmeans(Y,K)
segmented = matrix(0, ncol=ncol(pwi), nrow=nrow(pwi))
segmented[mask==1]=km$cluster
p1<-plot_ly(z=~segmented, type = "contour") 

Y.scaled = scale(Y)
km = kmeans(Y.scaled,K)
segmented1 = matrix(0, ncol=ncol(pwi), nrow=nrow(pwi))
segmented1[mask==1]=km$cluster
p2<-plot_ly(z=~segmented1, type = "contour") 


subplot(p1,p2) %>% layout(title = "Importance of scaling")



### new

require(mvtnorm)       # For multivariate Gaussian density

pi.est = rep(1,K)/K
rho = matrix(0,nrow=N,ncol=K)

init.id = sample(1:N,K)
mu = Y[init.id,]
sigma = array(0,c(K, dim(Y)[2], dim(Y)[2]))
for (k in 1:K) sigma[k,,]=cov(Y)

for (iter in 1:20){
  print(iter)
  
  # Calculate the N responsibilities
  
  for (i in 1:N){
    for (j in 1:K){
      rho[i,j] = pi.est[j]*dmvnorm(Y[i,],mu[j,],sigma[j,,])  		
    }
    rho[i,] = rho[i,] / sum(rho[i,])
  }
  
  # Update parameters
  
  for (j in 1:K){
    mu[j,] = colSums(Y*rho[,j]) / sum(rho[,j])		
  }
  
  for (j in 1:K){
    sigma.tmp = 0
    for (i in 1:N){
      sigma.tmp = sigma.tmp + rho[i,j] * (Y[i,]-mu[j,])%*%t((Y[i,]-mu[j,]))
    }
    sigma[j,,] = sigma.tmp / sum(rho[,j])
  }
  
  for (j in 1:K){
    pi.est[j] = sum(rho[,j]) / N
  }
  
}


img_er <- function(k){
  probclass = matrix(0, nrow=nrow(pwi), ncol=ncol(pwi))
  probclass[mask==1]=rho[,k]
  
  return(plot_ly(z=~probclass, type = "contour"))
}

subplot(img_er(1),img_er(2),img_er(3))


