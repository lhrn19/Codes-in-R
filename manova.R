#Punto 5---------------------------------------------------------------------------------------------------------------

t1 = matrix(c(2,3,7,2,7,9,2,5,1,5), nrow = 5)
t2 = matrix(c(3,2,9,2,4,4), nrow = 3)
t3 = matrix(c(1,7,4,3,4,2,9,2),nrow = 4) 

n1 = nrow(t1)
n2 = nrow(t2)
n3 = nrow(t3)

g = ncol(t1)

xi = matrix(c(t1[,1],t2[,1],t3[,1]),ncol = 1) #todas las observaciones de la variable x1 para calcular la media
yi = matrix(c(t1[,2],t2[,2],t3[,2]),ncol = 1)

xbarra = matrix(c(mean(xi), mean(yi)), ncol = 1)

#promedios de xi en cada tratamiento 

xb1 = matrix(c(mean(t1[,1]), mean(t1[,2])), ncol = 1)
xb2 = matrix(c(mean(t2[,1]), mean(t2[,2])), ncol = 1)
xb3 = matrix(c(mean(t3[,1]), mean(t3[,2])), ncol = 1)
#taos

tao1 = xb1 - xbarra
tao2 = xb2 - xbarra
tao3 = xb3 - xbarra

s1 = cov(t1) #matriz de covarianzas del tratamiento 1
s2 = cov(t2) #matriz de covarianzas del tratamiento 2
s3 = cov(t3)

W = (n1-1)*s1+ (n2-1)*s2+ (n3-1)*s3
B = n1*(tao1%*%t(tao1))+n2*(tao2%*%t(tao2))+n3*(tao3%*%t(tao3))
T = W+B

#estadistico
wilks = det(W)/det(T)


#tabla oneway manova

source = matrix(c("Tratamiento (H)", "Residual (E)", "Total"), nrow = 3)

SSCP_1_1 = matrix(c(B[1,1],W[1,1],T[1,1]), ncol=1)
SSCP_1_2 = matrix(c(B[1,2],W[1,2],T[1,2]), ncol=1)
SSCP_2_1 = matrix(c(B[2,1],W[2,1],T[2,1]), ncol=1)
SSCP_2_2 = matrix(c(B[2,2],W[2,2],T[2,2]), ncol=1)

df = matrix(c(g-1,n1+n2+n3-g,n1+n2+n3-1),nrow = 3)

tab = data.frame(source,SSCP_1_1,SSCP_1_2,SSCP_2_1,SSCP_2_2,df,wilks)



#b---
#Prueba de hipótesus








