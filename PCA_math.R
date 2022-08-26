#create the dataframe 3 variables for 5 observations
M<-data.frame(country=c("USA","ITALY","RUSSIA","INDIA","CHINA"),
                   Potato=c(50,36,113,25,42),
                   Rice=c(11,9,8,98,127),
                   Meat=c(99,	91,61,3,50))

# plot them in 3D space
library(plotly)
p<-plot_ly(M,x= ~Potato,y=~Rice,z=~Meat, type="scatter3d", mode="text",text=~country)
p <- p %>% add_markers()
show(p)

# covariance matrix 
covM <-cov(M[-1])

# total variance is the sum of diagonl elements of cov(M) and it is equal to the sum of eigen values of cov(M)
#eigen values and vectors
A<-eigen(covM)
lambda<-A$values
lambda

# eigen vectors : these are the same as prcomp rotaion matrix
v<-A$vectors
v

#To calculate PC's
PC<-as.matrix(M[-1])%*%v

#plot PC2 vs PC1
y<-as.data.frame(PC)
r<-cbind(M[1],y)
ggplot(r, aes(x=V1, y=V2,label=country))+geom_point()+geom_text(hjust=0, vjust=0)

#**********************************************************************************************
prcomp_pca <- prcomp(M[,c(2:4)], center = T,scale. = T) #scale to 0 mean and unit variance
# PCs
prcomp_pca$x
#**********************************************************************************************
# the same can be obtained if the data is initially scaled
Mscaled<-apply(M[-1], 2, function(x) (x - mean(x)) / sd(x))
covMs <-cov(Mscaled)
As<-eigen(covMs)
PCs<- t(t(As$vectors) %*% t(Mscaled))

# pca can be computed using correlation matrix or covariance matrix. however correlation matrix accounts for scaling
# and it is considered as more balanced. cov(M[-1]) and cor(Mscaled) would give the same results as well.


  
