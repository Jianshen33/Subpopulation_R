#### k means ####


library(factoextra)
library(NbClust)
library(GGally)
library(ConsensusClusterPlus)
library(readbitmap)
library(openxlsx)

rm(list= ls())


# consensus clustering: 

# tumor
mat1 <- read.csv("G:/1. Project/20220221 Lung metastasis/Data/20221017 Sub Metastasis Primary Lung metastasis HMDB & Gene.csv", header=T, sep=",", dec = ".")

mat1<- data.frame(mat1[,-1], row.names=mat1[,1])
mat1 <- t(mat1) 

mat1 <-na.omit(mat1)
mat1 = sweep(mat1 ,1, apply(mat1 ,1,median)) 
d = as.matrix(mat1) 
d[1:5,1:5] 

# tumor
rcc3 = ConsensusClusterPlus(d,maxK=10,reps=1000,pItem=0.8,pFeature=1,title="Sub Tumor intensity_consensus s hc",distance="spearman", verbose=T, clusterAlg="hc",seed=123,plot="pdf")
icl <- calcICL(rcc3, title = title,plot = "png")
da1 <- as.data.frame(rcc3[[3]][["consensusClass"]])
write.csv(da1, file="G:/1. Project/20220221 Lung metastasis/Data/Sub/Result Sub Lung metastasis.csv")

# tumor
mat1 <- read.csv("G:/1. Project/Sub PID AVE  Tumor Metastasis Lung metastasis.csv", header=T, sep=",", dec = ".")

mat1<- data.frame(mat1[,-1], row.names=mat1[,1])

df <-na.omit(mat1)   
df= sweep(df,2, apply(df,2,median))
set.seed(123)
#################################################
k4 <- kmeans(df,2, nstart = 100)   # change k value
#################################################
k4$size

PCA <- fviz_cluster(k4, geom = "point", data = df, axes = c(1, 2), palette = "Set2", ggtheme = theme_minimal())

svg(file="PCA.svg", width=12, height=8)        
plot(PCA)
dev.off()

dd <- cbind(cluster = k4$cluster,df)         
# tumor
write.xlsx(dd, row.names=TRUE,file="E:\\1.2_kmeans_tumor_intensity.xlsx")

