#Principal Component Analysis and Factor Analysis 

# Import data
data <- read.csv("C:/Users/Microsoft/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.csv")
print(data)

# Pilih hanya variabel yang relevan berdasarkan KMO dan Factor Loadings
selected_vars <- c("Age", "JobLevel", "MonthlyIncome", "NumCompaniesWorked", 
                   "PercentSalaryHike", "PerformanceRating", "TotalWorkingYears", 
                   "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", 
                   "YearsWithCurrManager")

data_selected <- data[, selected_vars]

#Pre-processing
sum(is.na(data_selected))
p <- ncol(data_selected)
p

#Check KMO
library(psych)
r <- cor(data_selected)
KMO(r)

#Bartlett Test
bartlett.test(data_selected)

#---------Principal Component Analysis-----------

##manual----

scale_data = scale(data_selected)
r = cov(scale_data)
r
##Menghitung eigenvalue dan eigenvector
pc <- eigen(r)
pc$values
#pc

library(dplyr)
##Menghitung proporsi varians dan kumulatif
sumvar <- sum(pc$values)
propvar <- sapply(pc$values, function(x) x/sumvar)*100
cumvar <- data.frame(cbind(pc$values, propvar)) %>% mutate(cum = cumsum(propvar))
colnames(cumvar)[1] <- "value"
rownames(cumvar) <- paste0("PC",c(1:p))
print(cumvar)


# Hasil PCA
pc$vectors
scores <- as.matrix(scale_data) %*% pc$vectors
head(scores)

# PCA menggunakan prcomp
PCA.mod <- prcomp(scale_data)
summary(PCA.mod)
PCA.mod$rotation
head(PCA.mod$x)

# PCA menggunakan FactoMineR
pca_result <- PCA(scale_data, scale.unit = TRUE, graph = FALSE, ncp=p)

# Menampilkan ringkasan hasil PCA
pca_result$eig
pca_result$svd$V
pca_result$ind['coord']

# Visualisasi
fviz_eig(pca_result, addlabels = TRUE, ncp = p, barfill = "skyblue", barcolor = "darkblue", linecolor = "red")
fviz_pca_biplot(pca_result, geom.ind = "point", addEllipses = TRUE)

# Contribution plots
fviz_contrib(pca_result, choice = "var", axes = 1, top = 5) + ggtitle("PC1")
fviz_contrib(pca_result, choice = "var", axes = 2, top = 5) + ggtitle("PC2")
fviz_contrib(pca_result, choice = "var", axes = 3, top = 5) + ggtitle("PC3")

#--------- Factor Analysis (FA) ---------
varcov = cov(scale_data)
pc = eigen(varcov)

L1 = sqrt(pc$values[1])*pc$vectors[,1]
L2 = sqrt(pc$values[2])*pc$vectors[,2]
L3 = sqrt(pc$values[3])*pc$vectors[,3]

L = cbind(L1, L2, L3)
L

# Perform factor analysis 
fa <- fa(r = scale_data, covar = TRUE, nfactors = 3, rotate = "varimax") 

# Factor Loadings
load <- fa$loadings
print(load)

# Plot Factor Loadings
plot(load[,c(1,3)], type="n") 
text(load[,c(1,3)], labels=names(data_selected), cex=.7)

fa.diagram(load)



