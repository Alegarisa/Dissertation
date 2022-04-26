# Notes that I didn't want to simply delete

--------------------------
  ## EFA decisions 
--------------------------

# Structure at home
scale_1 <- efa_vars %>%
  select(q23_p1:q30_p1)

# EFA using traditional method: pearson correlation & principal axis factoring extraction method
round(cor(scale_1, method = "pearson", use = "complete.obs"), digits = 2) # pearson corr

fa(scale_1, n.obs = NA, rotate = "none", fm = "pa", cor = "cor") #checking if output is different if using the raw data, not the matrix

# checking polychoric correlation
poly_scale_1 <- polychoric(scale_1) # applying poly function to scale 1

poly_scale_1 # RHO is the polychoric matrix, TAU is the "items difficulties" (?)

poly_scale_1_mat <- data.frame(poly_scale_1$rho) # need to create a dataframe with correlation matrix to use it in scree function 

scree(poly_scale_1_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# how to ask for estimation method ULS or ML --- answer: fm = "uls"

# this is the code copied from psych package documentation 
fa(poly_scale_1_mat, nfactors=1, n.obs = NA, n.iter=1, rotate = "oblimin", scores = "regression", residuals=FALSE, SMC=TRUE, covar=FALSE, missing=FALSE, impute="median", min.err = 0.001, max.iter = 50, symmetric=TRUE, warnings=TRUE, fm="uls", alpha=.1, p=.05, oblique.scores=FALSE, np.obs=NULL,use="pairwise", cor="poly", correct=.5, weight=NULL, n.rotations=1, hyper=.15)

# need to check if adding cor = "poly" is duplicative if the polychoric correlation matrix is provided instead of the dataframe with the items. Test both and see if results differ. 

fa(scale_1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly")

fa(poly_scale_1_mat, n.obs = NA, rotate = "none", fm = "uls")

# conclusion: using the raw data frame with cor = "poly" appears to be the same as using the poly corr matrix without the cor = "poly", thus, using just the data frame with the scale variables

