# Notes that I didn't want to simply delete

# inspecting linearity (well, trying)
ggplot(efa_vars, aes(q23_p1, q24_p1)) +
  geom_count() +
  geom_jitter(h = 2, w = 2)

# plots2 <- split.default(scale_1, names(scale_1)) %>% 
#   map(., setNames, nm = "var") %>% 
#   map(., rownames_to_column) %>%
#   imap(., ~ {
#     ggplot(.x, aes(rowname, var)) + 
#       geom_count() + 
#       coord_flip() +
#       labs(title = .y)
#     }) # this is one is not clear

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

poly_scale_1_mat <- data.frame(poly_scale_1$rho) # needed to create a dataframe with correlation matrix to use it in scree function 

scree(poly_scale_1_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# how to ask for estimation method ULS or ML --- answer: fm = "uls"

# this is the code copied from psych package documentation 
fa(poly_scale_1_mat, nfactors=1, n.obs = NA, n.iter=1, rotate = "oblimin", scores = "regression", residuals=FALSE, SMC=TRUE, covar=FALSE, missing=FALSE, impute="median", min.err = 0.001, max.iter = 50, symmetric=TRUE, warnings=TRUE, fm="uls", alpha=.1, p=.05, oblique.scores=FALSE, np.obs=NULL,use="pairwise", cor="poly", correct=.5, weight=NULL, n.rotations=1, hyper=.15)

# need to check if adding cor = "poly" is duplicative if the polychoric correlation matrix is provided instead of the dataframe with the items. Test both and see if results differ. 

fa(scale_1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly")

fa(poly_scale_1_mat, n.obs = NA, rotate = "none", fm = "uls")

# conclusion: using the raw data frame with cor = "poly" appears to be the same as using the poly corr matrix without the cor = "poly".

#####
# Structure at home
scale_1 <- efa_vars %>%
select(q23_p1:q30_p1)

# scree plot
scree(scale_1,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA
fa(scale_1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # using the dataframe instead of the poly corr matrix gives more info, but the numbers are the same
###

### -----Parent Endorsement of School (4)----- ###

# EFA without q54_p1 = bec. it had a case with a value of 1 (outlier?)
scale_4.1 <- efa_vars %>%
  select(q55_p1, q56_p1, q57_p1)

fa(scale_4.1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly")

scree(scale_4.1,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA without q55_p1 = bec it had a negative u2 and a factor loading of 1
scale_4.2 <- efa_vars %>%
  select(q54_p1, q56_p1, q57_p1)

fa(scale_4.2, n.obs = NA, rotate = "none", fm = "uls", cor = "poly")

scree(scale_4.2,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA without q56_p1 = just to check what happens
scale_4.3 <- efa_vars %>%
  select(q54_p1, q55_p1, q57_p1)

fa(scale_4.3, n.obs = NA, rotate = "none", fm = "uls", cor = "poly")

scree(scale_4.3,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

### ---------------------------------------------- ###
