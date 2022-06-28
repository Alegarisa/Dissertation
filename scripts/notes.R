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
### testing combining scales
### 
scale_test <- efa_vars %>%
  select(q54_p1, q56_p1, q57_p1, q64_p1, q65_p1)

# Poly corr matrix
poly_scale_test <- polychoric(scale_test)
poly_scale_test_mat <- data.frame(poly_scale_test$rho)

# scree plot
scree(poly_scale_test_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA 
fa(scale_test, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # heywood case WENT AWAY but a warning that factor scores are prob incorrect

alpha(scale_test) #.82

##############################
scale_test2 <- efa_vars %>%
  select(q52_p1, q54_p1, q55_p1, q56_p1, q57_p1, q59_p1, q60_p1)

# Poly corr matrix
poly_scale_test2 <- polychoric(scale_test2)
poly_scale_test2_mat <- data.frame(poly_scale_test2$rho)

# scree plot
scree(poly_scale_test2_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA 
fa(scale_test2, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # no heywood case, but matrix not positve definitive 

alpha(scale_test2) # .93

##############################
# scale_test.1<- efa_vars %>%
#   select(q54_p1, q56_p1, q57_p1, q59_p1, q60_p1, q64_p1, q65_p1)
# 
# # Poly corr matrix
# poly_scale_test.1 <- polychoric(scale_test.1)
# poly_scale_test.1_mat <- data.frame(poly_scale_test.1$rho)
# 
# # scree plot
# scree(poly_scale_test.1_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)
# 
# # EFA 
# fa(scale_test.1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # heywood case q59_p1


############################## using the juntos section
# scale_test.2 <- efa_vars %>%
#   select(q54_p1:q57_p1, q58_p1, q59_p1, q60_p1, q62_p1, q63_p1)
# 
# fa(scale_test.2, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # not a good approach either

###############
#alternative scale (fam-school comm + par-teach rel)
scale_11 <- efa_vars %>%
  select(q41_p1, q43_p1, q50_p1, q62_p1, q64_p1, q65_p1, q66_p1, q67_p1) # matrix not positive definitive, but can use items 66 & 67

###############
#alternative proposal Parent endorsement of School (7)(makes more sense face value):
scale_4.1 <- efa_vars %>%
  select(q52_p1, q54_p1:q57_p1, q59_p1, q60_p1)

# Poly corr matrix
poly_scale_4.1 <- polychoric(scale_4.1)
poly_scale_4.1_mat <- data.frame(poly_scale_4.1$rho)

# scree plot
scree(poly_scale_4.1_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA
fa(scale_4.1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # no heywood, but now says matrix non-positive defintive which I guess is improvement (?)

fa.parallel(scale_4.1, n.obs=NULL, fm="uls", fa="fa", nfactors=1, main="Parallel Analysis Scree Plots", n.iter=20, error.bars=FALSE, se.bars=FALSE, SMC=FALSE, ylabel=NULL, show.legend=TRUE, sim=TRUE, quant=.95, cor="poly", use="pairwise", plot=TRUE, correct=.5) # parrallel analysis suggest 2 factors

# 2 factors
fa(scale_4.1, nfactors = 2, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly") # 
####
####
####
### ---------------------------------------------- ###

--------------------
  EFA COMBINING PARENT ENDORSEMENT, PARENT-TEACH REL, AND FAMILY-SCHOOL COMMUNICATION. 
q54_p1:q57_p1 --> parent endorsement
q59_p1, q60_p1 --> extra from belong 
q64_p1:q67_p1 --> parent-teacher rel
q18_p1, q19_p1, q41_p1, q43_p1, q50_p1, q62_p1 --> communication

```{r}
test_scale <- efa_vars %>%
  select(q18_p1, q19_p1, q41_p1, q43_p1, q50_p1, q54_p1, q55_p1, q56_p1, q57_p1, q59_p1, q60_p1, q62_p1, q64_p1, q65_p1, q66_p1, q67_p1)

# Poly corr matrix
poly_test_scale <- polychoric(test_scale)
poly_test_scale_mat <- data.frame(poly_test_scale$rho)

# scree plot
scree(poly_test_scale_mat,factors=TRUE,pc=FALSE,main="Scree plot",hline=NULL,add=FALSE)

# EFA
fa(test_scale, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # says matrix not positive definite

# ITERATION 1
test_scale1 <- efa_vars %>%
  select(q41_p1, q43_p1, q50_p1, q54_p1, q55_p1, q56_p1, q57_p1, q59_p1, q60_p1, q62_p1, q64_p1, q65_p1, q66_p1, q67_p1) # removing 18 and 19 because loading less than .30

# EFA ITERATION 1
fa(test_scale1, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # says matrix not positive definite
```

```{r}
fa.parallel(test_scale1, n.obs=NULL, fm="uls", fa="fa", nfactors=1, main="Parallel Analysis Scree Plots", n.iter=20, error.bars=FALSE, se.bars=FALSE, SMC=FALSE, ylabel=NULL, show.legend=TRUE, sim=TRUE, quant=.95, cor="poly", use="pairwise", plot=TRUE, correct=.5) # parallel analysis suggest 3 factors, matrix not positive, ultra heywood detected

# 3 factors
fa(test_scale1, nfactors = 3, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly") # says matrix not positive & ultra heywood: item 66 negative u2

# ITERATION 2
test_scale2 <- efa_vars %>%
  select(q41_p1, q43_p1, q50_p1, q54_p1, q55_p1, q56_p1, q57_p1, q59_p1, q60_p1, q62_p1, q64_p1, q65_p1, q67_p1) # removing item 66 with ultra heywood

# 3 factors (without 66) - ITERATION 2
fa(test_scale2, nfactors = 3, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly") # now just matrix not positive definite warning; crossloading item 64

# ITERATION 3
test_scale3 <- efa_vars %>%
  select(q41_p1, q43_p1, q50_p1, q54_p1, q55_p1, q56_p1, q57_p1, q59_p1, q60_p1, q62_p1, q65_p1, q67_p1) # removing crossloading item 64

# 3 factors (without 64) - # ITERATION 3
fa(test_scale3, nfactors = 3, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly") # now just matrix not positive definite warning; crossloading item 59

# ITERATION 4
test_scale4 <- efa_vars %>%
  select(q41_p1, q43_p1, q50_p1, q54_p1, q55_p1, q56_p1, q57_p1, q60_p1, q62_p1, q65_p1, q67_p1) # removing crossloading item 64

# 3 factors (without 59) - # ITERATION 4
fa(test_scale4, nfactors = 3, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly") # now just matrix not positive definite warning
```


```{r}
#reliabilities on iteration 4

# factor 1 yellow (4 items)
test_scale4_fa1 <- efa_vars %>%
  select(q55_p1, q56_p1, q57_p1, q62_p1)

alpha(test_scale4_fa1) #.91

# factor 2 blue (4 items)
test_scale4_fa2 <- efa_vars %>%
  select(q43_p1, q50_p1, q54_p1, q60_p1)

alpha(test_scale4_fa2) #.84

# factor 3 green (3 items)
test_scale4_fa3 <- efa_vars %>%
  select(q41_p1, q65_p1, q67_p1)

alpha(test_scale4_fa3) #.75
```

--------------------
  
  ----
  ----
  EFA COMBINING PARENT ENDORSEMENT & PARENT BELONGING
q47_p1, q48_p1, q51_p1, q52_p1, q53_p1 --> PARENT BELONGING
q54_p1:q57_p1 --> parent endorsement

```{r}
# rel with school
test_scale_sch <- efa_vars %>%
  select(q47_p1, q48_p1, q51_p1, q52_p1, q53_p1, q54_p1, q55_p1, q56_p1, q57_p1) # 

# EFA ITERATION 1
fa(test_scale_sch, n.obs = NA, rotate = "none", fm = "uls", cor = "poly") # matrix not positive definite, but looks good
```

```{r}
fa.parallel(test_scale_sch, n.obs=NULL, fm="uls", fa="fa", nfactors=1, main="Parallel Analysis Scree Plots", n.iter=20, error.bars=FALSE, se.bars=FALSE, SMC=FALSE, ylabel=NULL, show.legend=TRUE, sim=TRUE, quant=.95, cor="poly", use="pairwise", plot=TRUE, correct=.5) # parallel analysis suggest 2 factors, matrix not positive, ultra heywood detected

# 2 factors
fa(test_scale_sch, nfactors = 2, n.obs = NA, rotate = "quartimin", fm = "uls", cor = "poly")
```