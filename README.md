# hellow-world
#'Regularized class-association rules
#'@description Fit a generalized linear model in the rules space via maximum likelihood with Lasso penalty.
#'The regularization is computed over a grid of values or at a specifyed value for the regularization parameter lambda.
#'It handle only categorical data space.#'@import arules#'@importFrom glmnet glmnet#'@importFrom arules apriori
#'@importFrom arules inspect#'@param data: A categorical data set with binary response variable; each row is an observation vector.
#'@param y.rank: The rank of the response variable.
#'@param s: A user supplied value for the minimal support of an item set.
#'@param c: A user supplied value for the minimal confidence of class-association rules.
#'@param maxl: A user specified value for the maximal number of items per item set (default: 6 items).
#'@param lambd: A user supplied value, or sequence of values, of the penalty terme. If it is not provided, the programme generate automatically a sequence ranging from the value of lambda such that all the coefficients are zero down to value of lambda whose the deviance do not change from lambda to the next.
#'@return The function fits Lasso regularized logistic model with binary response on the rule space.
#'@examples
#'data(vote)
#'rcar.fit<-rcar(vote,y.rank=1, s=0.2, c=0.8)
#'100 RCAR models are fit on the rule space given by the 5084 rules mined using Apriori with minimum support and#'confidence thresholds of (20%, 80%)
#'
#'
#'inspect(head(rcar.fit$Rules))#'#'The first 6 rules are displayed, from left to right are shown for each CAR: the condition (lhs), the consequent(rhs),#'the support, the confidence, and the frequence of the records that apply to the CAR.
#'
#'
#'rcar.fit
#'
#'It shows from left to right the number of nonzero coefficients (Df), the percent (of null) deviance#'explained (dev) and the value of λ (Lambda).
#'
#'plot(rcar.fit$Model,xvar = "lambda", label = TRUE)
#'
#'displays the coefficients of the models fit on Congressional Voting data set using RCAR algorithm over a grids
#'of values of lambda. Each curve corresponds to a coefficient of a rule, it shows the variation of
#'coefficient against lambda. The number of retained rules in the model at the current lambda is indicated at
#'the axis above.
#'@export
rcar<-function(data, y.rank=1, s=0.3, c=0.7, maxl=6,lambd){    
attach(data,warn.conflicts=FALSE)    
y.levels<-levels(data[[y.rank]])    
oldw<-getOption("warn")    
options(warn=-1)    
rules<- arules::apriori(data,parameter = list(minlen=2,maxlen=maxl, supp=s, conf=c),appearance = list(rhs=paste(colnames(data)[[y.rank]], y.levels, sep="="),default="lhs"),control=list(verbose=FALSE))    
options(warn=oldw)    
X<-arules::is.superset(as(data,"transactions"),as(rules@lhs,"itemMatrix"))    
nr<-rules@lhs@data@Dim[[2]]    dimnames(X) <- list(NULL, paste("rule", c(1:nr), sep=""))    
if(missing(lambd)) {model<-glmnet::glmnet(X,data[[y.rank]],family="binomial",alpha =1)}    
else {model<-glmnet::glmnet(X,data[[y.rank]],family="binomial",alpha =1,lambda=lambd)}    
model$call<-"rcar(data, Support, Confidence, max.length, lambda)"    list(Rules=rules,Model=model)}
