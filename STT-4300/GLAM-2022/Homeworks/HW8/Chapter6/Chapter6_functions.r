robGEE <- function(formula,cluster,data=sys.frame(sys.parnent()),corr="exchangeable",tuningc.y=1.345,weights.on.x = "none",kconst=sqrt(qchisq(0.9,2)),phi=1,Wald=NULL)
{
  # Thank you to Gaëlle Deville for a first translation of
  # this function from Splus to R.

  # RESTRICTION:
  # - cluster id. must be increasing, due to the behavior of split()
  cat("REMINDER: \n  Data are assumed to be sorted so that observations on a cluster are contiguous rows for all entities in the formula. Moreover, cluster number must be increasing. \n")

  # Construct X and y
  call <- match.call()
  m <- match.call(expand.dots = FALSE)
  m$model <- m$corr <- m$tuningc.y <- m$weights.on.x  <- m$kconst <- m$phi <- m$Wald <- m$... <- NULL
  if(is.null(m$cluster)) {
		   m$cluster <- as.name("cluster")
  }
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame()) # PROBLEM HERE
  Terms <- attr(m, "terms")
  y <- as.matrix(model.extract(m, response))
  X <- model.matrix(Terms, m)[,-1]
  cluster <- model.extract(m,cluster)

  # Variables
  mytol <- .Machine$double.eps^.25
  p <- dim(as.matrix(X))[[2]]+1
  Kn <- dim(as.matrix(X))[[1]]
  clusternumber <- length(unique(cluster))
  clustersize <- lapply(split(cluster,cluster),"length")
  maxclustersize <- as.integer(max(unlist(clustersize)))
  zero <- rep(0,times=Kn)
  one <- rep(1,times=Kn)
  alpha <- 0 #give the independence case
  Wx <- one

  # Preliminaries
  corrfunctions <- c("independence", "exchangeable")
  if(as.integer(match(c(corr), corrfunctions, -1))<1)
    stop("unknown correlation structure")
  xweights <- c("none","hat")
  if(as.integer(match(c(weights.on.x), xweights, -1))<1)
    stop("unknown weights.x")
  if(length(cluster) != length(y))
    stop("cluster and y not same length")

  # Auxiliary functions
  Huberweighting <- function(yy,mumu,AA,cc,phi)
    {
    rr <- (yy-mumu)/AA/sqrt(phi)
    ifelse(abs(rr)<cc,1,cc/abs(rr))
    }

  redescweighting <- function(yy,mumu,AA,aa,phi)
    {
    rr <- (yy-mumu)/AA/sqrt(phi)
    exp(-(rr/aa)^2)
    }

  doublesum <- function(vec)
    {
    1/2*(sum(vec%*%(t(vec)))-sum(vec^2))
    }

  makeRexch <- function(vec,aa)
    {
    if(length(vec)==1)
      {
      as.matrix(1)
      }
    else
      {
      matrix(aa,ncol=length(vec),nrow=length(vec)) + diag(rep(1-aa,length(vec)))
      }
    }

  makeRilist <- function(aux.list,aux.cov)
    {
    aux.cov
    }

  makevar <- function(vec)
    {
    vec%*%t(vec)
    }

  res.lag <- function(vec)
    {
    therow <- row (diag (length(vec)))
    thecol <- col (diag (length(vec)))
    updiag <- therow < thecol
    cbind(vec[therow[updiag]], vec[thecol[updiag]])
    }


  # Initialisation (independence assumed)
  beta.old <- as.vector(glm(formula,data,family=binomial)$coeff)
  Xwith <- cbind(1,X)

  if(corr=="independence")
    {
    R <- diag(one)
    }

# MAIN
  repeat
    {
    eta <- Xwith%*%beta.old
    mu <- exp(eta)/(1+exp(eta))
    dmudeta <- exp(eta)/((1+exp(eta))^2)
    A <- as.vector(sqrt(mu*(1-mu)))
    r.stand <- (y-mu)/A/sqrt(phi)
    r.nonstand <- (y-mu)/A

    psi <- Huberweighting(y,mu,A,tuningc.y,phi)*(y-mu)
    a.const <- (Huberweighting(one,mu,A,tuningc.y,phi) -
               Huberweighting(zero,mu,A,tuningc.y,phi))*A^2
    rstar <- (psi-a.const)/A
    bb <- ((1-mu)*Huberweighting(one,mu,A,tuningc.y,phi) +
          mu*Huberweighting(zero,mu,A,tuningc.y,phi))*Wx


    if(corr=="exchangeable")
      {
      # M-estimator of covariance:
      alpha.old <- alpha
      pairwise.res <- lapply(split(r.nonstand,cluster),res.lag)
      laggedres <- pairwise.res[[1]]
      for(i in 2:clusternumber)
        {
          laggedres <- rbind(laggedres,pairwise.res[[i]])
        }
##      starting.mve <- cov.mve(laggedres,print=F)
##      cov.rob <- starting.mve$cov
##      tt <- starting.mve$center
      tt <- apply(laggedres,2,mean)
      cov.rob <- var(laggedres)
      bias.corr <- pchisq(kconst^2,4)+kconst^2/2*(1-pchisq(kconst^2,2))

      weights.rob <- function(dd,kk)
        {
        ifelse(dd<kk,1,kk/dd)
        }

      repeat
        {
         mah.dist <- sqrt(mahalanobis(laggedres,center=tt,cov=cov.rob))
         tt <- apply(weights.rob(dd=mah.dist,kk=kconst)*laggedres,2,sum)/sum(weights.rob(dd=mah.dist,kk=kconst))
         cov.rob <- t(sweep(laggedres,2,tt))%*%((weights.rob(dd=mah.dist,kk=kconst^2))*sweep(laggedres,2,tt))/bias.corr/(nrow(laggedres)-2)

# ECR: modification January 2005:
# uses parametrized cov.rob as in Cantoni (2004, p. 171) and no centering
#         cov.rob <- matrix(c(1,alpha.old,alpha.old,1),ncol=2)
#         mah.dist <- sqrt(mahalanobis(laggedres,center=rep(0,2),cov=cov.rob))
         alpha.new <- (t(sweep(laggedres, 2, tt))%*%((weights.rob(dd=mah.dist,kk=kconst^2))*sweep(laggedres, 2, tt))/bias.corr/(nrow(laggedres)-2))[1,2]
         if(abs(max(alpha.old-alpha.new))/abs(max(alpha.old))<mytol) break
         alpha.old <- alpha.new
         }
      alpha <- alpha.new
      Ri <- lapply(split(cluster,cluster),makeRexch,aa=alpha)
    }

    Dstar <- as.vector(1/dmudeta)

    # Weighted linear regression
    Gamma <- -as.vector(bb)
    invRi <- lapply(Ri,solve)
    invVi <- lapply(1:clusternumber,function(i,list1,list2){t(1/list1[[i]]*t(1/list1[[i]]*list2[[i]]))},list1=split(A,cluster),list2=invRi)
    Wstari <- lapply(1:clusternumber,function(i,list1,list2){t(list1[[i]]*t(list1[[i]]*list2[[i]]))},list1=split(Gamma/Dstar,cluster),list2=invVi)
    Xwithi <- lapply(split(Xwith,cluster), function(x) matrix(x, ncol = p))
    XDinvVinvDinvXi <- lapply(1:clusternumber,function(i,list1,list2){t(list1[[i]])%*%list2[[i]]%*%list1[[i]]},list1=Xwithi,list2=Wstari)

    # Sum of the list components
    XDinvVinvDinvX <- matrix(apply(matrix(unlist(XDinvVinvDinvXi),p*p,clusternumber),1,sum),p,p)

    if(weights.on.x=="hat")
      {
      # This computes more than the hat matrix, but the diagonal blocks
      # are the correct H_i blocks
      invXDVDX <- solve(XDinvVinvDinvX)
      HHi <- lapply(1:clusternumber,function(i,list1,list2,mat){diag(list1[[i]]%*%mat%*%t(list1[[i]])%*%list2[[i]])},list1=Xwithi,list2 = Wstari, mat=invXDVDX)
      HH <- unlist(HHi)
      Wx <- sqrt(1-HH)
    }

    XDinvVinvDinvZi <- lapply(1:clusternumber,function(i,list1,list2,list3,list4){t(list1[[i]])%*%(list2[[i]]*list3[[i]]%*%list4[[i]])},list1=Xwithi,list2=split(Gamma/Dstar,cluster),list3=invVi,list4=split(Gamma/Dstar*(eta-Dstar*(1/Gamma)*Wx*(psi-a.const)),cluster))

    # Sum of the list components
    XDinvVinvDinvZ <- apply(matrix(unlist(XDinvVinvDinvZi),p,clusternumber),1,sum)

    beta.new <- solve(XDinvVinvDinvX,XDinvVinvDinvZ)
    if(abs(max(beta.old-beta.new))/abs(max(beta.old)) < mytol) break
    beta.old <- as.vector(beta.new)
  }
  names(beta.old) <- c("Intercept",dimnames(X)[[2]])

  w.r <- psi/(y-mu)
  w.x <- Wx

  # Asymptotic variance
  Gamma <- -as.vector(bb)
  if(corr=="independence")
    {
    var.psi <- diag(as.vector(bb^2*mu*(1-mu)))
    }
  else
    {
    var.list <- lapply(split(psi-a.const,cluster),makevar)
    }

  AKi <- lapply(1:clusternumber,function(i,list1,list2,list3){t(list1[[i]])%*%(list2[[i]]*list3[[i]])%*%(list2[[i]]*list1[[i]])},list1=Xwithi,list2=split(-Gamma/Dstar,cluster),list3=invVi)
  GKi <- lapply(1:clusternumber,function(i,list1,list2,list3,list4,list5){t(list1[[i]])%*%(list2[[i]]*list3[[i]])%*%(list4[[i]]*list5[[i]])%*%(list4[[i]]*list3[[i]])%*%(list2[[i]]*list1[[i]])},list1=Xwithi,list2=split(-Gamma/Dstar,cluster),list3=invVi,list4=split(Wx,cluster),list5=var.list)

  # Sum of the list components
  matrix(apply(matrix(unlist(XDinvVinvDinvXi),p*p,clusternumber),1,sum),p,p)
  AK <- matrix(apply(matrix(unlist(AKi),p*p,clusternumber),1,sum),p,p)
  GK <- matrix(apply(matrix(unlist(GKi),p*p,clusternumber),1,sum),p,p)

  AK.inv <- solve(AK)
  as.var <- AK.inv%*%GK%*%t(AK.inv)
  as.sd <- sqrt(diag(as.var))
  names(as.sd) <- c("Intercept",dimnames(X)[[2]])


  robGEEres <- list(coeff=beta.old,mu=mu,phi=phi,alpha=alpha,as.sd=as.sd,w.r=w.r, w.x=w.x,AK=AK,GK=GK,formula=formula)
  class(robGEEres) <- c("robGEE")
  robGEEres
}


print.robGEE <- function(obj)
  {
    newobj <- cbind(obj$coeff,obj$as.sd,obj$coef/obj$as.sd,2*(1-pnorm(abs(obj$coef/obj$as.sd))))
   dimnames(newobj)[[2]] <- c("Coefficients", "Standard Errors", "z-stat","p-value")
    cat("\n")
    printCoefmat(newobj, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), has.Pvalue=TRUE, P.values = TRUE)

    cat("\n Exchangeable correlation: \n")
    print(obj$alpha)
  }


anovarobGEE <- function(obj1,obj2,data)
{

# The following lines are adapted from robustbase:::anovaGlmrobPair
  if (length(obj1$coef) < length(obj2$coef)) {
    full.fit <- obj2
    full.formula <- obj2$formula
    reduced.fit <- obj1
    reduced.formula <- obj1$formula
  }
  else {
    full.fit <- obj1
    full.formula <- obj1$formula
    reduced.fit <- obj2
    reduced.formula <- obj2$formula
  }

  X <- model.matrix(full.formula,data=data)
  asgn <- attr(X, "assign")
  tt <- terms(full.formula)
  tt0 <- terms(reduced.formula)
  tl <- attr(tt, "term.labels")
  tl0 <- attr(tt0, "term.labels")
  numtl0 <- match(tl0, tl, nomatch = -1)
  if (attr(tt0, "intercept") == 1)
      numtl0 <- c(0, numtl0)
  if (any(is.na(match(numtl0, unique(asgn)))))
      stop("Models are not nested!")
  mod0 <- seq(along = asgn)[!is.na(match(asgn, numtl0))]
  if (length(asgn) == length(mod0))
      stop("Models are not strictly nested")
  H0ind <- setdiff(seq(along = asgn), mod0)
  H0coef <- full.fit$coeff[H0ind]
  df <- length(H0coef)
  pp <- df + length(mod0)
  matM <- full.fit$AK
  matM11 <- matM[mod0, mod0]
  matM12 <- matM[mod0, H0ind]
  matM22 <- matM[H0ind, H0ind]
  matM22.1 <- matM22 - crossprod(matM12, solve(matM11)%*%matM12)
  Dquasi.dev <- c(H0coef%*%matM22.1%*%H0coef)
  matQ <- full.fit$GK
  matM11inv <- solve(matM[mod0, mod0])
  Mplus <- matrix(0, ncol = pp, nrow = pp)
  Mplus[mod0, mod0] <- matM11inv
  d.ev <- Re(eigen(matQ %*% (solve(matM) - Mplus))$values)
  d.ev <- d.ev[1:df]
  if (any(d.ev < 0))
     warning("some eigenvalues are negative")
  statistic <- c(quasi.dev = Dquasi.dev) #/mean(d.ev)) this only used in pvalue

quasirobGEEres <- c(nobs=nrow(X),p=pp,df=df, stat=statistic, pval=pchisq(as.vector(statistic/mean(d.ev)), df = df, lower.tail = FALSE),formula1=obj1$formula,formula2=obj2$formula)
  class(quasirobGEEres) <- "quasirobGEE"
  quasirobGEEres
  }

print.quasirobGEE <- function(obj)
  {
    title <- "Robust Quasi-Deviance Table Based on a Quadratic Approximation"
    topnote1 <- paste("Model1:",paste(deparse(obj$formula1)))
    topnote2 <- paste("Model2:", paste(deparse(obj$formula2)))
    tbl <- matrix(rep(NA, 8), ncol = 4)
    tbl[1, 1] <- obj$nobs-obj$p
    tbl[2,] <- c(obj$nobs-obj$p-obj$df,obj$stat,obj$df,obj$pval)
    dimnames(tbl) <- list(1:2, c("pseudoDf", "Test.Stat", "Df", "Pr(>chisq)"))
    print(structure(as.data.frame(tbl), heading = c(title, "", topnote1,
        topnote2, ""), class = c("anova", "data.frame")))
}