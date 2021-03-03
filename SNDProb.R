trainInitialModel.parametricNovelty <- function(data, percentile, method="gaussian", eps=.001, ...){
  if("class" %in% colnames(data)){
    if(method == 'lognormal'){
      print('Transforming lognormal data to normal data doing log(x)')
      data <- data.frame(log(data[, -which(colnames(data)=='class')]), 'class'=data[, which(colnames(data)=='class')])
      method <- 'gaussian'
    }
    if(method == "gaussian"){
      data.without.class <- data[, -which(colnames(data)=="class")]
      dTrain <- split(data.without.class, data$class)
      models <- lapply(1:length(dTrain), function(i){
        mdl<-list('means'=matrix(colMeans(dTrain[[i]]), nrow=1),
                  'sigma'=var(dTrain[[i]]),
                  'invCov' = MASS::ginv(var(dTrain[[i]])),
                  'N'=nrow(dTrain[[i]]),
                  'ImportanceN'=nrow(dTrain[[i]]),
                  'boundary'=qchisq(percentile, df=ncol(dTrain[[i]])),
                  'toWhoClassify'=i)
        while(base::det(mdl$sigma) < eps){
          mdl$sigma <- mdl$sigma + (diag(nrow(mdl$sigma)) * 0.00001)
        }
        return(mdl)
      })
      names(models) <- c(1:length(dTrain))
      return(models)
    }else
      stop("Provide a valid method: gaussian, mixtureGaussians, or copula")
  }else
    stop("There is no column named class")
}

predict.parametricNovelty <- function(data, models, quarantine, quarantineInstances, old.preds, maxBuffer, method="gaussian", nClustersToSeek, regression.model, epoch.start, export.path='./'){
  if(class(data) != 'matrix')
    data <- matrix(data, nrow=1)
  if(method == 'lognormal'){
    print('Transforming lognormal data to normal data doing log(x)')
    data <- log(data)
    method <- 'gaussian'
  }
  if(method=='gaussian'){
    res <- matrix(sapply(models, function(m){
      dst <- stats::mahalanobis(x=unlist(data), center=m$means, cov=m$invCov, inverted = T)
      if(dst <= m$boundary)
        return(dst)
      else
        return("Candidate") # Candidate for new class
    }), ncol = length(models))
    res.aux <- res
    res.aux[which(res.aux == 'Candidate')] <- Inf
    res.candidate <- which(res == 'Candidate')
    dst <- as.numeric(res.aux)
    if(all(is.infinite(dst))){ # It does not fall into any mixture percentile
      if(!identical(res.candidate, integer(0))){
        # return(-1) # Predict as candidate
        a <- -1
      }else{
        # return(NaN) # If outlier to at least one, outlier
        a <- -2
      }
    }else{
      # return(which.min(dst)) # Predict as index of that model
      a <- models[[which.min(dst)]]$toWhoClassify
    }
    preds <- c(old.preds, a) # Old predictions + the new one
    r <- which(preds == -1) # Get all the predictions classified as novelty
    quarantine <- unique(c(quarantine, r))
    if(length(quarantine) > nrow(quarantineInstances)){ # New Instances to quarantine
      quarantineInstances <- rbind(quarantineInstances, data)
    }
    updated.res <- update.parametricNovelty(data = data, quarantine = quarantine, quarantineInstances = quarantineInstances, preds = preds, maxBuffer = maxBuffer, models = models, nClustersToSeek = nClustersToSeek, regression.model = regression.model, export.path = export.path, epoch.start = epoch.start)
    models <- updated.res$models
    quarantine <- updated.res$quarantine
    quarantineInstances <- updated.res$quarantineInstances
    preds <- updated.res$preds
  }else
    stop("Select a valid distribution family: gaussian (by default) or lognormal")
  return(list('models'=models, 'preds'=preds, 'quarantine'=quarantine, 'quarantineInstances'=quarantineInstances))
}

update.parametricNovelty <- function(data, quarantine, quarantineInstances, preds, maxBuffer, models, nClustersToSeek, regression.model, epoch.start, method='gaussian', export.path = './'){
  if(method=='gaussian'){
    if(length(quarantine) >= maxBuffer){
      print("Discovering new class... This can take several minutes")
      ## Do we have to remove a class?
      my.preds <- preds[IX_SEQ:EX_SEQ]
      my.preds <- my.preds[my.preds != -1]
      count.preds <- rep(0, max(sapply(models, function(m) m$toWhoClassify)))
      count.preds[as.numeric(names(table(my.preds)/length(my.preds)))] <- table(my.preds)/length(my.preds)
      to.delete <- which(count.preds <= 0.05)
      ix.to.delete <- which(sapply(models, function(m) m$toWhoClassify) %in% to.delete)
      if(length(ix.to.delete) > 0 ){
        print(paste0("WE DELETE CLASS ", ix.to.delete))
        models <- models[-ix.to.delete]
        names(models) <- c(1:length(models))
      }

      #### This part of code is how we sweep the weights of the metaregressor.
      #### A number of weights are tested, the set highest accuracy is used to build the dataset
      #### For more info, refer to the paper. Section 2.3
      # res.barrido <- barridoPesos(models, preds, quarantine, quarantineInstances, nClustersToSeek, maxBuffer )
      # models <- res.barrido$models
      # quarantine <- res.barrido$quarantine
      # quarantineInstances <- res.barrido$quarantineInstances
      # preds <- res.barrido$preds

      if(length(models) > 1)
        weights.models <- getWeightsPredict(regression.model, models, preds[epoch.start:length(preds)], quarantineInstances)
      else
        weights.models <- list('weights'=c(250), 'cd'=0)
      print("==> The predicted weights by the meta-regressor are: ")
      print(weights.models$weights)
      for(ix.model in 1:length(models)){
        models[[ix.model]]$ImportanceN <- unlist(weights.models$weights)[ix.model]
      }
      if(weights.models$cd == 1){
        nClustersToSeek <- c(0)
      }else{
        nClustersToSeek <- c(0:nClustersToSeek)
      }

      # Discover new classes: Call the function that runs the EM and discovers new classes
      cluster <- discoverNewClass(models = models, quarantine = quarantine, quarantineInstances = quarantineInstances, nClustersToSeek = nClustersToSeek, maxBuffer = maxBuffer, export.path = export.path)
      preds[quarantine] <- cluster$observations
      models <- cluster$models
      quarantine <- c() # Delete quarantine for future elements
      quarantineInstances <- quarantineInstances[0, ] # Delete quarantine keeping structure for future elements
    }else{
      # Update concept drift
      for (m in 1:length(models)) {
        model <- models[[m]]
        dToUpdate <- matrix(data[which(tail(preds, nrow(data)) == models[[m]]$toWhoClassify), ], nrow=1)
        if(length(dToUpdate) > 0){
          mean <- updateMean(xbar = model$means, x = unlist(dToUpdate), n = model$ImportanceN)
          s <- updateCovariance(C = model$sigma, x = unlist(dToUpdate), n = model$ImportanceN, xbar = t(model$means))
          while(base::det(s) <= 1e-10){
            s <- s + diag(ncol(s)) * 1e-3
          }
          models[[m]] <- list('means'=mean,
                              'sigma'=s,
                              'invCov'=MASS::ginv(s),
                              'boundary'=model$boundary,
                              'N'=(model$N + nrow(dToUpdate)),
                              'ImportanceN' = (model$ImportanceN + nrow(dToUpdate)),
                              'toWhoClassify' = model$toWhoClassify )
        }
      }
    }
    return(list('models'=models, 'quarantine'=quarantine, 'quarantineInstances' = quarantineInstances, 'preds'=preds))
  }else
    stop("Provide a valid method: 'gaussian' or 'lognormal'")
}

# This is the function that finds the best weight set for the current situation
barridoPesos <- function(models, preds, quarantine, quarantineInstances, nClustersToSeek, maxBuffer){
  # This function is used to generate the dataset for learning the metaregressor.
  combinations <- expand.grid(1:length(models), 1:length(models))
  combinations <- combinations[combinations[, 2] > combinations[, 1], ]
  overlap <- apply(combinations, MARGIN=1, function(pair){
    montecarlo.overlap(list('1'=models[[pair[1]]], '2'=models[[pair[2]]]))
  })
  if (length(models) == 2){
    if(any(overlap > 0.30)){
      matrix.barrido <- getweights(length(models), seq(5.7, 12, 1.3))
    }else{
      matrix.barrido <- getweights(length(models), seq(4.3, 8, 0.8))
    }
  }else if(length(models) == 3){
    if(any(overlap > 0.30)){
      matrix.barrido <- getweights(length(models), c(5, 5.5, 6.5, 7, 7.5))
    }else{
      matrix.barrido <- getweights(length(models), c(4.3, 4.7,  5.5, 6.2, 6.7))
    }
  }else if(length(models) >= 4){
    if(any(overlap > 0.30)){
      matrix.barrido <- getweights(length(models), c(5, 6, 6.5, 7))
    }else{
      matrix.barrido <- getweights(length(models), c(4.2, 5, 5.7, 6.3))
    }
  }

  matrix.barrido <- getweights(length(models), c(2,3))
  print(paste0("Se prueban ", nrow(matrix.barrido$valid.w), " combinaciones de pesos"))
  list.weights <- split(matrix.barrido$valid.w, c(1:nrow(matrix.barrido$valid.w)))
  list.lambda <- split(matrix.barrido$valid.l, c(1:nrow(matrix.barrido$valid.l)))
  time.dir <- format(Sys.time(), "%d_%H_%M_%s")
  dir.exp <- paste0('./', time.dir)
  if(dir.exists(dir.exp)){
    dir.exp <- paste0(dir.exp, sample(x = 1:99999, size = 1), '/')
  }else{
    dir.exp <- paste0(dir.exp, '/')
  }
  print(paste0("Se exporta en: ", dir.exp))
  dir.create(dir.exp)
  res.clustering <- mapply(w=list.weights, l=list.lambda, FUN=function(w, l){
    mls <- models
    for (m.ix in 1:length(mls)){
      mls[[m.ix]]$ImportanceN <- w[m.ix]
      mls[[m.ix]]$N <- w[m.ix]
    }

    cluster <- discoverNewClass(models = mls, quarantine = quarantine, quarantineInstances = quarantineInstances, nClustersToSeek = c(0:nClustersToSeek), maxBuffer = maxBuffer)
    cluster$params <- list('w' = w, 'l' = l)
    return(cluster)
  }, SIMPLIFY = F)

  # Here the best result of the greed search is gathered
  res.metrics <- lapply(res.clustering, FUN=function(cl.res){
    my.preds <- preds
    my.preds[quarantine] <- cl.res$observations
    my.real <- data.test[c(IX_SEQ:EX_SEQ), ncol(data.test)]
    conf.matrix <- conf.matrix.create(testReal = my.real, predictions = my.preds, knownClasses = knownClasses)
    if(ncol(conf.matrix) > 1){
      if(ncol(conf.matrix) == nrow(conf.matrix)){
        sol <- solve_LSAP(conf.matrix, maximum = T)
        for (s in sol){
          my.preds[which(my.preds == s)] <- sol[s]
        }
      }
    }
    my.real <- as.numeric(my.real)
    my.preds <- as.numeric(my.preds)[c(IX_SEQ:EX_SEQ)]
    conf.matrix <- conf.matrix.create(testReal = my.real, predictions = my.preds, knownClasses = knownClasses)
    metrics <- data.frame('MissNew' = missNew(my.real, my.preds, knownClasses), # Percentage of new classes misclassified as existing
                          'FalseNew' = falseNew(my.real, my.preds, knownClasses), # Percentage of existing classes missclassified as new
                          # 'Error' = length(which(preds != my.real)) / length(preds), # Total missclassification error in % (MNew and FNew incl.)
                          'Error' = error.global(my.real, my.preds),
                          'CorrectBetweenKnown' = goodPredicted(my.real, my.preds, knownClasses), # Total missclassification among known classes in %
                          'Correct' = sum(diag(conf.matrix)) / sum(conf.matrix),
                          'EN_Accuracy' = en.accuracy(my.real, my.preds, c(knownClasses)),
                          'FMeasure' = fmeasure.newClasses(my.real, my.preds, knownClasses = c(knownClasses)),
                          'NModels' = length(cl.res$models))
    metrics$bic <- min(cl.res$bic)
    metrics$logLikelihood <- cl.res$loglik
    return(metrics)
  })


  for(m.ix in 1:length(res.metrics)){
    dets <- sapply(res.clustering[[m.ix]]$models, function(m) det(m$sigma))
    if(any(dets <= 1.5)) res.metrics[[m.ix]]$bic <- Inf
  }
  all.evaluated.bic <- sapply(1:length(res.metrics), function(m.ix){
    cat(paste0(m.ix, ")\tPesos: ", paste0(res.clustering[[m.ix]]$params$w, collapse = " "), "\t", "Accuracy: ", res.metrics[[m.ix]]$Correct, "\tBIC: ", res.metrics[[m.ix]]$bic, "\tModelos: ", length(res.clustering[[m.ix]]$models), "\n"))
    res.metrics[[m.ix]]$bic
  })
  if(all(all.evaluated.bic == Inf)){
    print("Solución no válida")
  }

  all.acc <- sapply(res.metrics, function(r) r$Correct)
  all.bic <- sapply(res.clustering, function(r) min(r$bic))
  best.acc <- which(max(all.acc) == all.acc)
  best.ix <- which(min(all.bic[best.acc]) == all.bic, arr.ind = T)

  cluster <- res.clustering[[best.ix]]
  export.features.regression <- paste0(dir.exp, '/features_regression', num.models.train, 'D.csv')
  if(length(cluster$models) > num.models.train){
    type.cd.nd <- 0 # ND
  }else{
    type.cd.nd <- 1 # CD
  }
  if(!all(all.evaluated.bic == Inf)){
    write.feature.vector(cluster$params$w, export.features.regression, type=type.cd.nd, models, quarantineInstances)
  }

  preds[quarantine] <- cluster$observations

  return(list('models' = cluster$models, 'preds' = preds, quarantine = c(), quarantineInstances = quarantineInstances[0,]))
}


discoverNewClass <- function(models, quarantine, quarantineInstances, nClustersToSeek, maxBuffer, method = 'gaussian', eps=5e-5, export.path='./'){
  strategy = 'exponential'
  quarantineInstances <- matrix(unlist(quarantineInstances), ncol=ncol(quarantineInstances))
  for (i in 1:length(models)){
    models[[i]]$means <- matrix(models[[i]]$means)
  }
  all.mixtures <- list()

  quarantineInstances.orig <- quarantineInstances
  for (k in nClustersToSeek){
    quarantineInstances <- quarantineInstances.orig
    # print(paste0("EM Alg -> K = ",k))
    if(k > 0){
      # Expectation step
      quarantineInstances.class <- sample(1:k, nrow(quarantineInstances), replace = T)
      if(method == 'mixtureGaussians'){
        mixtures <- lapply(split(as.data.frame(quarantineInstances), quarantineInstances.class), function(d){
          ix <- sample(x=1:nrow(d), size=1)
          list('means'=matrix(unlist(d[ix, ])),
               'sigma'=diag(ncol(quarantineInstances)),
               'N'=nrow(d),
               'ImportanceN' = 0,
               'boundary' = models[[1]]$boundary,
               'toWhoClassify' = NaN,
               'invCov' = MASS::ginv(var(d)),
               'weightK' = 1)
        })
      }else{
        mixtures <- lapply(split(as.data.frame(quarantineInstances), quarantineInstances.class), function(d){
          ix <- sample(x=1:nrow(d), size=1)
          list('means'= matrix(unlist(d[ix, ])),
               'sigma'=diag(ncol(quarantineInstances)),
               'invCov' = MASS::ginv(var(d)),
               'N'=nrow(d),
               'ImportanceN' = 0,
               'boundary'=models[[1]]$boundary,
               'toWhoClassify'=NaN)
        })
      }

      # We add the current models to the mixtures
      oldMixtureIndex <- (length(mixtures)+1):(length(mixtures)+length(models))
      mixtures <- append(mixtures, models)
      for(toWho in 1:(oldMixtureIndex[1]-1)){
        mixtures[[toWho]]$toWhoClassify <- max(sapply(mixtures, function(m) m$toWhoClassify), na.rm = T) + 1
      }
      names(mixtures) <- 1:length(mixtures)
      tau <- rep(1/(k+length(models)), k+length(models))
    }else{
      mixtures <- models
      oldMixtureIndex <- 1:length(mixtures)
      tau <- rep(1/length(mixtures), length(models))
    }
    originalMeans <- lapply(models, function(m) m$means)
    originalVariances <- lapply(models, function(m) m$sigma)
    logLikelihood.old <- 1
    logLikelihood <- 0
    error.old <- 1
    mixtures.old <- mixtures
    error <- 0
    max.ite <- 0

    sample.models=F
    if(sample.models){
      sampled.instances <- do.call(rbind, lapply(models, function(m) mvtnorm::rmvnorm(m$ImportanceN, mean = m$means, sigma = m$sigma)))
      quarantineInstances <- rbind(quarantineInstances.orig, sampled.instances)
      weights.vector <- rep(1, nrow(quarantineInstances))
    }else{
      quarantineInstances <- rbind(quarantineInstances, matrix(sapply(mixtures, function(m) m$means), ncol=length(mixtures[[1]]$means), byrow = T))
      importances <- sapply(1:length(mixtures), FUN=function(i) mixtures[[i]]$ImportanceN)
      weights.vector <- rep(1, times=nrow(quarantineInstances))
      weights.vector[(length(weights.vector) - length(mixtures) + 1):length(weights.vector)] <- importances
    }
    while(abs(error.old - error) > eps && abs(error) != abs(error.old) && max.ite < 2000){
      logLikelihood.old <- logLikelihood
      error.old <- abs(error)
      mixtures.old <- mixtures

      for(j in 1:length(mixtures)){
        determ <- base::det(mixtures[[j]]$sigma)
        while(determ <= 0.2){
          mixtures[[j]]$sigma <- mixtures[[j]]$sigma + diag(ncol(mixtures[[j]]$sigma)) * 1e-3
          determ <- base::det(mixtures[[j]]$sigma)
        }
        mixtures[[j]]$determ <- determ
        mixtures[[j]]$invCov <- MASS::ginv(mixtures[[j]]$sigma)
      }

      mahalanobis.dist <- sapply(mixtures, function(m){
        apply(quarantineInstances, MARGIN=1, stats::mahalanobis, center=m$means, cov=m$invCov, inverted=T)
      })

      densities <- sapply(1:length(mixtures), function(j){
        ((2*pi)^-(ncol(quarantineInstances)/2)) * (mixtures[[j]]$determ^-0.5) * exp(-0.5*mahalanobis.dist[, j])
      })
      densities[densities== 0] <- densities[densities== 0] + 1e-50

      x <- (densities * tau)
      probs <- t(x/rowSums(x))
      probs[is.nan(probs)] <- 0
      if(is.vector(probs)){
        probs <- matrix(probs, nrow = length(mixtures))
      }

      for(j in 1:length(mixtures)){
        mixtures[[j]]$Nk <- sum(probs[j, ] * weights.vector)
      }

      tau <- sapply(mixtures, function(m) m$Nk) / sum(weights.vector)

      # Estimate mixture component parameters
      for(j in 1:length(mixtures)){
        mixtures[[j]]$means <- matrix(colSums((weights.vector * probs[j, ] * quarantineInstances) / mixtures[[j]]$Nk))
      }
      if(sample.models){
        for (j in 1:length(mixtures)){
          x.mu <- quarantineInstances - matrix(mixtures[[j]]$means, nrow = nrow(quarantineInstances), ncol=length(mixtures[[j]]$means), byrow = T)
          res <- lapply(1:nrow(x.mu), function(ix) probs[j, ix] * (matrix(x.mu[ix, ]) %*% x.mu[ix, ]) )
          mixtures[[j]]$sigma <- Reduce('+', res) / sum(probs[j, ])
        }
      }else{
        ix.qu <- 1:(nrow(quarantineInstances)-length(mixtures)+k)
        ix.orig <- tail(1:nrow(quarantineInstances), length(originalMeans))
        for(j in 1:length(mixtures)){
          x.mu <- quarantineInstances - matrix(mixtures[[j]]$means,nrow=nrow(quarantineInstances), ncol=length(mixtures[[j]]$means), byrow = T)
          x.mu.t <- lapply(1:nrow(x.mu), function(ix) x.mu[ix, ]%*%t(x.mu[ix, ]))
          if(j %in% (length(originalMeans)+k-1):length(mixtures)){
            j.aux <- j - k
            if(j.aux == 0) j.aux <- 1
            x.mu.t[[ix.orig[j.aux]]] <- originalVariances[[j.aux]]
          }

          sigma <- Reduce('+', lapply(1:length(x.mu.t), function(ix) (weights.vector[ix] * probs[j, ix]) * x.mu.t[[ix]])) / mixtures[[j]]$Nk
          mixtures[[j]]$sigma <- sigma
        }
      }

      error <- 0
      for(j in 1:length(mixtures)){
        error <- error + sum(mixtures[[j]]$means - mixtures.old[[j]]$means) + sum(mixtures[[j]]$sigma - mixtures.old[[j]]$sigma)
      }
      max.ite <- max.ite + 1

    }

    logLikelihood <- sum(weights.vector * log(rowSums(sapply(1:length(mixtures), function(j){
      tau[j] * densities[, j]
    }))))

    mixtures$observations <- apply(probs, MARGIN=2, function(p) mixtures[[which.max(p)]]$toWhoClassify)
    for(i in 1:length(mixtures[-which(names(mixtures) == 'observations')])){
      mixtures[[i]]$N <- length(which(mixtures$observations == i)) + mixtures[[i]]$N
    }
    if(strategy %in% c('restart')){
      for(i in 1:length(mixtures[-which(names(mixtures) == 'observations')])){
        mixtures[[i]]$ImportanceN <- 0
      }
    }
    mixtures <- list('model'=mixtures,
                     'logLik'=logLikelihood,
                     'observations' = apply(probs, MARGIN=2, which.max),
                     'probs' = probs,
                     'mahalanobis.dist' = mahalanobis.dist,
                     'oldMixtureIndex' = oldMixtureIndex,
                     'SumOfN' = sum(weights.vector))
    all.mixtures <- append(all.mixtures, list(mixtures))
  }
  nparams <- length(mixtures$model[[1]]$means) + ((nrow(models[[1]]$sigma)*(nrow(models[[1]]$sigma)+1))/2)
  bic <- sapply(all.mixtures, FUN=function(m){
    ((length(m$model)-1) * nparams * log(m$SumOfN)) - (2*m$logLik)
  })
  all.dets <- sapply(all.mixtures, function(mx){
    sapply(1:(length(mx$model)-1), function(ix.mdl) det(mx$model[[ix.mdl]]$sigma))
  })
  print(all.dets)
  for(ixmdet in 1:length(all.dets)){
    if(any(all.dets[[ixmdet]] < 0.2))
      bic[ixmdet] <- Inf
  }
  print("Este es el BIC")
  print(bic)
  mdls <- all.mixtures[[which.min(bic)]]$model
  correct.mixture <- all.mixtures[[which.min(bic)]]
  probs <- correct.mixture$probs
  oldMixtureIndex <- all.mixtures[[which.min(bic)]]$oldMixtureIndex
  observations <- all.mixtures[[which.min(bic)]]$model$observations[1:length(quarantine)]
  mdls$oldMixtureIndex <- NULL
  mdls$observations <- NULL
  for(i in 1:length(mdls)){
    mdls[[i]]$means <- matrix(mdls[[i]]$means, nrow=1)
  }
  if(length(mdls) > length(models)){
    k <- length(mdls) - length(models)
    for(ik in 1:k) mdls[[ik]]$ImportanceN <- table(apply(probs, MARGIN=2, which.max))[k]
  }
  return(list('models'=mdls, 'observations'=observations, 'probs' = probs, 'bic'=bic, 'loglik'=correct.mixture$logLik))
}

## Auxiliar functions
strategyForN <- function(models, NquarantineInstances, strategy, nIterations, maxBuffer, quarantineInstances){
  # This function is not used. However, this has been used until we figured out that the best option was to use a metaregressor
  if(strategy=='fixed'){
    for(j in 1:length(models)){
      models[[j]]$ImportanceN  <- 0.9 * models[[j]]$ImportanceN
    }
  }else if(strategy == 'restart'){
    # just restarts in the discoverNewClass function
  }else if(strategy == 'dynamic'){
    for(j in 1:length(models)){
      models[[j]]$ImportanceN  <- (NquarantineInstances / (NquarantineInstances + models[[j]]$ImportanceN)) * models[[j]]$ImportanceN
    }
  }else if(strategy == 'exponential'){
    for(j in 1:length(models)){
      alfa <- 0.5
      lambda <- 0.1
      if (maxBuffer == nIterations)
        v <- 1
      else{
        v <- exp(-lambda*(nIterations - maxBuffer))
      }
      print(paste0("La velocidad es: ", v))
      h <- hopkins.index(quarantineInstances = quarantineInstances)
      print(paste0("La heterogeneidad es: ", h))
      a <- (alfa*h) + ((1-alfa)*v)
      print(paste0("El a => ", a))
      a <- models[[j]]$ImportanceN * a
      print(paste0('Iteraciones: ', nIterations))
      print(paste0('N casos classif: ', models[[j]]$N))
      print(paste0('ImportanciaN nueva: ', a))
      models[[j]]$ImportanceN <- a
    }
  }else if(strategy == 'basic'){
    if((nIterations-maxBuffer) > maxBuffer/4){
      for(j in 1:length(models)){
        models[[j]]$ImportanceN <- 5*models[[j]]$ImportanceN
      }
    }else{
      for(j in 1:length(models)){
        models[[j]]$ImportanceN <- 0.5*models[[j]]$ImportanceN
      }
    }
  }else{
    stop('Select a valid strategy for Importance of N')
  }
  return(models)
}

hopkins.index <- function(quarantineInstances, percentaje = 0.20){
  # This function is not used
  d <- quarantineInstances[sample(x = seq_len(nrow(quarantineInstances)), size = nrow(quarantineInstances)*percentaje, replace = F), ]
  d.unif <- sapply(c(1:ncol(quarantineInstances)), function(ix){
    runif(n = nrow(d), min = min(unlist(quarantineInstances[, ix])), max=max(unlist(quarantineInstances[, ix])))
  })
  dist.matrix <- apply(d, 1, function(x){
    apply(d.unif, 1, function(y){
      dist(rbind(x,y))
    })
  })

  u <- apply(dist.matrix, MARGIN=1, min)
  w <- apply(dist.matrix, MARGIN=2, min)

  return((sum(u^ncol(d))) / (sum(u^ncol(d)) + sum(w^ncol(d))))
}

# Gaussian KLDivergence
# Method: Calculate divergence from A to B and from B to A and calculate the mean
gaussianKLDivergence <- function(model1, model2){
  # Useful for debugging
  AtoB <- 0.5 * (log(base::det(model2$sigma)) - log(base::det(model1$sigma))) - length(model2$means) + sum(diag( MASS::ginv(model2$sigma) %*% model1$sigma )) + ( (model2$means - model1$means) %*% MASS::ginv(model2$sigma) %*% t(model2$means - model1$means) )
  BtoA <- 0.5 * (log(base::det(model1$sigma)) - log(base::det(model2$sigma))) - length(model2$means) + sum(diag( MASS::ginv(model1$sigma) %*% model2$sigma )) + ( (model1$means - model2$means) %*% MASS::ginv(model1$sigma) %*% t(model1$means - model2$means))
  return(mean(c(AtoB, BtoA), na.rm=F))
}

getMdl <- function(data, nclass=1, min.n = NULL, eps=.25){
  # Not used
  clusters <- lapply(1:nclass, FUN=function(G){
    Mclust(data, G=G, verbose = F)
  })
  AICs <- sapply(clusters, function(c){
    2*c$df - 2*c$loglik
  })
  mdl <- clusters[[which.min(AICs)]]
  for (covM in 1:mdl$G) {
    if (base::det(mdl$parameters$variance$sigma[,,covM]) < 0.5){
      mdl$parameters$variance$sigma[,,covM] <- mdl$parameters$variance$sigma[,,covM] + eps
    }
  }
  return(mdl)
}

# Fits two linear regressions to a curve and returns the x cutpoint of both linear equations
linear.regression.cut.point <- function(x){
  # Not used
  cutpoint <- 2
  data.res <- data.frame('cutpoint'=1, 'MSE'=0, 'x'=0, 'y'=0)[0]
  while(cutpoint <= length(x)-1){
    set.1 <- x[1:cutpoint]
    set.2 <- x[cutpoint:length(x)]
    set.1.lm <- lm(set.1 ~ c(1:length(set.1)))
    set.2.lm <- lm(set.2 ~ c(length(set.1):(length(set.1) + length(set.2)-1)))
    b <- matrix(c(-set.1.lm$coefficients[1], -set.2.lm$coefficients[1]), ncol=1)
    A <- matrix(c(set.1.lm$coefficients[2], set.2.lm$coefficients[2], -1, -1), ncol=length(set.1.lm$coefficients), nrow=2)
    rs <- solve(A,b)
    data.res <- rbind(data.res, data.frame('cutpoint'=cutpoint, 'MSE'=mean(set.1.lm$residuals) + mean(set.2.lm$residuals), 'x'=rs[1], 'y'=rs[2]))
    cutpoint <- cutpoint + 1
  }
  return(round(data.res[which.min(data.res$MSE), ]$x))
}

mahalanobis.mixtures <- function(x, models){
  # Mahalanobis distance among mixtures
  x <- as.vector(x)
  res <- lapply(models, FUN=function(m){ # For each model
    integral <- sapply(c(1:length(m$means)), function(k){ # For each component
      v <- matrix(m$means[[k]] - x, ncol=1)
      u <- matrix(m$means[[k]] - x, ncol=1)
      b2 <- (t(v) %*% m$invCov[[k]] %*% v)^-1
      a <- b2*(t(v) %*% m$invCov[[k]] %*% u)
      Z <- (t(u) %*% m$invCov[[k]] %*% u) - b2 * (t(v) %*% m$invCov[[k]] %*% u)^2

      A <- sqrt(pi*b2 / 2) * exp(-Z/2) * ((erf((1-a)/(sqrt(2*b2)))) - (erf((-a)/(sqrt(2*b2)))))
    })
    if(all(integral != 0)){
      bot <- sum(m$weightK * integral)
      top <- Reduce('+', lapply(m$invCov, function(kcov) kcov * bot))
      G <- top/bot

      dst <- sapply(1:length(m$means), function(k){
        matrix(unlist(x - m$means[[k]]), nrow=1) %*% G %*% matrix(unlist(x-m$means[[k]]), ncol=1)
      })
    }else return(integral)

  })
  return(res)
}

mahalanobis.mixtures.sinUnir <- function(x, models){
  # Not used
  x <- as.vector(x)
  res <- lapply(models, FUN=function(m){
    mapply(center=m$means, cov= m$invCov, FUN=stats::mahalanobis, MoreArgs = list(x = x, inverted=T))
  })
}

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


dist2line <- function(a, b, c){
  # Not used
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}

entropy.mixtures <- function(models){
  # Calculates the entropy among mixtures
  ninstances <- 5000
  instances <- do.call(rbind, lapply(models, function(m){
    mvtnorm::rmvnorm(ninstances, mean = m$means, sigma=m$sigma)
  }))
  # quarantineInstances <- matrix(unlist(quarantineInstances), ncol=2)
  densities.all <- sapply(models, function(m){
    # mahalanobis.dist <- apply(quarantineInstances, MARGIN=1, stats::mahalanobis, center=matrix(m$means), cov=m$invCov, inverted=T)
    # densities <- ((2*pi)^-(ncol(quarantineInstances)/2)) * (base::det(m$sigma)^-0.5) * exp(-0.5*mahalanobis.dist)
    mahalanobis.dist <- apply(instances, MARGIN=1, stats::mahalanobis, center=matrix(m$means), cov=m$invCov, inverted=T)
    densities <- ((2*pi)^-(ncol(instances)/2)) * (base::det(m$sigma)^-0.5) * exp(-0.5*mahalanobis.dist)
  })
  probs <- t(apply(densities.all, MARGIN=1, function(d) d/sum(d)))
  probs[probs == 0] <- 1e-10 # If 0, error, then numeric 0
  entropy <- -sum(probs * log(probs))
  return(entropy)
}


#' Extracted from the paper: Combining mixture comopnents for clustering Baundry, J. et. al. (2010). American statistical association
combiningMixtures <- function(models, probs, oldMixtureIndex){
  # Not used in this paper but useful for future work
  lmodels <- length(models)
  lmodels.merge <- length(unique(sapply(models, FUN=function(m) m$toWhoClassify)))
  expanded <- expand.grid(setdiff(c(1:lmodels), oldMixtureIndex), c(1:lmodels))
  expanded <- as.matrix(expanded[expanded[,2] > expanded[,1], ]) # Delete duplicates 1,1 and 1,2 with 2,1
  if(nrow(expanded) > 0) {
    res <- sapply(split(expanded, seq(nrow(expanded))), FUN=function(pair){
      pair <- as.numeric(pair) # First is k and second is kp
      merged.probs <- probs[pair[1], ] + probs[pair[2], ]
      restmixtures <- c(1:lmodels)[-which(c(1:lmodels) %in% c(pair[1], pair[2]))]
      r <- -sum(probs[pair[1], ] * log(probs[pair[1]]) + probs[pair[2], ] * log(probs[pair[2], ]), na.rm=T) + sum(merged.probs * log(merged.probs))
      print(paste0(paste(pair, collapse = '-'), ' --> ', r))
      return(r)
    })
    expanded <- expanded[order(res, decreasing = T), ]
    res <- sort(res, decreasing = T)
    values <- apply(rbind(2:(length(res)-1), res[c(-1, -length(res))]), MARGIN=2, dist2line, b=c(1, res[1]), c=c(length(res), res[length(res)]))
    total.clusters <-  which.max(values) + 1
    while(lmodels.merge > total.clusters){
      best.merge <- expanded[which.max(res), ]
      if(any(best.merge %in% oldMixtureIndex)){
        selected <- best.merge[best.merge %in% oldMixtureIndex]
        toWho <- models[[selected]]$toWhoClassify
        models[[best.merge[1]]]$toWhoClassify <- toWho
        models[[best.merge[2]]]$toWhoClassify <- toWho
      }else{
        models[[best.merge[1]]]$toWhoClassify <- models[[best.merge[2]]]$toWhoClassify
      }
      probs[expanded[which.max(res), 1], ] <- probs[expanded[which.max(res), 1], ] + probs[expanded[which.max(res), 2], ]
      probs <- probs[-expanded[which.max(res), 2], ]
      lmodels.merge <- lmodels.merge - 1
      lmodels <- lmodels - 1
    }
    # Adjust the weight for all the mixture komponents
    classes.list <- unique(sapply(models, function(m) m$toWhoClassify))
    for(clix in classes.list){
      mx.ix <- which(sapply(models, function(m) m$toWhoClassify) == clix)
      for(ix in mx.ix){
        models[[ix]]$weightK <- 1/length(mx.ix)
      }
    }
  }
  return(models)
}

calculateDeterminat <- function(covM){
  # Not used
  nRows <- nrow(covM)
  nColumns <- ncol(covM)
  while(i < nRows){
    while(j < nColumns){
      print(paste0(i, ' -- ', j))
      if(covM[i,j] == sqrt(covM[i,i]*covM[j,j])){
        covM <- covM[-i, ]
        covM <- covM[, -i]
        print('ADIOS')
      }
      j <- j + 1
      nColumns <- ncol(covM)
    }
    i <- i + 1
    nRows <- nrow(covM)
  }
  base::det(covM)
}

getweights <- function(number.params, sweep.vector){
  # A former way to get the weights without the metaregressor
  combinations <- expand.grid(rep(list(sweep.vector), number.params))
  valid.w <- round(apply(combinations, MARGIN=2, exp))
  valid.l <- combinations
  return(list('valid.w' = valid.w, 'valid.l' = valid.l))
}

getWeights.density <- function(models, quarantineInstances){
  # A former way to get the weights without the metaregressor
  x <- mvtnorm::dmvnorm(mvtnorm::rmvnorm(n = 1000, mean = models[[1]]$means, sigma = models[[1]]$sigma), mean = models[[1]]$means, sigma = models[[1]]$sigma)
  ref <- sort(x, decreasing = T)[length(x)*0.99]
  quarantineInstances <- matrix( unlist(quarantineInstances), ncol=2)
  qdens <- sapply(models, function(m) {
    apply(quarantineInstances, MARGIN=1, function(x){
      mahalanobis.dist <- mahalanobis(x, m$means, cov=m$invCov, inverted = T)
      ((2*pi)^-(ncol(quarantineInstances)/2)) * (base::det(m$sigma)^-0.5) * exp(-0.5*mahalanobis.dist)
    })
  })
  sum(qdens[, 1] <= ref)
}

getWeightsPredict <- function(regression.model, models, preds, quarantineInstances){
  # Uses the metaregression model to predict the best weights
  cl.model <- classifier.model[[length(models)]]
  distances.to.origin <- sapply(models, function(m){
    origin <- matrix(0, ncol=ncol(m$means))
    dist(rbind(origin, m$means))
  })
  feature.vector <- compute.features.regression(models, quarantineInstances, preds)
  # First classify between ND or CD
  # 0: ND
  # 1: CD
  # print(" Estas son las features: ")
  # print(feature.vector)
  res.classifier <- predict(cl.model, feature.vector)
  print(paste0("====>  EL CLASIFICADOR DICE: ", res.classifier))
  k.nn <- 3
  if(res.classifier == 0){
    feature.vector <- sapply(1:length(feature.vector), FUN=function(ix){
      (feature.vector[ix] - mean(dts.nd[[length(models)]][, ix]))/var(dts.nd[[length(models)]][, ix])
    })
    # Using a KNN
    distances <- apply(dts.nd[[length(models)]][, 1:length(feature.vector)], MARGIN=1, function(x){ dist(rbind(feature.vector, x)) })
    closes.distances <- distances[order(distances, decreasing = T)[1:k.nn]]
    closest.neighbors <- dts.nd[[length(models)]][order(distances, decreasing = T)[1:k.nn], (ncol(dts.nd[[length(models)]])-length(models)+1):ncol(dts.nd[[length(models)]])]
    p <- colMeans(closest.neighbors)
  }else{
    feature.vector <- sapply(1:length(feature.vector), FUN=function(ix){
      (feature.vector[ix] - mean(dts.nd[[length(models)]][, ix]))/var(dts.nd[[length(models)]][, ix])
    })
    distances <- apply(dts.cd[[length(models)]][, 1:length(feature.vector)], MARGIN=1, function(x){ dist(rbind(feature.vector, x)) })
    closes.distances <- distances[order(distances, decreasing = T)[1:k.nn]]
    closest.neighbors <- dts.cd[[length(models)]][order(distances, decreasing = T)[1:k.nn], (ncol(dts.cd[[length(models)]])-length(models)+1):ncol(dts.cd[[length(models)]])]
    p <- colMeans(closest.neighbors)
  }
  print(feature.vector)
  return(list('weights'=p, 'cd'=res.classifier))
}

compute.features.regression <- function(models, quarantineInstances, preds){
  feature.vector <- c() # Just in case, we initialize the vector
  count.classes <- rep(0, length(models))
  table.val <- table(preds[preds != -1])
  counter <- 1
  for (ix.nclasses in 1:length(names(table.val))){
    count.classes[ix.nclasses] <- as.vector(table.val)[counter]
    counter <- counter + 1
  }
  count.classes <- count.classes / sum(count.classes)
  feature.vector <- c(count.classes, entropy.mixtures(models))
  distances.to.origin <- sapply(models, function(m){
    origin <- matrix(0, ncol=ncol(m$means))
    dist(rbind(origin, m$means))
  })
  distances.to.origin <- c(distances.to.origin, rep(Inf, length(count.classes)-length(distances.to.origin)))
  mahalanobis.dist <- sapply(models, function(m){
    apply(matrix(unlist(quarantineInstances), ncol=ncol(quarantineInstances)), MARGIN=1, stats::mahalanobis, center=m$means, cov=m$invCov, inverted=T)
  })
  distances.mahalanobis <- sapply(1:ncol(mahalanobis.dist), function(ix.p){
    return(list('meanDens' = mean(mahalanobis.dist[, ix.p]), 'varDens'=var(mahalanobis.dist[, ix.p])))
  })
  mean.fill <- mean(unlist(distances.mahalanobis[1, ]))
  var.fill <- mean(unlist(distances.mahalanobis[2, ]))
  while(ncol(distances.mahalanobis) < length(count.classes)){
    distances.mahalanobis <- cbind(distances.mahalanobis, matrix(c(mean.fill, var.fill), ncol=1))
  }

  distances.mahalanobis <- distances.mahalanobis[, order(distances.to.origin)]
  distances.mahalanobis <- c(unlist(distances.mahalanobis))
  feature.vector <- c(feature.vector, distances.mahalanobis)

  if (length(models) > 1){
    all.comb <- as.matrix(expand.grid(1:length(models), 1:length(models)))
    divergences <- c()
    distances.diverg <- c()
    all.comb <- matrix(all.comb[apply(all.comb, MARGIN=1, function(x) x[1] > x[2]), ], ncol=2)
    for(ix.c in 1:nrow(all.comb)){
      ix.comb <- all.comb[ix.c, ]
      divergences <- c(divergences, gaussianKLDivergence(models[[ix.comb[1]]], models[[ix.comb[2]]]))
      distances.diverg <- c(distances.diverg, distances.to.origin[ix.comb[1]] + distances.to.origin[ix.comb[2]])
    }
    divergences <- divergences[order(distances.diverg)]
    feature.vector <- c(feature.vector, divergences)
  }
  # Only the buffered -1 (it needs to go first)
  ref.change.preds <- c(-1, order(as.numeric(names(table(preds[preds != -1])), decreasing=T)))
  possible.preds <- as.numeric(names(table(preds)))
  for(pr.ix in 1:length(ref.change.preds)){
    preds[preds == possible.preds[pr.ix]] <- ref.change.preds[pr.ix]
  }
  feature.vector <- c(feature.vector, unlist(calculateMeansVarsFromStream(preds, -1)))
  values <- list()
  possibleClasses <- 1:length(models)
  for (cls in possibleClasses){
    values[[cls]] <- unlist(calculateMeansVarsFromStream(preds, possibleClasses[cls]))
  }
  values <- unlist(values[order(distances.to.origin)])
  feature.vector <- c(feature.vector, values)
  feature.vector <- data.frame(t(unlist(feature.vector)))
  feature.vector.reg <- data.frame(feature.vector)
  feature.vector.reg <- data.matrix(feature.vector.reg)
  return(feature.vector.reg)
}

calculateMeansVarsFromStream <- function(stream, class.number){
  # This calculates the mean and variance of the time that takes to predict an instance of the same class.
  mean.arrive <- mean(stream == class.number)
  var.arrive <- var(stream == class.number)
  return(list(mean.arrive, var.arrive))
}

montecarlo.overlap <- function(models){
  # Overlapping among two Gaussian distributions. Done by simulation.
  if(length(models) > 1){
    batch.size <- 1000
    all.points.sampled <- 0
    points.in <- 0
    while(points.in < 500 && all.points.sampled < 1000000){
      prob.x <- runif(batch.size, min = 0, max=1)
      sum(prob.x > 0.5)
      x <- mvtnorm::rmvnorm(sum(prob.x > 0.5), mean = models[[1]]$means, sigma=models[[1]]$sigma)
      x <- rbind(x, mvtnorm::rmvnorm(batch.size-nrow(x), mean = models[[2]]$means, sigma=models[[1]]$sigma))
      inout <- sapply(models, function(m){
        stats::mahalanobis(x, center=m$means, cov=m$invCov, inverted=T)
      }) < models[[1]]$boundary
      points.in <- points.in + sum(rowSums(inout) == 2)
      all.points.sampled <- all.points.sampled + batch.size
    }
    return(points.in / all.points.sampled)
  }else
    return(0)
}
