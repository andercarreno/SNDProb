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
        # mdl <- getMdl(dTrain[[i]])
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
    }else if (method == "mixtureGaussian"){
      data.without.class <- data[, -which(colnames(data)=="class")]
      dTrain <- split(data.without.class, data$class)
      models <- lapply(1:length(dTrain), function(i){
        m1 <- Mclust(dTrain[[i]], modelNames = c('VVV', 'VII'))
        sigmaList <- lapply(1:dim(m1$parameters$variance$sigma)[3], function(k){
          m1$parameters$variance$sigma[,,k]
        })
        mdl<-list('means'=split(m1$parameters$mean, rep(1:ncol(m1$parameters$mean), each=nrow(m1$parameters$mean))),
                  'sigma'=sigmaList, 
                  'N'=nrow(dTrain[[i]]),
                  'ImportanceN'=nrow(dTrain[[i]]),
                  'boundary'=qchisq(percentile, df=ncol(dTrain[[i]])), 
                  'toWhoClassify'=i,
                  'invCov' = lapply(sigmaList, MASS::ginv),
                  'weightK' = rep(1/ncol(m1$parameters$mean), ncol(m1$parameters$mean)))
        mdl$sigma <- lapply(mdl$sigma, function(sg){
          while(base::det(sg) < eps){
            sg <- sg + (diag(nrow(sg)) * 0.00001)
          }
          return(sg)
        })
        return(mdl)
      })
      names(models) <- c(1:length(dTrain))
      return(models) 
    }else if(method == "copula"){
      if(missing(vector)){
        data.without.class <- data[, -which(colnames(data)=="class")]
        dTrain <- split(data.without.class, data$class)
        models <- lapply(1:length(dTrain), function(i){
          list('copula' = estimateMvdv(data = dTrain[[i]], vector = vector), 
               'toWhoClassify' = i, 
               'N' = nrow(dTrain[[i]]),
               'boundary' = NULL)
        })
        for(m in 1:length(models)){
          simulateDensityBoundary(models[[m]]$copula)
        }
        return(models)
      }else{
        stop("Provide a parameter vector to learn the copula. Missing vector variable.
             
             Example:
             vector <- c(
             3, # number of variables
             MARGIN_NORMAL, # 1st margin (normal)
             NA, # 1st margin (parameters to be estimated by the algorithm)
             MARGIN_NORMAL, 0, 10, # 2nd margin (normal, mean and std)
             MARGIN_NORMAL, 0, 100, # 3rd margin (normal, mean and std)
             VINE_RVINE,
             2, # number of vine trees
             COPULA_NORMAL, # 1st copula
             NA, # 1st copula (parameters to be estimated by the algorithm)
             NA, # 2nd copula (to be selected by the algorithm)
             COPULA_CLAYTON, 10 # 3rd copula (Clayton, theta)
             )")
      }
    }else{
      stop("Provide a valid method: gaussian, mixtureGaussians, or copula")
    }
  }
  else
    stop("There is no column named class")
}

predict.parametricNovelty <- function(data, models, quarantine, quarantineInstances, old.preds, maxBuffer, next.class.label, method="gaussian", nClustersToSeek, regression.model, epoch.start, export.path='./'){
  # Need it to control the index of quarantine instances
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
    updated.res <- update.parametricNovelty(data = data, quarantine = quarantine, quarantineInstances = quarantineInstances, 
                                            preds = preds, next.class.label = next.class.label, maxBuffer = maxBuffer, 
                                            models = models, nClustersToSeek = nClustersToSeek, regression.model = regression.model, 
                                            export.path = export.path, epoch.start = epoch.start)
    models <- updated.res$models
    quarantine <- updated.res$quarantine
    quarantineInstances <- updated.res$quarantineInstances
    preds <- updated.res$preds
  }else if(method == 'mixtureGaussian'){
    data <- unlist(data)
    # dst <- mahalanobis.mixtures(data, models)
    dst <- mahalanobis.mixtures.sinUnir(data, models)
    dst <- sapply(dst, min) # This select the distance to the closer component
    res <- sapply(1:length(dst), function(d){
      sapply(dst[[d]], function(dk){
        if (dk <= models[[d]]$boundary) return(dk)
        else return('Candidate')
      })
    })
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
    updated.res <- update.parametricNovelty(data = data, quarantine = quarantine, quarantineInstances = quarantineInstances, preds = preds, maxBuffer = maxBuffer, models = models, nClustersToSeek = nClustersToSeek, method = method)
    models <- updated.res$models
    quarantine <- updated.res$quarantine
    quarantineInstances <- updated.res$quarantineInstances
    preds <- updated.res$preds
  }else
    stop("Select a valid distribution family: gaussian (by default) or mixtureGaussian")
  return(list('models'=models, 'preds'=preds, 'quarantine'=quarantine, 'quarantineInstances'=quarantineInstances))
}

update.parametricNovelty <- function(data, quarantine, quarantineInstances, preds, next.class.label, maxBuffer, models, nClustersToSeek, regression.model, epoch.start, method='gaussian', export.path = './'){
  if(method=='gaussian'){
    if(length(quarantine) >= maxBuffer){
      print("Discovering new class... This can take several minutes")
      # print(paste0("This is the iteration: ", EX_SEQ))
      stop("PARA")
      ### ESTO CUANDO TIENES EL REGRESSION MODEL
      ## Do we have to remove a class?
      my.preds <- preds[epoch.start:length(preds)]
      my.preds <- my.preds[my.preds != -1]
      count.preds <- rep(0, max(sapply(models, function(m) m$toWhoClassify)))
      count.preds[as.numeric(names(table(my.preds)/length(my.preds)))] <- table(my.preds)/length(my.preds)
      to.delete <- which(count.preds <= 0.05)
      print("Estas son las cantidades de instancias de cada clase")
      print(count.preds)
      ix.to.delete <- which(sapply(models, function(m) m$toWhoClassify) %in% to.delete)
      to.delete.class.labels <- sapply(models[ix.to.delete], function(m) m$toWhoClassify)
      if(length(ix.to.delete) > 0 ){
        print(paste0("WE DELETE CLASS ", ix.to.delete))
        models <- models[-ix.to.delete]
        names(models) <- c(1:length(models))
      }
      
      ######## Esto es lo nuevo para hacer el barrido y obtener el dataset
      # res.barrido <- barridoPesos(models, preds, quarantine, quarantineInstances, nClustersToSeek, maxBuffer )
      # models <- res.barrido$models
      # quarantine <- res.barrido$quarantine
      # quarantineInstances <- res.barrido$quarantineInstances
      # preds <- res.barrido$preds
      
      #############################################################################
      ############ RESTO MODIFICACIONES PARA IMAGENES DEL PAPER ###################
      # weights.models <- getWeightsPredict(regression.model, models, preds[epoch.start:length(preds)], quarantineInstances)
      ########### TRUCAMOS
      # pdf(paste0('~/Downloads/', '/plot_', i, '.pdf'))
      # plot(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0), xlim=c(-8, 14), xlab="", ylab="")
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in models){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      ####################
      if(length(models) > 1){
        weights.models <- getWeightsPredict(regression.model, models, preds[epoch.start:length(preds)], quarantineInstances, to.delete.class.labels)
      }else{
        weights.models <- list('weights'=c(500), 'cd'=0)
      }
      # pdf(paste0(export.path, 'quarantine', paste(weights.models, collapse = '__'), '.pdf'))
      # par(mai=c(1,1,0.3,0.3))
      # plot(quarantineInstances, cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8)
      # # , xlim = c(-20, 15), ylim=c(-10,20)
      # dev.off()
      
      # Para sacar en PDF
      # pdf(paste0(exportPath, '/plot_', i, '.pdf'))
      # par(mai=c(1,1,0.3,0.3))
      # plot(x=dTest$X1[1:i], y=dTest$X2[1:i], col=alpha(as.factor(preds[1:i]+ 1), 0.4), cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8, xlim = c(-20,15), ylim=c(-10, 20))
      # # xlim = c(-20, 15), ylim=c(-10,20)
      # points(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0))
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in models.to.plot){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      
      # png(paste0(exportPath, '/plot_', i, '.png'), width = 5, height = 5, units = "in", res=400)
      # par(mai=c(1,1,0.3,0.3))
      # plot(x=dTest$X1[1:i], y=dTest$X2[1:i], col=alpha(as.factor(preds[1:i]+ 1), 0.4), cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8, xlim = c(-20,15), ylim=c(-10, 20))
      # # xlim = c(-20, 15), ylim=c(-10,20)
      # points(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0))
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in models.to.plot){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      
      # setEPS()
      # # postscript(paste0(exportPath, '/plot_', i, '.eps'),  width = 5, height = 5, units = "in")
      # cairo_ps(file = paste0(exportPath, '/plot_', i, '.eps'), width = 5, height = 5, onefile = FALSE, fallback_resolution = 600)
      # par(mai=c(1,1,0.3,0.3))
      # plot(x=dTest$X1[1:i], y=dTest$X2[1:i], col=alpha(as.factor(preds[1:i]+ 1), 0.4), cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8, xlim = c(-20,15), ylim=c(-10, 20))
      # # xlim = c(-20, 15), ylim=c(-10,20)
      # points(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0))
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in models.to.plot){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      
      print("==> The predicted weights set by the metaregressor are: ")
      print(weights.models$weights)
      # Calculate the distances between the center of the mixtures and origin
      # distances <- sapply(models, function(m){
      #   sqrt(sum(m$means^2))
      # })
      # models <- models[order(distances)]
      for(ix.model in 1:length(models)){
        models[[ix.model]]$ImportanceN <- unlist(weights.models$weights)[ix.model]
      }
      if(weights.models$cd == 1){
        nClustersToSeek <- c(0)
      }else{
        nClustersToSeek <- c(0:nClustersToSeek)
      }
      
      ########### TRUCAMOS
      # print("HOLA")
      # weights.models <- list('weights'=c(365, 812), 'cd'=0)
      # for(ix.model in 1:length(models)){
      #   models[[ix.model]]$ImportanceN <- unlist(weights.models$weights)[ix.model]
      # }
      # nClustersToSeek <- c(0:nClustersToSeek)
      # cluster <- discoverNewClass(models = models, quarantine = quarantine, quarantineInstances = quarantineInstances, nClustersToSeek = nClustersToSeek, maxBuffer = maxBuffer, export.path = export.path)
      # pdf(paste0('~/Downloads/', '/plotNuevosPesos_', i, '.pdf'))
      # plot(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0), xlim=c(-8, 14), xlab="", ylab="")
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in cluster$models){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      # png(paste0('~/Downloads/', '/plotNuevosPesos_', i, '.png'), width = 5, height = 5, units = "in", res=400)
      # plot(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0), xlim=c(-8, 14), xlab="", ylab="")
      # # legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      # for(m in cluster$models){
      #   car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      # }
      # dev.off()
      ####################
      
      # Discover new classes
      cluster <- discoverNewClass(models = models, quarantine = quarantine, quarantineInstances = quarantineInstances, 
                                  next.class.label = next.class.label, nClustersToSeek = nClustersToSeek, 
                                  maxBuffer = maxBuffer, export.path = export.path)
      if(all(cluster$bic==Inf)){
        # If the weights of the metaregressor are wrong we can get incoherent mixtures with minimal variance
        # we relaunch the discovering process in a standard equal setting for each class
        weights.models$weights <- rep(86, length(models))
        for(ix in 1:length(models)){
          models[[ix]]$ImportanceN <- weights.models$weights[ix]
        }
        cluster <- discoverNewClass(models = models, quarantine = quarantine, quarantineInstances = quarantineInstances, 
                                    next.class.label = next.class.label, nClustersToSeek = nClustersToSeek, 
                                    maxBuffer = maxBuffer, export.path = export.path)
      }
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
          mean <- updateMean(xbar = model$means, x = unlist(dToUpdate), n = model$ImportanceN, f=0)
          s <- updateCovariance(C = model$sigma, x = unlist(dToUpdate), n = model$ImportanceN, xbar = t(model$means), f=0)
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
  }else if (method == 'mixtureGaussian'){
    if(length(quarantine) >= maxBuffer){
      # Discover new classes
      cluster <- discoverNewClass(models = models, quarantineInstances = quarantineInstances, nClustersToSeek = nClustersToSeek, maxBuffer = maxBuffer, method = method)
      preds[quarantine] <- cluster$observations
      models <- cluster$models
      quarantine <- c() # Delete quarantine for future elements
      quarantineInstances <- quarantineInstances[0, ] # Delete quarantine keeping structure for future elements
    }else{
      if(preds[length(preds)] != -1){
        # Update concept drift
        mixture <- models[[preds[length(preds)]]] # only this mixture is going to be updated
        mixture$N <- mixture$N + 1
        dns <- sapply(1:length(mixture$means), function(mx.ix) dmvnorm(data, mixture$means[[mx.ix]], mixture$sigma[[mx.ix]]))
        dns <- dns/sum(dns)
        data <- unlist(data)
        for (mx.ix in 1:length(mixture$means)){
          new.mean <- mixture$means[[mx.ix]] + (dns[mx.ix] * ((data - mixture$means[[mx.ix]]) / (mixture$N + 1)))
          mixture$sigma[[mx.ix]] <- mixture$sigma[[mx.ix]] + (dns[mx.ix] * ((((matrix(data - mixture$means[[mx.ix]]) %*% matrix(data - new.mean, nrow=1)) - mixture$sigma[[mx.ix]]) / (mixture$N + 1))))
          mixture$invCov[[mx.ix]] <- MASS::ginv(mixture$sigma[[mx.ix]])
          mixture$means[[mx.ix]] <- new.mean
        }
        models[[preds[length(preds)]]] <- mixture
      }
    }
    return(list('models'=models, 'quarantine'=quarantine, 'quarantineInstances' = quarantineInstances, 'preds'=preds))
  }else if(method=='copula'){
    if(length(quarantine) >= maxBuffer){
      # Discover new classes. Run copula based clustering
      cluster <- CoClust(quarantineInstances, dimset = )
    }
  }else
    stop("Provide a valid method: 'gaussian' or 'copula'")
} 

barridoPesos <- function(models, preds, quarantine, quarantineInstances, nClustersToSeek, maxBuffer){
  # browser()
  combinations <- expand.grid(1:length(models), 1:length(models))
  combinations <- combinations[combinations[, 2] > combinations[, 1], ]
  overlap <- apply(combinations, MARGIN=1, function(pair){
    montecarlo.overlap(list('1'=models[[pair[1]]], '2'=models[[pair[2]]]))
  })
  # print("El overlap es: ")
  # print(overlap)
  
  # if(any(overlap > 0.30)){
  #   matrix.barrido <- getweights(length(models), seq(5.7, 12, 0.8))
  # }else{
  #   matrix.barrido <- getweights(length(models), seq(4.3, 8, 0.5))
  # }
  
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
  dir.exp <- paste0('/Users/andercarreno/Documents/EHU/EvolvingClasses/EXPERIMENTS/Dataset_Bateria/', time.dir)
  # dir.exp <- paste0('/home/acarreno/outputs/Dataset_Bateria/', num.models.train ,'Dims/', time.dir)
  if(dir.exists(dir.exp)){
    dir.exp <- paste0(dir.exp, sample(x = 1:99999, size = 1), '/')
  }else{
    dir.exp <- paste0(dir.exp, '/')
  }
  print(paste0("Se exporta en: ", dir.exp))
  dir.create(dir.exp)
  # pdf(paste0(dir.exp, 'situacion.pdf'))
  # par(mai=c(1,1,0.3,0.3))
  # plot(quarantineInstances, xlim = c(-20, 50), ylim=c(-30, 20), cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8)
  # for(j in 1:length(models)){
  #   car::ellipse(center = c(models[[j]]$means), shape = models[[j]]$sigma, radius = sqrt(models[[j]]$boundary), add = T, fill=F, fill.alpha = .4, col='black')
  # }
  # dev.off()
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
  
  # Aqui evaluo cual es la mejor de todo el gridSearch
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
    # print(metrics)
    # suppressWarnings(
    #   write.names <- c(paste0('w', 1:length(cl.res$params$w)), paste0('l', 1:length(cl.res$params$l)), # Los parametros con los que sale esto
    #                    sort(levels(interaction(paste0('model', 1:length(models)), paste0('mu', 1:length(models[[1]]$means))))), # Esto para model1.mu1, model1.mu2, ...
    #                    sort(levels(interaction(paste0('model', 1:length(models)), paste0('s', sort(apply(expand.grid(c(1:nrow(models[[1]]$sigma)), c(1:ncol(models[[1]]$sigma)), stringsAsFactors = F), MARGIN=1, paste0, collapse='')))))), # Esto para sigma
    #                    paste0('I', c(IX_SEQ:EX_SEQ)), # Esto para la Sequencia
    #                    names(metrics))
    # )
    # write.x <- matrix(c(as.matrix(cl.res$params$w, nrow=1), as.matrix(cl.res$params$l, nrow=1),
    #                     matrix(do.call(cbind, lapply(models, function(m) m$means)), nrow=1),
    #                     matrix(as.vector(sapply(models, function(m) m$sigma)), nrow=1),
    #                     matrix(preds[c(IX_SEQ:EX_SEQ)], nrow=1), as.matrix(metrics, nrow=1)), nrow=1)
    # colnames(write.x) <- write.names
    # 
    # # Check if file exists
    # f <- paste0(dir.exp, 'BIGTMP_', paste(cl.res$params$l,collapse='-'), '_', format(Sys.time(), "%H%M%S"), '.csv')
    # # print(write.x)
    # if(!exists(f)){
    #   write.table(x = write.x, col.names = T, append = F, sep = ',', dec = '.', file = f, row.names = F)
    # }else{
    #   write.table(x = write.x, col.names = F, append = T, sep = ',', dec = '.', file = f, row.names = F)
    # }
    return(metrics) # Estas ahora hay que elegir cual es la mejor
    # }, mc.cores = NUM_CORES, USE.NAMES = T, SIMPLIFY = F)
  })
  # THIS MERGES ALL FILES IN TMP
  # files <- list.files(path=dir.exp, pattern="^BIGTMP", full.names=TRUE, recursive=FALSE)
  # big.df <- read.csv2(files[1], header=T, sep=',', dec='.')
  # for (fl in files[-1]){
  #   big.df <- rbind(big.df, read.csv2(fl, header=T, sep=",", dec='.'))
  # }
  # fl <- gsub(x = fl, pattern = 'BIGTMP', replacement = 'BIG')
  # if(!file.exists(fl)){
  #   write.table(x = big.df, col.names = T, append = F, sep = ',', dec = '.', file = fl, row.names = F)
  # }else{
  #   write.table(x = big.df, col.names = F, append = T, sep = ',', dec = '.', file = fl, row.names = F)
  # }
  
  # Ahora a elegir cual es la mejor para seguir adelante con estos modelos
  # Basado en: Los que empatan en accuracy, desempato con BIC
  # Eliminar las configuraciones que dan una varianza minima
  for(m.ix in 1:length(res.metrics)){
    dets <- sapply(res.clustering[[m.ix]]$models, function(m) det(m$sigma))
    if(any(dets <= 0.2)){
      res.metrics[[m.ix]]$bic <- Inf
      stop("BIC INF")
    } 
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
  # best.ix <- which(max(all.acc) == all.acc)[ceiling(length(which(max(all.acc) == all.acc))/2)]
  cluster <- res.clustering[[best.ix]]
  # write.x <- matrix(c(as.matrix(res.clustering[[best.ix]]$params$w, nrow=1), as.matrix(res.clustering[[best.ix]]$params$l, nrow=1),
  #                     matrix(do.call(cbind, lapply(cluster$models, function(m) m$means)), nrow=1),
  #                     matrix(as.vector(sapply(cluster$models, function(m) m$sigma)), nrow=1),
  #                     matrix(preds[IX_SEQ:EX_SEQ], nrow=1), as.matrix(res.metrics[[best.ix]], nrow=1)), nrow=1)
  # write.names <- c(paste0('w', 1:length(res.clustering[[best.ix]]$params$w)), paste0('l', 1:length(res.clustering[[best.ix]]$params$l)), # Los parametros con los que sale esto
  #                  sort(levels(interaction(paste0('model', 1:length(cluster$models)), paste0('mu', 1:length(cluster$models[[1]]$means))))), # Esto para model1.mu1, model1.mu2, ...
  #                  sort(levels(interaction(paste0('model', 1:length(cluster$models)), paste0('s', sort(apply(expand.grid(c(1:nrow(cluster$models[[1]]$sigma)), c(1:ncol(cluster$models[[1]]$sigma)), stringsAsFactors = F), MARGIN=1, paste0, collapse='')))))), # Esto para sigma
  #                  paste0('I', c(IX_SEQ:EX_SEQ)), # Esto para la Sequencia
  #                  names(res.metrics[[best.ix]]))
  # colnames(write.x) <- write.names
  # f <- paste0(dir.exp, 'BEST_ACC_', paste(res.clustering$params[[best.ix]]$l,sep='-'), '.csv')
  # if(file.exists(f)){
  #   write.table(x = write.x, col.names = F, append = T, sep = ',', dec = '.', file = f, row.names = F)
  # }else{
  #   write.table(x = write.x, col.names = T, append = F, sep = ',', dec = '.', file = f, row.names = F)
  # }
  # write.feature.vector(big.df[length(which(max(big.df$Correct) == big.df$Correct))/2, ], '/home/acarreno/outputs/Dataset_Bateria/3Dims/features.regression.csv')
  export.features.regression <- paste0(dir.exp, '/features_regression', num.models.train, 'D.csv')
  if(length(cluster$models) > num.models.train){
    type.cd.nd <- 0 # ND
  }else{
    type.cd.nd <- 1 # CD
  }
  # write.feature.vector(big.df[ceiling(length(which(max(big.df$Correct) == big.df$Correct))/2), ], export.features.regression, type=type.cd.nd, models, quarantineInstances)
  if(!all(all.evaluated.bic == Inf)){
    write.feature.vector(cluster$params$w, export.features.regression, type=type.cd.nd, models, quarantineInstances)
    
    # par(mai=c(1,1,0.3,0.3))
    # pdf(paste0(dir.exp, 'situacion_final.pdf'))
    # plot(quarantineInstances, xlim = c(-20, 50), ylim=c(-30, 20), col=cluster$observations, cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8)
    # for(j in 1:length(cluster$models)){
    #   car::ellipse(center = c(cluster$models[[j]]$means), shape = cluster$models[[j]]$sigma, radius = sqrt(cluster$models[[j]]$boundary), add = T, fill=F, fill.alpha = .4, col='black')
    # }
    # dev.off()
  }
  
  preds[quarantine] <- cluster$observations
  
  return(list('models' = cluster$models, 'preds' = preds, quarantine = c(), quarantineInstances = quarantineInstances[0,]))
}


discoverNewClass <- function(models, quarantine, quarantineInstances, next.class.label, nClustersToSeek, maxBuffer, method = 'gaussian', eps=5e-5, export.path='./'){
  # stop("discover")
  strategy = 'exponential'
  if(method == 'mixtureGaussian'){
    # Unpack the mixtures to single model
    models <- unlist(lapply(models, function(m){
      lapply(1:length(m$sigma), function(nComp){
        list('means'=m$means[[nComp]],
             'sigma'=m$sigma[[nComp]],
             'N'=m$N*m$weightK[nComp],
             'ImportanceN' = m$ImportanceN*m$weightK[nComp],
             'boundary' = m$boundary,
             'toWhoClassify' = m$toWhoClassify,
             'invCov' = m$invCov[[nComp]],
             'weightK' = m$weightK[nComp])
      })
    }), recursive=F)
  }
  quarantineInstances <- matrix(unlist(quarantineInstances), ncol=ncol(quarantineInstances))
  # Hay que optimizar esto..
  for (i in 1:length(models)){
    models[[i]]$means <- matrix(models[[i]]$means)
  }
  # models <- strategyForN(models, NquarantineInstances = nrow(quarantineInstances), strategy, maxBuffer, quarantineInstances)
  all.mixtures <- list()
  
  quarantineInstances.orig <- quarantineInstances
  # quarantineInstances <- quarantineInstances.orig
  for (k in nClustersToSeek){
    quarantineInstances <- quarantineInstances.orig
    ########### PARA DIBUJAR LAS ITERACIONES
    # ID_TO_EXPORT_IMAGES <- format(Sys.time(), "%d%m%Y_%H%M%S")
    # dir.create(paste0('~/Desktop/tmp/', ID_TO_EXPORT_IMAGES, '/'))
    # png(paste0('~/Desktop/tmp/', ID_TO_EXPORT_IMAGES, '/', k, '-plot_Inicial', Sys.time(), '.png'))
    # # Sys.sleep(0.3)
    # plot(quarantineInstances)
    # sapply(models, function(m){
    #   ellipse(mu = c(m$means), sigma=m$sigma, alpha = .02)
    # })
    # dev.off()
    ##########
    # print(paste0("PARA K = ",k))
    if(k > 0){
      # Expectation step
      quarantineInstances.class <- sample(1:k, nrow(quarantineInstances), replace = T)
      if(method == 'mixtureGaussians'){
        mixtures <- lapply(split(as.data.frame(quarantineInstances), quarantineInstances.class), function(d){
          ix <- sample(x=1:nrow(d), size=1)
          list('means'=matrix(unlist(d[ix, ])),
               # 'sigma'=var(d),
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
               # 'sigma'=var(d),
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
        mixtures[[toWho]]$toWhoClassify <- next.class.label
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
      # png(paste0('~/Desktop/tmp/', ID_TO_EXPORT_IMAGES, '/', k, '-0.png'))
      # plot(quarantineInstances)
      # sapply(mixtures, function(m){
      #   ellipse(mu = c(m$means), sigma=m$sigma, alpha = .02)
      # })
      # dev.off()
    }else{
      quarantineInstances <- rbind(quarantineInstances, matrix(sapply(mixtures, function(m) m$means), ncol=length(mixtures[[1]]$means), byrow = T))
      importances <- sapply(1:length(mixtures), FUN=function(i) mixtures[[i]]$ImportanceN)
      weights.vector <- rep(1, times=nrow(quarantineInstances))
      weights.vector[(length(weights.vector) - length(mixtures) + 1):length(weights.vector)] <- importances
    }
    # contador <- 0
    while(abs(error.old - error) > eps && abs(error) != abs(error.old) && max.ite < 2000){
      # contador <- contador + 1
      # print(contador)
      logLikelihood.old <- logLikelihood
      error.old <- abs(error)
      mixtures.old <- mixtures
      
      # if(LOZANO == T)
      #   originalVariances <- lapply(mixtures, function(m) m$sigma)
      # inverses <- lapply(mixtures, function(m) ginv(m$sigma))
      # Expectation Step
      # print("Determinante")
      for(j in 1:length(mixtures)){
        determ <- base::det(mixtures[[j]]$sigma)
        # while(determ <= 1e-10){
        while(determ <= 0.01){
          mixtures[[j]]$sigma <- mixtures[[j]]$sigma + diag(ncol(mixtures[[j]]$sigma)) * 1e-3
          determ <- base::det(mixtures[[j]]$sigma)
        }
        mixtures[[j]]$determ <- determ
        # print(determ)
        # print(mixtures[[j]]$sigma)
        # print('')
        mixtures[[j]]$invCov <- MASS::ginv(mixtures[[j]]$sigma)
        # print("si")
        # mixtures[[j]]$invCov <- solve(mixtures[[j]]$sigma)
        # print(mixtures[[j]]$invCov)
      }
      
      # print("mahalanobis")
      mahalanobis.dist <- sapply(mixtures, function(m){
        apply(quarantineInstances, MARGIN=1, stats::mahalanobis, center=m$means, cov=m$invCov, inverted=T) 
      })
      
      # print("Densidades")
      densities <- sapply(1:length(mixtures), function(j){
        ((2*pi)^-(ncol(quarantineInstances)/2)) * (mixtures[[j]]$determ^-0.5) * exp(-0.5*mahalanobis.dist[, j])
      })
      # print(densities[densities==0])
      densities[densities== 0] <- densities[densities== 0] + 1e-50
      
      # print("Probabilidades")
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
      
      # tau <- colSums(sapply(1:length(mixtures), function(j) probs[j, ] * weights.vector)) / sum(weights.vector)
      
      # Estimate mixture component parameters
      # Mean
      # print("====> Ahora actualizo parametros")
      # print("Media")
      for(j in 1:length(mixtures)){
        # print(mixtures[[j]]$means)
        # mixtures[[j]]$means <- matrix(colSums((weights.vector * probs[j, ] * quarantineInstances) / sum(probs[j, ] * weights.vector)))
        mixtures[[j]]$means <- matrix(colSums((weights.vector * probs[j, ] * quarantineInstances) / mixtures[[j]]$Nk))
        # print(mixtures[[j]]$means)
      }
      # print("Varianza")
      
      # for(j in 1:length(mixtures)){
      #   res <- matrix(0, ncol=2, nrow=2)
      #   for (i in 1:nrow(quarantineInstances)){
      #     if(i == 501){
      #       res <- res + (weights.vector[i] * probs[j, i] * originalVariances[[i - nrow(quarantineInstances.orig)]])
      #     }else{
      #       x.mu <- (quarantineInstances[i, ]-mixtures[[j]]$means)
      #       res <- res + (weights.vector[i] * probs[j, i] * (x.mu %*% t(x.mu)))
      #     }
      #   }
      #   res <- res /sum(weights.vector * probs[j, ])
      # }
      #
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
          # Replace with the jth mixture original variance
          # Ander Vero 2/12
          if(j %in% (length(originalMeans)+k-1):length(mixtures)){
            j.aux <- j - k
            if(j.aux == 0) j.aux <- 1
            x.mu.t[[ix.orig[j.aux]]] <- originalVariances[[j.aux]]
          }
          
          # print(mixtures[[j]]$sigma)
          sigma <- Reduce('+', lapply(1:length(x.mu.t), function(ix) (weights.vector[ix] * probs[j, ix]) * x.mu.t[[ix]])) / mixtures[[j]]$Nk
          # sigma <- lapply(1:length(x.mu.t), function(ix){
          #   print(weights.vector[ix])
          #   # print(probs[j, ix])
          #   (weights.vector[ix] * probs[j, ix]) * x.mu.t[[ix]]
          # })
          # sigma <- Reduce('+', sigma)
          # print(sigma)
          mixtures[[j]]$sigma <- sigma
        }  
      }
      
      # print("Error")
      # Calculate the error among the parameters
      error <- 0
      for(j in 1:length(mixtures)){
        error <- error + sum(mixtures[[j]]$means - mixtures.old[[j]]$means) + sum(mixtures[[j]]$sigma - mixtures.old[[j]]$sigma)
      }
      max.ite <- max.ite + 1
      
      ########### PARA DIBUJAR LAS ITERACIONES
      # png(paste0('~/Desktop/tmp/', ID_TO_EXPORT_IMAGES, '/', k, '-plot_', paste0(importances, collapse='_') , Sys.time(), '.png'))
      # Sys.sleep(0.3)
      # plot(quarantineInstances, col=apply(probs, MARGIN=2, which.max))
      # sapply(mixtures, function(m){
      #   ellipse(mu = c(m$means), sigma=m$sigma, alpha = .02)
      # })
      # dev.off()
      #########
      # print("Fin dibujo")
      # if(! abs(error.old - error) > eps){
      #   print("Sale por el siguiente Criterio: ")
      #   print("abs(error.old - error) > eps")
      # }else if(! abs(error) != abs(error.old)){
      #   print("Sale por el siguiente Criterio: ")
      #   print("abs(error) != abs(error.old)")
      # }else if(! max.ite < 2000){
      #   print("Sale por el siguiente Criterio: ")
      #   print("max.ite < 2000")
      # }
      # 
    }
    # densities.sum <- colSums(densities)
    # densities.weighted <- sapply(1:ncol(densities), function(j){
    #   sapply(densities[, j], function(d) d/densities.sum[j])
    # })
    
    logLikelihood <- sum(weights.vector * log(rowSums(sapply(1:length(mixtures), function(j){
      tau[j] * densities[, j]
    }))))
    # print("Este es el log likelihood")
    # print(logLikelihood)
    
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
    # (log(m$SumOfN)*nparams) - (2*m$logLik)
    # (-2*m$logLik) - (log(nparams)*m$SumOfN)
    # (log(m$SumOfN)*(nparams*(length(m$model)-1))) - (2*m$logLik)
    ((length(m$model)-1) * nparams * log(m$SumOfN)) - (2*m$logLik)
  })
  all.dets <- sapply(all.mixtures, function(mx){
    sapply(1:(length(mx$model)-1), function(ix.mdl) det(mx$model[[ix.mdl]]$sigma))
  })
  # print(all.dets)
  for(ixmdet in 1:length(all.dets)){
    print(all.dets)
    if(any(all.dets[[ixmdet]] < 0.5)){
      bic[ixmdet] <- Inf
      # stop("BIC INF")
    }
  }
  print("Este es el BIC")
  print(bic)
  # icl <- sapply(all.mixtures, FUN=function(m){
  #   my.probs.aux <- t(m$probs)
  #   my.probs <- matrix(0, ncol=nrow(m$probs), nrow=ncol(m$probs))
  #   ix <- apply(my.probs.aux, MARGIN=1, which.max)
  #   my.probs[, ix] <- 1
  #   new.loglik <- sum(sapply(1:(length(m$model)-1), function(j){
  #     sum(-0.5 * (log(base::det(m$model[[j]]$sigma)) + mahalanobis.dist[,j] + length(m$model[[j]]$means)*log(2*pi)))
  #   }), na.rm=T)
  #   # -new.loglik - sum(sapply(m$model[-length(m$model)], function(k) ((1/length(m$model))*log(1/length(m$model))) + ((length(k$means)+length(k$sigma))/2)*log(k$N)))
  #   (log(m$SumOfN)*(nparams*(length(all.mixtures[[1]]$model)-1))) - (2*new.loglik)
  # })
  # total.clusters <- which.min(icl)
  # print(paste0('BIC: ', paste(bic, sep=' -- ')))
  mdls <- all.mixtures[[which.min(bic)]]$model
  correct.mixture <- all.mixtures[[which.min(bic)]]
  probs <- correct.mixture$probs
  oldMixtureIndex <- all.mixtures[[which.min(bic)]]$oldMixtureIndex
  observations <- all.mixtures[[which.min(bic)]]$model$observations[1:length(quarantine)]
  mdls$oldMixtureIndex <- NULL
  mdls$observations <- NULL
  # print('FIN EM')
  for(i in 1:length(mdls)){
    mdls[[i]]$means <- matrix(mdls[[i]]$means, nrow=1)
  }
  if(length(mdls) > length(models)){
    k <- length(mdls) - length(models)
    # for(ik in 1:k) mdls[[ik]]$ImportanceN <- table(observations)[length(mdls)-k+ik]
    for(ik in 1:k) mdls[[ik]]$ImportanceN <- table(apply(probs, MARGIN=2, which.max))[k]
  }
  # print("ASI QUEDAN LOS PESOS PARA LA POSTERIDAD:")
  # print(sapply(mdls, function(m) m$ImportanceN))
  if(method == 'mixtureGaussian'){
    models <- combiningMixtures(mdls, correct.mixture$probs, oldMixtureIndex)
    classes.list <- unique(sapply(models, function(m) m$toWhoClassify))
    models.new <- list()
    models2.new <- list()
    for (clix in classes.list){
      models.new[[clix]] <- models[which(sapply(models, function(m) m$toWhoClassify) == clix)]
      means <- list()
      sigma <- list()
      for(ix in 1:length(models.new[[clix]])){
        means[[ix]] <- models.new[[clix]][[ix]]$means
        sigma[[ix]] <- models.new[[clix]][[ix]]$sigma
      }
      models2.new[[clix]] <- list('means'=means, 
                                  'sigma'=sigma,
                                  'invCov' = lapply(sigma, MASS::ginv),
                                  'N'= sum(sapply(models.new[[clix]], function(m) m$N)),
                                  'ImportanceN'=sum(sapply(models.new[[clix]], function(m) m$ImportanceN)),
                                  'boundary'=models.new[[clix]][[1]]$boundary,
                                  'weightK'=rep(1/length(models.new[[clix]]), length(models.new[[clix]])),
                                  'toWhoClassify'=models.new[[clix]][[1]]$toWhoClassify)
    }
    names(models2.new) <- 1:length(classes.list)
    mdls <- models2.new
  }
  return(list('models'=mdls, 'observations'=observations, 'probs' = probs, 'bic'=bic, 'loglik'=correct.mixture$logLik))
}

## Auxiliar functions
strategyForN <- function(models, NquarantineInstances, strategy, nIterations, maxBuffer, quarantineInstances){
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
      # lambda <- nIterations
      lambda <- 0.1
      # a <- models[[j]]$N*exp(sqrt(models[[j]]$N)/lambda)
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
  AtoB <- 0.5 * (log(base::det(model2$sigma)) - log(base::det(model1$sigma))) - length(model2$means) + sum(diag( MASS::ginv(model2$sigma) %*% model1$sigma )) + ( (model2$means - model1$means) %*% MASS::ginv(model2$sigma) %*% t(model2$means - model1$means) )
  BtoA <- 0.5 * (log(base::det(model1$sigma)) - log(base::det(model2$sigma))) - length(model2$means) + sum(diag( MASS::ginv(model1$sigma) %*% model2$sigma )) + ( (model1$means - model2$means) %*% MASS::ginv(model1$sigma) %*% t(model1$means - model2$means))
  return(mean(c(AtoB, BtoA), na.rm=F))
}

getMdl <- function(data, nclass=1, min.n = NULL, eps=.25){
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

simulateDensityBoundary <- function(model, percentile = 0.98, n=100000){
  if(method == 'copula'){
    x <- rmvdv(model, n)
    x<-rmvdv(model, 100000)
    x.density <- dmvdv(model, x)
    x.density <- sort(x.density, decreasing = F)
    boundary.density <- x.density[ceiling(length(x.density) * (1-percentile))]
    return(boundary.density)  
  }else{
    stop("Provide a valid method: copula, lognormal")
  }
}

# Fits two linear regressions to a curve and returns the x cutpoint of both linear equations
linear.regression.cut.point <- function(x){
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
    # plot(x)
    # abline(set.1.lm$coefficients)
    # abline(set.2.lm$coefficients)
    cutpoint <- cutpoint + 1
  }
  return(round(data.res[which.min(data.res$MSE), ]$x))
}

mahalanobis.mixtures <- function(x, models){
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
  x <- as.vector(x)
  res <- lapply(models, FUN=function(m){
    mapply(center=m$means, cov= m$invCov, FUN=stats::mahalanobis, MoreArgs = list(x = x, inverted=T))
  })
}

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


dist2line <- function(a, b, c){
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}

entropy.mixtures <- function(models){
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
  lmodels <- length(models)
  lmodels.merge <- length(unique(sapply(models, FUN=function(m) m$toWhoClassify)))
  expanded <- expand.grid(setdiff(c(1:lmodels), oldMixtureIndex), c(1:lmodels))
  expanded <- as.matrix(expanded[expanded[,2] > expanded[,1], ]) # Delete duplicates 1,1 and 1,2 with 2,1
  if(nrow(expanded) > 0) {
    res <- sapply(split(expanded, seq(nrow(expanded))), FUN=function(pair){
      pair <- as.numeric(pair) # First is k and second is kp
      merged.probs <- probs[pair[1], ] + probs[pair[2], ]
      restmixtures <- c(1:lmodels)[-which(c(1:lmodels) %in% c(pair[1], pair[2]))]
      # r <- -sum(probs[restmixtures, ] * log(probs[restmixtures, ]) + (probs[pair[1], ] + probs[pair[2], ]) * log((probs[pair[1], ] + probs[pair[2], ])), na.rm = T)
      r <- -sum(probs[pair[1], ] * log(probs[pair[1]]) + probs[pair[2], ] * log(probs[pair[2], ]), na.rm=T) + sum(merged.probs * log(merged.probs))
      print(paste0(paste(pair, collapse = '-'), ' --> ', r))
      return(r)
    })
    expanded <- expanded[order(res, decreasing = T), ]
    res <- sort(res, decreasing = T)
    # total.clusters <- linear.regression.cut.point(res)
    # ix.res <- 1
    # entropy.original <- -sum(probs * log(probs), na.rm=T)
    values <- apply(rbind(2:(length(res)-1), res[c(-1, -length(res))]), MARGIN=2, dist2line, b=c(1, res[1]), c=c(length(res), res[length(res)]))
    total.clusters <-  which.max(values) + 1
    # while(res[ix.res] >= mean(res[c(1:ix.res)])){
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
  # look for dependencies
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
  # combinations <- combn(sweep.vector, m = number.params)
  combinations <- expand.grid(rep(list(sweep.vector), number.params))
  valid.w <- round(apply(combinations, MARGIN=2, exp))
  valid.l <- combinations
  return(list('valid.w' = valid.w, 'valid.l' = valid.l))
}

getWeights.density <- function(models, quarantineInstances){
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

getWeightsPredict <- function(regression.model, models, preds, quarantineInstances, ix.to.delete){
  # reg.nd.model <- regression.nd.model[[length(models)]]
  # reg.cd.model <- regression.cd.model[[length(models)]]
  cl.model <- classifier.model[[length(models)]]
  distances.to.origin <- sapply(models, function(m){
    origin <- matrix(0, ncol=ncol(m$means))
    dist(rbind(origin, m$means))
  })
  feature.vector <- compute.features.regression(models, quarantineInstances, preds, ix.to.delete)
  # First classify between ND or CD
  # 0: ND
  # 1: CD
  # print(" Estas son las features: ")
  # print(feature.vector)
  res.classifier <- predict(cl.model, feature.vector)
  meaning.classif <- c("Novelty Detection", "Concept Drift")
  print(paste0("====>  The classifier predicts (RF): ", meaning.classif[as.numeric(res.classifier)]))
  k.nn <- 3
  if(res.classifier == 0){ # ND
    mydata <- dts.nd[[length(models)]]
  }else{ # CD
    mydata <- dts.cd[[length(models)]]
  }
  feature.vector <- sapply(1:length(feature.vector), FUN=function(ix){
    # a <- min(dts.nd[[length(models)]][, ix])
    # b <- max(dts.nd[[length(models)]][, ix])
    # if(a > feature.vector[ix]) feature.vector[ix] <- a
    # if(b < feature.vector[ix]) feature.vector[ix] <- b
    # feature.vector[ix] <- (feature.vector[ix] - a ) / (b-a)
    (feature.vector[ix] - mean(mydata[, ix]))/sd(mydata[, ix])
  })
  # Normalize the data
  i.features <- 1:(ncol(mydata)-length(models)-1)
  i.nd.cd <- i.features[length(i.features)] +1
  i.class <- c(1:ncol(mydata))[-c(i.features, i.nd.cd)]
  mydata <- cbind(apply(mydata[, i.features], MARGIN=2, FUN=normalize), mydata[, i.nd.cd], mydata[, i.class])
  # Aprendo un KNN
  distances <- apply(mydata, MARGIN=1, function(x){
    my.x <- x[i.features]
    # dist(rbind(feature.vector,my.x))
    sqrt(sum((feature.vector - my.x)^2))
  })
  
  order.distances <- order(distances, decreasing = F)
  # which(distances.closest.neighbors %in% distances)
  closest.neighbors <- mydata[order.distances[c(1:k.nn)], i.class]
  # closest.neighbors <- mydata[which(order.distances %in% c(1:k.nn)), i.class]
  print("Distance to the nearest neighbors")
  print(order.distances[1:k.nn])
  p <- colMeans(closest.neighbors)
  # p <- dts.nd[[length(models)]][which.min(distances), (ncol(dts.nd[[length(models)]])-length(models)+1):ncol(dts.nd[[length(models)]])]
  # p <- predict(reg.nd.model, newx=feature.vector.reg, s=min(reg.nd.model$lambda))[,,1]
  # p <- predict(reg.nd.model, feature.vector)
  
  # print(feature.vector)
  # GAITA
  # feature.vector <- sapply(1:length(feature.vector), FUN=function(ix){
  #   a <- min(dts.nd[[length(models)]][, ix])
  #   b <- max(dts.nd[[length(models)]][, ix])
  #   if(a > feature.vector[ix]) feature.vector[ix] <- a
  #   if(b < feature.vector[ix]) feature.vector[ix] <- b
  #   feature.vector[ix] <- (feature.vector[ix] - a ) / (b-a)
  # })
  # print(" Estas son las features: ")
  # print(feature.vector)
  # all.data <- rbind(dts.cd[[length(models)]], dts.nd[[length(models)]])
  # distances <- apply(all.data[, 1:length(feature.vector)], MARGIN=1, function(x){ dist(rbind(feature.vector, x)) })
  # p <- all.data[which.min(distances), (ncol(all.data)-length(models)+1):ncol(all.data)]
  # FIN GAITA
  # The weights are sorted taking into account the distance to origin, hence, we need to 
  # properly assign the predicted weight to the corresponding model
  # p <- p[order(distances.to.origin)]
  # p[p<=0] <- 1
  return(list('weights'=p, 'cd'=res.classifier))
}

compute.features.regression <- function(models, quarantineInstances, preds, deleted.classes){
  feature.vector <- c() # Just in case, borramos todo
  count.classes <- rep(0, length(models))
  table.val <- table(preds[!preds %in% c(-1, deleted.classes)])
  counter <- 1
  for (ix.nclasses in 1:length(names(table.val))){
    count.classes[ix.nclasses] <- as.vector(table.val)[counter]
    counter <- counter + 1
  }
  count.classes <- count.classes / sum(count.classes)
  feature.vector <- c(count.classes, entropy.mixtures(models))
  # feature.vector <- count.classes
  distances.to.origin <- sapply(models, function(m){
    origin <- matrix(0, ncol=ncol(m$means))
    dist(rbind(origin, m$means))
  })
  distances.to.origin <- c(distances.to.origin, rep(Inf, length(count.classes)-length(distances.to.origin)))
  # densities <- sapply(models, function(m){
  #   dens <- mvtnorm::dmvnorm(x = matrix(unlist(quarantineInstances), ncol=ncol(quarantineInstances)), mean = m$means, sigma = m$sigma)
  #   return(dens)
  # }) # <- densidades media y varianza
  # densities <- densities/rowSums(densities)
  # densities.meanvar <- apply(densities, MARGIN=2, function(dens){
  #   return(c('meanDens'=mean(dens, na.rm = T), 'varDens'=var(dens, na.rm = T)))
  # })
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
  
  # densities.meanvar <- densities.meanvar[order(distances.to.origin), ] # <- ordenadas en base al origen
  # feature.vector <- c(feature.vector, densities.meanvar)
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
    # print(paste0("Changing ", possible.preds[pr.ix], " by -> ", ref.change.preds[pr.ix]))
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
  # print(feature.vector.reg)
  return(feature.vector.reg)
}

# getWeightsPredict <- function(regression.model, models, preds, quarantineInstances){
#   reg.nd.model <- regression.nd.model[[length(models)]]
#   reg.cd.model <- regression.cd.model[[length(models)]]
#   cl.model <- classifier.model[[length(models)]]
#   feature.vector <- c() # Just in case, borramos todo
#   count.classes <- rep(0, length(models))
#   table.val <- table(preds[preds != -1])
#   counter <- 1
#   for (ix.nclasses in as.numeric(names(table.val))){
#     count.classes[ix.nclasses] <- as.vector(table.val)[counter]
#     counter <- counter + 1
#   }
#   # feature.vector <- c(count.classes, entropy.mixtures(models, quarantineInstances))
#   feature.vector <- count.classes
#   densities.meanvar <- t(sapply(models, function(m){
#     dens <- mvtnorm::dmvnorm(x = matrix(unlist(quarantineInstances), ncol=ncol(quarantineInstances)), mean = m$means, sigma = m$sigma)
#     return(c('meanDens'=mean(dens, na.rm = T), 'varDens'=var(dens, na.rm = T)))
#   })) # <- densidades media y varianza
#   distances.to.origin <- sapply(models, function(m){
#     origin <- matrix(0, ncol=ncol(m$means))
#     dist(rbind(origin, m$means))
#   })
#   densities.meanvar <- densities.meanvar[order(distances.to.origin), ] # <- ordenadas en base al origen
#   feature.vector <- c(feature.vector, densities.meanvar)
#   # Only the buffered -1 (it needs to go first)
#   feature.vector <- c(feature.vector, unlist(calculateMeansVarsFromStream(preds, -1)))
#   values <- list()
#   for (cls in c(sapply(models, function(m) m$toWhoClassify))){
#     values[[cls]] <- unlist(calculateMeansVarsFromStream(preds, cls))
#   }
#   values <- unlist(values[order(distances.to.origin)])
#   feature.vector <- c(feature.vector, values)
#   feature.vector <- data.frame(t(unlist(feature.vector)))
#   feature.vector.reg <- data.frame(feature.vector)
#   feature.vector.reg <- data.matrix(feature.vector.reg)
#   # First classify between ND or CD
#   # 0: ND
#   # 1: CD
#   print(feature.vector.reg)
#   res.classifier <- predict(cl.model, feature.vector)
#   print(paste0("====>  EL CLASIFICADOR DICE: ", res.classifier))
#   if(res.classifier == 0){
#     p <- predict(reg.nd.model, newx=feature.vector.reg, s=min(reg.nd.model$lambda))[,,1]
#     # p <- predict(reg.nd.model, feature.vector)
#   }else{
#     p <- predict(reg.cd.model, newx=feature.vector.reg, s=min(reg.cd.model$lambda))[,,1]
#     # p <- predict(reg.cd.model, feature.vector)
#   }
#   # The weights are sorted taking into account the distance to origin, hence, we need to 
#   # properly assign the predicted weight to the corresponding model
#   p <- p[order(distances.to.origin)]
#   p[p<=0] <- 1
#   return(list('weights'=p, 'cd'=res.classifier))
# }

calculateMeansVarsFromStream <- function(stream, class.number){
  # st <- which(stream == class.number)
  # res <- st[1] - 1
  # res <- c(res, unlist(sapply(2:length(st), function(ix.st){
  #   st[ix.st] - st[(ix.st-1)] - 1
  # })))
  # return(list(mean(res), var(res)))
  mean.arrive <- mean(stream == class.number)
  var.arrive <- var(stream == class.number)
  return(list(mean.arrive, var.arrive))
}

montecarlo.overlap <- function(models){
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
