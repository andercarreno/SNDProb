require(clue)
evaluateStreamingNoveltyDetection <- function(testReal, predictions, quarantine, knownClasses, nmodels){
  conf.matrix <- conf.matrix.create(testReal = testReal, predictions = predictions, knownClasses = knownClasses)
  sol <- solve_LSAP(conf.matrix, maximum = T)
  for (s in sol){
    predictions[which(predictions == s)] <- sol[s]
  }

  testReal <- as.numeric(testReal)
  predictions <- as.numeric(predictions)
  if(length(quarantine) != 0){
    testReal <- testReal[-quarantine]
    predictions <- predictions[-quarantine]
  }
  if(length(testReal) > 0 && length(predictions) > 0){
    data.frame(# 'MissNew' = missNew(testReal, predictions, knownClasses), # Percentage of new classes misclassified as existing
               # 'FalseNew' = falseNew(testReal, predictions, knownClasses), # Percentage of existing classes missclassified as new
               # 'Error' = length(which(predictions != testReal)) / length(predictions), # Total missclassification error in % (MNew and FNew incl.)
               'Error' = error.global(testReal, predictions),
               # 'CorrectBetweenKnown' = goodPredicted(testReal, predictions, knownClasses), # Total missclassification among known classes in %
               # 'Correct' = length(which(testReal == predictions)) / length(testReal),
               'EN_Accuracy' = en.accuracy(testReal, predictions, c(knownClasses)),
               # 'FMeasure' = fmeasure.newClasses(testReal, predictions, knownClasses = c(knownClasses)),
               'NModels' = nmodels)
  }else{
    data.frame(# 'MissNew' = 0, # Percentage of new classes misclassified as existing
               # 'FalseNew' = 0, # Percentage of existing classes missclassified as new
               # 'Error' = length(which(predictions != testReal)) / length(predictions), # Total missclassification error in % (MNew and FNew incl.)
               'Error' = 0,
               # 'CorrectBetweenKnown' = 0, # Total missclassification among known classes in %
               # 'Correct' = 0,
               'EN_Accuracy' = 0,
               # 'FMeasure' = 0,
               'NModels' = 0)
  }

}

evaluateStreamingNoveltyDetectionIncremental <- function(conf.matrix, knownClasses){
  error <- error.global.incremental(conf.matrix)
  en.acc <- en.accuracy.incremental(conf.matrix, knownClasses)
  data.frame('Error' = error,
             'EN_Accuracy' = en.acc)
}

incrementalConfMatrix <- function(conf.matrix, testReal, predictions, knownClasses, dim){
  conf.matrix <- matrix(0, ncol=dim, nrow=dim)
  conf.matrix[testReal, predictions] <- conf.matrix[testReal, predictions] + 1
  return(conf.matrix)
}

error.global.incremental <- function(conf.matrix){
  n <- sum(conf.matrix)
  return((n-sum(diag(conf.matrix))) / n)
}

en.accuracy.incremental <- function(conf.matrix, knownClasses){
  A0 <- sum(diag(length(knownClasses)) * conf.matrix[knownClasses, knownClasses])
  new.classes <- c(1:nrow(conf.matrix))[c(1:nrow(conf.matrix)) != knownClasses]
  An <- sum(conf.matrix[new.classes, new.classes])
  return((A0 + An)/sum(conf.matrix))
}

error.global <- function(testReal, predictions){
  return(length(which(testReal != predictions)) / length(testReal))
}

goodPredicted <- function(testReal, predictions, knownClasses){
  ioldR <- testReal %in% knownClasses
  res <- length(which(testReal[ioldR] == predictions[ioldR]))
  return(res / length(which(ioldR)))
}

missNew <- function(testReal, predictions, knownClasses){
  inewR <- ! testReal %in% knownClasses
  nnewR <- length(which(inewR))
  if (nnewR == 0){
    return(0)
  }else{
    ioldP <- predictions %in% knownClasses
    noldP <- length(which(ioldP))

    res <- length(which(inewR & ioldP))
    return(res / nnewR)
  }
  return(0)
}

missNew.incremental <- function(conf.matrix, knownClasses){
  new.classes <- c(1:nrow(conf.matrix))[c(1:nrow(conf.matrix)) != knownClasses]
  return(sum(conf.matrix[new.classes, knownClasses]) / sum(conf.matrix[new.classes, ]))
}

en.accuracy <- function(testReal, predictions, initClasses){
  predictions[!(predictions %in% initClasses)] <- 999
  testReal[!(testReal %in% initClasses)] <- 999
  n <- length(predictions)

  A0 <- length(which(testReal[testReal %in% initClasses] == predictions[testReal %in% initClasses]))
  An <- length(which(predictions[!(testReal %in% initClasses)] == testReal[!(testReal %in% initClasses)]))

  return((An + A0) / n)
}

precision.newClasses <- function(testReal, predictions, knownClasses){
  testR <- testReal
  testR[testR %in% knownClasses] <- 0
  testR[testR != 0] <- 1
  pred <- predictions
  pred[pred %in% knownClasses] <- 0
  pred[pred != 0] <- 1
  conf <- confusionMatrix.binary(testR, pred)
  return(conf[1,1]/(conf[1,1]+conf[2,2]))
}

recall.newClasses <- function(testReal, predictions, knownClasses){
  testR <- testReal
  testR[testR %in% knownClasses] <- 0
  testR[testR != 0] <- 1
  pred <- predictions
  pred[pred %in% knownClasses] <- 0
  pred[pred != 0] <- 1
  conf <- confusionMatrix.binary(testR, pred)
  return(conf[1,1] / sum(conf[1, ]))
}

fmeasure.newClasses <- function(testReal, predictions, knownClasses){
  precision <- precision.newClasses(testReal, predictions, knownClasses)
  recall <- recall.newClasses(testReal, predictions, knownClasses)
  (2*precision*recall) / (precision + recall)
}

fmeasure.newClasses.incremental <- function(conf.matrix, knownClasses){
  new.classes <- c(1:nrow(conf.matrix))[c(1:nrow(conf.matrix)) != knownClasses]
  precision <- sum(conf.matrix[new.classes, new.classes]) / sum(conf.matrix[, new.classes])
  recall <- sum(conf.matrix[new.classes, knownClasses]) / sum(conf.matrix[new.classes, ])
  return((2*precision*recall)/(precision + recall))
}

falseNew <- function(testReal, predictions, knownClasses){
  inewP <- !predictions %in% knownClasses
  ioldR <- testReal %in% knownClasses

  res <- inewP & ioldR
  return(length(which(res)) / length(which(ioldR)))
}

falseNew.incremental <- function(conf.matrix, knownClasses){
  new.classes <- c(1:nrow(conf.matrix))[c(1:nrow(conf.matrix)) != knownClasses]
  return(sum(conf.matrix[knownClasses, new.classes]) / sum(conf.matrix[knownClasses, ]))
}

accuracyPerClass <- function(testReal, predictions){
  sapply(unique(testReal), function(p){
    x <- subset(testReal, testReal == p)
    y <- subset(predictions, predictions == p)
    r <- matrix(x-y, ncol=1)
    length(which(r == T)) / length(which(r==F))
  })
}

plotEvaluation <- function(evalGlobal, chunk.size, exportPath=getwd(), whenNew = NULL, filename = NULL){
  evalGlobal$ix <- c(1:nrow(evalGlobal)*chunk.size)
  # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # g1 <- ggplot(data=evalGlobal, aes(ix)) + geom_line(aes(y=MissNew, group=1, colour=factor(2)), size=1) + scale_x_continuous(name='Chunk Number', expand=c(0,0), limits=c(0, nrow(evalGlobal)+10)) + scale_y_continuous(name='Miss New', expand=c(0,0), limits=c(0,1)) + theme_linedraw() + theme(legend.position="none") + xlab("Epochs") + ylab("Miss New")
  # g1 + annotate("point", x=whenNew, y=1, pch='x', size=6)
  # g2 <- ggplot(data=evalGlobal, aes(ix)) + geom_line(aes(y=FalseNew, group=1, colour=factor(2)), size=1) + ylim(c(0,1)) + theme_linedraw() + theme(legend.position="none") + xlab("Epochs") + ylab("False New")
  # g3 <- ggplot(data=evalGlobal, aes(ix)) + geom_line(aes(y=Error, group=1, colour=factor(3)), size=1) + ylim(c(0,1)) + theme_linedraw() + theme(legend.position="none") + xlab("Epochs") + ylab("Error")
  # g4 <- ggplot(data=evalGlobal, aes(ix)) + geom_line(aes(y=CorrectBetweenKnown, group=1, colour=factor(4)), size=1) + ylim(c(0,1)) + theme_linedraw() + theme(legend.position="none") + ylab("Correct Between Known") + xlab("Epochs")
  # g5 <- ggplot(data=evalGlobal, aes(ix)) + geom_line(aes(y=Correct, group=1, colour=factor(5)), size=1) + ylim(c(0,1)) + theme_linedraw() + theme(legend.position="none") + xlab("Epochs") + ylab("Correct")
  #
  # ggsave(paste0(exportPath, 'MissNew_', Sys.Date(), '_', Sys.time(), '.png'), g1)
  # ggsave(paste0(exportPath, 'FalseNew_', Sys.Date(), '_', Sys.time(), '.png'), g2)
  # ggsave(paste0(exportPath, 'Error_', Sys.Date(), '_', Sys.time(), '.png'), g3)
  # ggsave(paste0(exportPath, 'CorrectBetweenKnown_', Sys.Date(), '_', Sys.time(), '.png'), g4)
  # ggsave(paste0(exportPath, 'Correct_', Sys.Date(), '_', Sys.time(), '.png'), g5)
  for(i in 1:(ncol(evalGlobal)-1)){
    if(is.null(filename)){
      fname <- paste0(exportPath, names(evalGlobal)[i], Sys.Date(), '_', Sys.time(), '.png')
    }else{
      fname <- paste0(exportPath, filename, '_', names(evalGlobal)[i], '.png')
    }
    png(filename=fname)
    par(mar=c(5, 4, 4, 4) + 0.1)
    plot(x=evalGlobal$ix, evalGlobal[, i], type='l', xlab = 'Iteration', ylab=names(evalGlobal)[i], lwd=2)
    axis(side=3, at = whenNew, pch='x', labels = rep('x', length(whenNew)))
    # par(new=T)
    # plot(x=evalGlobal$ix, y=evalGlobal$NModels, xlab='', ylab='', ylim = c(0,10), axes=F, type='b', col='red')
    # mtext("Number of models at that chunk",side=4,col="red",line=2.5)
    # axis(4, ylim=c(0,10), col="red",col.axis="red",las=1)
    dev.off()
  }
}


plotPrediction2D <- function(data, preds, realLabels, exportPath = getwd()){
  df1 <- data
  df2 <- data
  df1$c <- realLabels
  df2$c <- preds
  df1$id = 'Real'
  df2$id = 'Predictions'
  df_all = rbind(df1, df2)
  df_all$c[df_all$c == -2] <- 'Novelty'
  df_all$c[df_all$c == -1] <- 'Outlier'
  g <- ggplot(df_all, aes(x=df_all[, 1], y=df_all[, 2], shape=factor(c), col=factor(c))) +
    geom_point() + scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,11,12,15,16,17,18,21,22,42)) +
    facet_wrap(~id) + theme(legend.position="top", legend.text=element_text(size=16)) + labs(colour = "Class", x='', y='', shape='Class')

  # g <- ggplot(data) + geom_point(aes(x = data[, 1], y = data[, 2], col=factor(realLabels)))
  # g2 <- ggplot(data) + geom_point(aes(x=data[, 1], y = data[, 2], col=factor(preds)))
  ggsave(paste0(exportPath, 'plotReal_', Sys.Date(), '_', Sys.time(), '.png'), g)
  # ggsave(paste0(exportPath, 'plotPreds_', Sys.Date(), '_', Sys.time(), '.png'), g2)
}

confusionMatrix.binary <- function(testReal, predictions){
  conf <- matrix(0, ncol=2, nrow=2)
  values <- unique(predictions)
  for(i in 1:length(testReal)){
    if(testReal[i] == values[1]){
      if(predictions[i] == values[1]) conf[1,1] <- conf[1,1] + 1
      else conf[1,2] <- conf[1,2] + 1
    }else{
      if(predictions[i] == values[1]) conf[2,1] <- conf[2,1] + 1
      else conf[2,2] <- conf[2,2] + 1
    }
  }
  colnames(conf) <- paste0('P', 1:2)
  rownames(conf) <- paste0('R', 1:2)
  return(conf)
}

conf.matrix.create <- function(testReal, predictions, knownClasses){
  # In case that the predictions are labels 1,2 and testReal labels are 3,2 this does not work. Hence, the labels are going to be replaced with testReal labels
  testReal.labels <- unique(testReal)
  predictions.labels <- unique(predictions)
  # res.matrix <- matrix(0, max(testReal.labels), max(predictions.labels))
  # for(ix.matrix in 1:length(testReal)){
  #   res.matrix[testReal[ix.matrix], predictions[ix.matrix]] <- res.matrix[testReal[ix.matrix], predictions[ix.matrix]] + 1
  # }
  # return(res.matrix)
  #
  #
  if(length(testReal.labels) == length(predictions.labels)){
    if(!all(testReal.labels == predictions.labels)){
      to.change.real <- setdiff(testReal.labels, predictions.labels)
      to.change.pred <- setdiff(predictions.labels, testReal.labels)
      # Just to check
      if(length(to.change.real) != length(to.change.pred)) stop("Error building conf. matrix -- The length of the relabelling is wrong!!")
      for(li in 1:length(to.change.real)){
        predictions[to.change.pred[li] == predictions] <- to.change.real[li]
      }
    }
  }
  conf.matrix <- Reduce('+', mapply(r=testReal, p=predictions, FUN = function(r, p){
    m <- matrix(0, ncol=max(max(testReal.labels), max(predictions.labels)), nrow=max(max(testReal.labels), max(predictions.labels)))
    m[r,p] <- 1
    return(list(m))
  }))
  # columns are predictions
  # rows are reals
  return(conf.matrix)
}

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

write.feature.vector <- function(weights, output.filename, type, models, quarantineInstances){
  # dts <- unlist(dts)
  # # dts.medidas <- read.csv2(file=f, sep=',', dec='.', header=T)
  # feature.vector <- c() # Just in case, borramos todo
  # target.vector <- c()# Just in case, borramos todo
  # ix.buffer <- which(names(dts) %like% 'I')
  # ix.models <- grep("model[1-9][.]mu", names(dts))
  # # my.models <- dts[ix.models]
  # # names(my.models)
  # # split(my.models, paste0('model', 1:(length(my.models)/2)), lex.order = F) # Siempre va a ser 2 porque estoy haciendo 2D sino cambiar!!!
  # ix.models <- c(ix.models, grep("model[1-9][.]s", names(dts)))
  # target.vector <- grep("w\\d$", names(dts))
  # # target.vector <- c(target.vector, grep("l\\d$", names(dts)))
  # target.vector <- dts[target.vector]
  # #####
  # buffer.dts <- dts[ix.buffer]

  feature.vector <- compute.features.regression(models, quarantineInstances, preds)
  res.data.frame <- data.frame(matrix(c(feature.vector, type, weights), nrow=1))
  # print(res.data.frame)

  if(file.exists(output.filename)){
    write.table(x = res.data.frame, file = output.filename, append = T, sep = ',', dec='.', row.names = F, col.names = F)
  }else{
    write.table(x = res.data.frame, file = output.filename, append = F, sep = ',', dec='.', row.names = F, col.names = T)
  }
}

calculateMeansVarsFromStream <- function(stream, class.number){
  st <- which(stream == class.number)
  res <- st[1] - 1
  res <- c(res, unlist(sapply(2:length(st), function(ix.st){
    st[ix.st] - st[(ix.st-1)] - 1
  })))
  mean.res <- mean(res)
  var.res <- var(res)
  mean.res[is.na(mean.res)] <- -1
  var.res[is.na(var.res)] <- -1
  return(list(mean.res, var.res))
}
