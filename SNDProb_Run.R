distrFamily <- 'gaussian'
args = commandArgs(trailingOnly=TRUE)


source('./SNDProb.R') # Loads the functions of the SNDProb
source('./Evaluation.R') # Loads the evaluation
source('./dataGenerators/Scenarios.R') # Loads the strategies and the scenarios

# LIBRARIES
library(caret)
library(mvtnorm)
library(OneR)
library(MASS)
library(mclust)
library(clue)
library(parallel)
library(randomForest)

library(onlinePCA)
library(data.table)
library(lcmix)
library(mixtools)

library(ggplot2)

set.seed(22)

# PARAMETERS
maxBuffer <- 500 # Buffer size
nClustersToSeek <- 1 # Number of clusters to seek at every buffer fill
percentile <- .98 # 1-\alpha = percentile
calculateMeasures <- 1 # Calculate the evaluation measures at every iteration
exportPath <- './'
num.train <- 2 # Number of offline classes
n.instances.start <- 5000 # Number of instances from each class used in the offline phase
iteration.sim <- 1:7001 # Range of iterations of the experiment. Totally 7000 iterations are run

load('./meta-regression/classifier_model.Rmodel')
load('./meta-regression/dts_cd.Rmodel')
load('./meta-regression/dts_nd.Rmodel')
normalize <- function(x){
  return((x-mean(x))/var(x))
}
for(i in 2:length(dts.cd)){
  x <- 1:(ncol(dts.cd[[i]])-i-1)
  y <- c(1:ncol(dts.cd[[i]]))[-x]
  dts.cd[[i]] <- cbind(apply(dts.cd[[i]][, x], MARGIN=2, FUN=normalize), dts.cd[[i]][, y])
  dts.nd[[i]] <- cbind(apply(dts.nd[[i]][, x], MARGIN=2, FUN=normalize), dts.nd[[i]][, y])
}

print("SUMMARY OF PARAMETERS")
print(paste0("Percentile: ", percentile))
print(paste0("MaxBuffer: ", maxBuffer))
print(paste0("Distribution: ", distrFamily))
print(paste0("Export path: ", exportPath))

scenarios <- getScenarios(num.train) # Load all the scenarios
strategies <- getStrategies(num.train) # Load all the strategies

allQuarantine <- c()
for (scenario.ix in 1:1){ # Select the range of scenarios to run
  dir.create(paste0(exportPath, '/Scenario', scenario.ix, '/')
  for (strategy.ix in 1:1){ # Select the number of strategies to run with the previously selected scenario
    dir.create(paste0(exportPath, '/Scenario', scenario.ix, '/Strategy', strategy.ix, '/')
    tosample <- sample(x = 1:2, size = n.instances.start, replace = T)
    # Generate data
    dTrain <- data.frame(mvtnorm::rmvnorm(sum(tosample==1), scenarios[[scenario.ix]]$model.tr.1$means, sigma = scenarios[[scenario.ix]]$model.tr.1$sigma), 'class'=1)
    dTrain <- rbind(dTrain, data.frame(mvtnorm::rmvnorm(sum(tosample==2), scenarios[[scenario.ix]]$model.tr.2$means, sigma = scenarios[[scenario.ix]]$model.tr.2$sigma), 'class'=2))
    names(dTrain) <- c('X1', 'X2', 'class')
    dTest <- dTrain[0, ]
    for (it in iteration.sim)
      dTest <- rbind(dTest, sample.scenario(scenarios[[scenario.ix]], strategies[[strategy.ix]], n.instances = 1, iteration = it))
    names(dTest) <- c('X1', 'X2', 'class')

    ########################################################## MANUALLY LOAD A CSV!!!!!!!!!!!!
    # dTrain <- read.csv(file="Path_to_my_TRAIN.csv", sep=',', dec='.')
    # dTest <- read.csv(file='Path_to_my_TEST.csv', sep=',', dec='.')
    # names(dTrain) <- c(paste0('X', 1:(ncol(dTrain)-1)), 'class')
    # names(dTest) <- c(paste0('X', 1:(ncol(dTest)-1)), 'class')
    # exportPath <- paste0('~/Documents/EHU/EvolvingClasses/DocsForDirectors/ExperimentsSynthetic/CoverForestActual/Strategy5/')

    models <- trainInitialModel.parametricNovelty(dTrain, percentile, method=distrFamily)
    whereNewClass <- c()
    print("====================")
    print("The STREAM begins:")
    print("====================")
    i <- 1
    IX_SEQ <- 1
    EX_SEQ <- i
    NUM_CORES <- 3
    nIterations <- 1
    evalGlobal <- data.frame('MissNew'=0.0, 'FalseNew'=0.0, 'Error'=0.0, 'CorrectBetweenKnown'=0.0, 'Correct'=0.0, 'EN_Accuracy'=0.0, 'NModels'=0)[0, ]
    preds <- c()
    quarantine <- c()
    buffer.iterations <- c(0)
    quarantineInstances <- dTest[0, -which("class" == colnames(dTest))] # Just to copy the structure for later rbinds
    knownClasses <- unique(dTrain$class)
    i <- 1
    algunavez.quarantine.llena <- F
    while (i <= nrow(dTest)){
      # for(i in 1:nrow(dTest)){
      # if(i == 95)
      #   stop('Wait')
      if(i %% 1000 == 0)
        print(paste0('Iteration ', i))
      D <- dTest[i, -which("class" == colnames(dTest))]
      D <- as.matrix(D)
      D.target <- dTest[i, which("class" == colnames(dTest))]
      if(any(!(unique(D.target) %in% knownClasses))){
        whereNewClass <- c(whereNewClass, i)
      }
      res.prediction <- predict.parametricNovelty(data = D, models = models, quarantine = quarantine, quarantineInstances = quarantineInstances,
                                                  old.preds = preds, maxBuffer = maxBuffer, method = distrFamily,
                                                  nClustersToSeek = nClustersToSeek, epoch.start = IX_SEQ,
                                                  regression.model = regression.model, export.path = exportPath)
      # print(paste0("hecho ", i))
      nIterations <- nIterations+1
      models <- res.prediction$models
      preds <- res.prediction$preds
      if(length(res.prediction$quarantine) > 0)
        algunavez.quarantine.llena <- T
      if(length(res.prediction$quarantine) <= 0 && algunavez.quarantine.llena){
        IX_SEQ <- i
        algunavez.quarantine.llena <- F
      }
      buffer.iterations <- res.prediction$buffer.iterations
      knownClasses <- unique(c(knownClasses, as.numeric(names(table(dTest[i, which("class" == colnames(dTest))])))))

      # PLOT THE STREAM SNAPSHOTS
      # if(i %% 500 == 0){
      #   if(distrFamily == "mixtureGaussian"){
      #     models.to.plot <- unlist(lapply(models, function(m){
      #       lapply(1:length(m$sigma), function(nComp){
      #         list('means'=m$means[[nComp]],
      #              'sigma'=m$sigma[[nComp]],
      #              'N'=m$N*m$weightK[nComp],
      #              'ImportanceN' = m$ImportanceN*m$weightK[nComp],
      #              'boundary' = m$boundary,
      #              'toWhoClassify' = m$toWhoClassify,
      #              'invCov' = m$invCov[[nComp]],
      #              'weightK' = m$weightK[nComp])
      #       })
      #     }), recursive=F)
      #   }else{
      #     models.to.plot <- models
      #   }
      #
      #   # pdf(paste0(exportPath, '/plot_', i, '.pdf'))
      #   png(paste0(exportPath, '/plot_', i, '.png'), width = 5, height = 5, units = "in", res=400)
      #   par(mai=c(1,1,0.3,0.3))
      #   plot(x=dTest$X1[1:i], y=dTest$X2[1:i], col=alpha(as.factor(preds[1:i]+ 1), 0.4), cex.axis=1.4, xlab="X1", ylab="X2", cex.lab=1.8, xlim = c(-20,15), ylim=c(-10, 20))
      #   # , xlim = c(-20, 15), ylim=c(-10,20)
      #   points(x=quarantineInstances[, 1], y=quarantineInstances[, 2], col=as.factor(0))
      #   legend('bottomright', legend = unique(preds[1:i]+ 1), fill=unique(as.factor(preds[1:i]+ 1)))
      #   for(m in models.to.plot){
      #     car::ellipse(center = c(m$means), shape = m$sigma, radius = sqrt(m$boundary), add = T, fill=F, fill.alpha = .4, col='black')
      #   }
      #   dev.off()
      # }
      quarantineInstances <- res.prediction$quarantineInstances
      quarantine <- res.prediction$quarantine
      allQuarantine <- c(allQuarantine, length(quarantine))
      if(i %% calculateMeasures == 0)
        evalGlobal <- rbind(evalGlobal, evaluateStreamingNoveltyDetection(dTest$class[1:i], res.prediction$preds[1:i], res.prediction$quarantine, knownClasses, length(models)))

      i <- i + 1
      EX_SEQ <- EX_SEQ + 1
    }
    # Export predictions
    write.table(data.frame("Real" = dTest$class, "Preds" = preds), file=paste0(exportPath, '/PredictionsSNDProb.csv'), sep=',', dec='.')
    # Export the evolution of the buffer
    write.table(x=allQuarantine, file=paste0(exportPath, 'buffer.csv'), append=T, col.names=F, row.names = F)

    dir.create(paste0(exportPath))
    # Exports the evaluation measures
    write.csv(colMeans(evalGlobal), file=paste0(exportPath, '/', maxBuffer, '_', 'EvalGlobal_Mean', format(Sys.time(), "%H_%M_%S"), '.csv'))
    write.table(evalGlobal, file=paste0(exportPath, '/', maxBuffer, '_', 'EvalGlobal', '.csv'), sep=',', dec='.')
    plotEvaluation(evalGlobal = evalGlobal, chunk.size=calculateMeasures, exportPath = exportPath, whenNew = whereNewClass, filename=maxBuffer)
  }
}
