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
iteration.sim <- 1:15000 # Range of iterations of the experiment. Totally 7000 iterations are run

load('./meta-regression/classifier_model.Rmodel')
load('./meta-regression/dts_cd.Rmodel')
load('./meta-regression/dts_nd.Rmodel')
normalize <- function(x){
  return((x-mean(x))/var(x))
}
# for(i in 2:length(dts.cd)){
#   x <- 1:(ncol(dts.cd[[i]])-i-1)
#   y <- c(1:ncol(dts.cd[[i]]))[-x]
#   dts.cd[[i]] <- cbind(apply(dts.cd[[i]][, x], MARGIN=2, FUN=normalize), dts.cd[[i]][, y])
#   dts.nd[[i]] <- cbind(apply(dts.nd[[i]][, x], MARGIN=2, FUN=normalize), dts.nd[[i]][, y])
# }

print("SUMMARY OF PARAMETERS")
print(paste0("Percentile: ", percentile))
print(paste0("MaxBuffer: ", maxBuffer))
print(paste0("Distribution: ", distrFamily))
print(paste0("Export path: ", exportPath))

scenarios <- getScenarios(num.train) # Load all the scenarios
strategies <- getStrategies(num.train) # Load all the strategies

allQuarantine <- c()
for (scenario.ix in 1:1){ # Select the range of scenarios to run
  dir.create(paste0(exportPath, '/Scenario', scenario.ix, '/'))
  for (strategy.ix in 1:1){ # Select the number of strategies to run with the previously selected scenario
    dir.create(paste0(exportPath, '/Scenario', scenario.ix, '/Strategy', strategy.ix, '/'))
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
    # dTest <- read.csv(file='Path_to_my_TEST.csv', sep=',', dec='.')
    # names(dTrain) <- c(paste0('X', 1:(ncol(dTrain)-1)), 'class')
    # names(dTest) <- c(paste0('X', 1:(ncol(dTest)-1)), 'class')
    # exportPath <- paste0('./')

    models <- trainInitialModel.parametricNovelty(dTrain, percentile, method=distrFamily)
    whereNewClass <- c()
    print("====================")
    print("The STREAM begins:")
    print("====================")
    i <- 1
    IX_SEQ <- 1
    EX_SEQ <- i
    nIterations <- 1
    evalGlobal <- data.frame('MissNew'=0.0, 'FalseNew'=0.0, 'Error'=0.0, 'CorrectBetweenKnown'=0.0, 'Correct'=0.0, 'EN_Accuracy'=0.0, 'NModels'=0)[0, ]
    preds <- c()
    quarantine <- c()
    buffer.iterations <- c(0)
    quarantineInstances <- dTest[0, -which("class" == colnames(dTest))] # Just to copy the structure for later rbinds
    knownClasses <- unique(dTrain$class)
    algunavez.quarantine.llena <- F
    allQuarantine <- c()
    next.class.label <- max(sapply(models, function(m) m$toWhoClassify) + 1)
    while (i <= nrow(dTest)){
      # if(i==999){
      #   stop("Parar iteracion")
      # }
      if(i %% 500 == 0)
        print(paste0('Iteration ', i))
      D <- dTest[i, -which("class" == colnames(dTest))]
      D <- as.matrix(D)
      D.target <- dTest[i, which("class" == colnames(dTest))]
      if(any(!(unique(D.target) %in% knownClasses))){
        whereNewClass <- c(whereNewClass, i)
      }
      res.prediction <- predict.parametricNovelty(data = D, models = models, quarantine = quarantine, quarantineInstances = quarantineInstances,
                                                  old.preds = preds, maxBuffer = maxBuffer, next.class.label = next.class.label, method = distrFamily,
                                                  nClustersToSeek = nClustersToSeek, epoch.start = IX_SEQ,
                                                  regression.model = regression.model, export.path = exportPath)
      # stop("EPA")
      if(length(res.prediction$models) > length(models)){
        knownClasses <- knownClasses + 1
      }
      nIterations <- nIterations+1
      models <- res.prediction$models
      preds <- res.prediction$preds
      if(length(res.prediction$quarantine) > 0){
        algunavez.quarantine.llena <- T
      }
      buffer.iterations <- res.prediction$buffer.iterations
      knownClasses <- unique(c(knownClasses, as.numeric(names(table(dTest[i, which("class" == colnames(dTest))])))))
      # PLOT ELLIPSE
      # plotPrediction2D(data = dTest[1:i, -which(names(dTest) == 'class')], preds = preds[1:i], realLabels = dTest[1:i, which(names(dTest) == 'class')], exportPath = paste0('Desktop/Plots/tmp/'))
      
      ### If it is a 2 dimensions dataset you can plot and store the plots uncommenting this lines
      # if(i %% 500 == 0){
      #   models.to.plot <- models
      #   setEPS()
      #   # postscript(paste0(exportPath, '/plot_', i, '.eps'),  width = 5, height = 5, units = "in")
      #   cairo_ps(file = paste0(exportPath, '/plot_', i, '.eps'), width = 5, height = 5, onefile = FALSE, fallback_resolution = 600)
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
      if(i %% calculateMeasures == 0){
        mypreds <- preds
        evalGlobal <- rbind(evalGlobal, evaluateStreamingNoveltyDetection(dTest$class[1:i], preds[1:i], quarantine, knownClasses, length(models), IX_SEQ))
      }
      
      if(length(res.prediction$quarantine) <= 0 && algunavez.quarantine.llena){
        IX_SEQ <- i
        algunavez.quarantine.llena <- F
        next.class.label <- max(sapply(models, function(m) m$toWhoClassify) + 1)
      }
      
      i <- i + 1
      EX_SEQ <- EX_SEQ + 1
    }
    # Writes some performance data. The output of the experiments.
    write.table(x=allQuarantine, file=paste0(exportPath, 'buffer.csv'), append=F, col.names=F, row.names = F)
    write.table(x=data.frame('preds'=preds, 'class'=dTest$class), file=paste0(exportPath, 'PredictionsOriginalSNDProb.csv'), sep=',', append=F, col.names=F, row.names = F)
    
    dir.create(paste0(exportPath))
    write.csv(colMeans(evalGlobal), file=paste0(exportPath, '/', maxBuffer, '_', 'EvalGlobal_Mean', format(Sys.time(), "%H_%M_%S"), '.csv'))
    write.table(evalGlobal, file=paste0(exportPath, '/', maxBuffer, '_', 'EvalGlobal', '.csv'), sep=',', dec='.')
    plotEvaluation(evalGlobal = evalGlobal, chunk.size=calculateMeasures, exportPath = exportPath, whenNew = whereNewClass, filename=maxBuffer)
  }
}
