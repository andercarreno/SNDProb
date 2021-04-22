getScenarios <- function(num.train){
  num.dimensions <- 2
  if(num.train == 1){
    scenarios <- list('S1' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(3,1,1,3), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S2' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S3' = list('model.tr.1' = list('means' = c(5,8), 'sigma'=matrix(c(4,-4,-4,8), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S4' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions))),
                      'S5' = list('model.tr.1' = list('means' = c(4,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(8,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions))))
  }
  if(num.train == 2){
    scenarios <- list('S1' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(3,1,1,3), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(3,1,1,1), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S2' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,1,1,8), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S3' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,-4,-4,8), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S4' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(9,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S5' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(4,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(8,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions))))
  }else if(num.train == 3){
    scenarios <- list('S1' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(3,1,1,3), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(3,1,1,1), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(-2,10), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S2' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,1,1,8), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(2,-8), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S3' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,-4,-4,8), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(2,2), 'sigma'=matrix(c(0.5,0,0,0.5), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S4' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(14,7), 'sigma'=matrix(c(0.75,0,0,0.75), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(9,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S5' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(4,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(12,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(8,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions))))
  }else if(num.train == 4){
    scenarios <- list('S1' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(3,1,1,3), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(3,1,1,1), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(-2,10), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions)),
                                  'model.tr.4' = list('means' = c(-8,3), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S2' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,1,1,8), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(2,-8), 'sigma'=matrix(c(5,2,2,1), ncol=num.dimensions)),
                                  'model.tr.4' = list('means' = c(-5,-7), 'sigma'=matrix(c(4,2,2,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,0,0,3), ncol=num.dimensions))),
                      'S3' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(5,8), 'sigma'=matrix(c(4,-4,-4,8), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(2,2), 'sigma'=matrix(c(0.5,0,0,0.5), ncol=num.dimensions)),
                                  'model.tr.4' = list('means' = c(-1,5), 'sigma'=matrix(c(0.5,0,0,0.5), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(7,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S4' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,-1.85,-1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(14,7), 'sigma'=matrix(c(0.75,0,0,0.75), ncol=num.dimensions)),
                                  'model.tr.4' = list('means' = c(15,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(9,0), 'sigma'=matrix(c(3,2,2,5), ncol=num.dimensions))),
                      'S5' = list('model.tr.1' = list('means' = c(0,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.2' = list('means' = c(4,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.3' = list('means' = c(12,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.tr.4' = list('means' = c(16,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions)),
                                  'model.ts.1' = list('means' = c(8,0), 'sigma'=matrix(c(2,1.85,1.85,2), ncol=num.dimensions))))
  }

  return(scenarios)
}

plotScenarios <- function(scenarios){
  lapply(1:length(scenarios), function(i.s){
    s <- scenarios[[i.s]]
    data <- NULL
    n <- 3000
    # pdf(paste0('Documents/EHU/EvolvingClasses/DocsForDirectors/ScenariosClassif3_', i.s, '.pdf'))
    png(paste0('Documents/EHU/EvolvingClasses/DocsForDirectors/ScenariosClassif3_', i.s, '.png'), res = 400, width = 5, height=5, units = "in")
    par(mai=c(1,1,0.3,0.3))
    data <- lapply(1:length(s), function(m.ix){
      data.frame(mvtnorm::rmvnorm(n, mean = s[[m.ix]]$means, sigma = s[[m.ix]]$sigma), 'class'=names(s)[m.ix])
    })
    data <- do.call(rbind, data)
    # colors.to.plot <- sort(rep(c(unique(as.numeric(data$class))+1), n))
    plot(x=data[, 1], y=data[, 2], col=as.numeric(data[,3])+1, xlab = "X1", ylab = "X2", cex.axis=1.4, cex.lab=1.8)
    # g <- ggplot(data=data) + geom_point(aes(x=X1, y=X2, col=class)) + theme_linedraw() + ggtitle(names(scenarios)[i.s]) + theme(legend.position = "none")
    # return(g)
    dev.off()
  })
}

getStrategies <- function(num.train){
  if(num.train == 1){
    strategies <- list('Str1' = list('model.tr.1' = list('width' = -0.1, 'At.which.break'=2000),
                                     'model.ts.1' = list('width' = 0.1,  'At.which.break'=3000)),
                       'Str2' = list('model.tr.1' = list('width' = 0.01, 'At.which.break'=2000),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=3000)),
                       'Str3' = list('model.tr.1' = list('width' = -0.1, 'At.which.break'=4000),
                                     'model.ts.1' = list('width' = 0.4,  'At.which.break'=1000)),
                       'Str4' = list('model.tr.1' = list('width' = -0.01, 'At.which.break'=1000),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=2000))
                      )
  }else if(num.train == 2){
    strategies <- list('Str1' = list('model.tr.1' = list('width' = -0.01, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = 0.001,  'At.which.break'=2000),
                                     'model.ts.3' = list('width' = 0.001,  'At.which.break'=3000)),
                       'Str2' = list('model.tr.1' = list('width' = 0.01, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = 0.01, 'At.which.break'=2000),
                                     'model.ts.3' = list('width' = 0.01, 'At.which.break'=3000)),
                       'Str3' = list('model.tr.1' = list('width' = -0.3, 'At.which.break'=1500),
                                     'model.tr.2' = list('width' = -0.1, 'At.which.break'=4000),
                                     'model.ts.3' = list('width' = 0.4,  'At.which.break'=1000)),
                       'Str4' = list('model.tr.1' = list('width' = -0.2, 'At.which.break'=4000),
                                     'model.tr.2' = list('width' = 0.8,  'At.which.break'=1000),
                                     'model.ts.3' = list('width' = -0.2, 'At.which.break'=3000)),
                       'Str5' = list('model.tr.1' = list('width' = -0.8, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = -0.1, 'At.which.break'=4000),
                                     'model.ts.3' = list('width' = -0.01, 'At.which.break'=2000)),
                       'Str6' = list('model.tr.1' = list('width' = -0.009, 'At.which.break'=1500),
                                     'model.tr.2' = list('width' = 0.001,  'At.which.break'=3000),
                                     'model.ts.3' = list('width' = 0.01, 'At.which.break'=3500))
                      )
  }else if (num.train == 3){
    strategies <- list('Str1' = list('model.tr.1' = list('width' = -0.01, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = -0.001,  'At.which.break'=1700),
                                     'model.tr.3' = list('width' = -0.001,  'At.which.break'=2500),
                                     'model.ts.1' = list('width' = 0.1,  'At.which.break'=3800)),
                       'Str2' = list('model.tr.1' = list('width' = 0.01, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = 0.01, 'At.which.break'=2000),
                                     'model.tr.3' = list('width' = 0.01, 'At.which.break'=3000),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=4000)),
                       'Str3' = list('model.tr.1' = list('width' = -0.3, 'At.which.break'=1500),
                                     'model.tr.2' = list('width' = -0.1, 'At.which.break'=4000),
                                     'model.tr.3' = list('width' = -0.2, 'At.which.break'=1000),
                                     'model.ts.1' = list('width' = 0.4,  'At.which.break'=1000)),
                       'Str4' = list('model.tr.1' = list('width' = -0.2, 'At.which.break'=4000),
                                     'model.tr.2' = list('width' = 0.8,  'At.which.break'=1000),
                                     'model.tr.3' = list('width' = 0.01,  'At.which.break'=3000),
                                     'model.ts.1' = list('width' = -0.2, 'At.which.break'=1000)),
                       'Str5' = list('model.tr.1' = list('width' = -0.8, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = -0.1, 'At.which.break'=4000),
                                     'model.tr.3' = list('width' = -0.1, 'At.which.break'=2000),
                                     'model.ts.1' = list('width' = -0.01, 'At.which.break'=1500)),
                       'Str6' = list('model.tr.1' = list('width' = -0.1, 'At.which.break'=1500),
                                     'model.tr.2' = list('width' = 0.1,  'At.which.break'=4000),
                                     'model.tr.3' = list('width' = 0.1,  'At.which.break'=2000),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=4500))
                      )
  }else if (num.train == 4){
    strategies <- list('Str1' = list('model.tr.1' = list('width' = -0.01, 'At.which.break'=1000),
                                     'model.tr.2' = list('width' = -0.001,  'At.which.break'=3000),
                                     'model.tr.3' = list('width' = 0.01,  'At.which.break'=2500),
                                     'model.tr.4' = list('width' = -0.2,  'At.which.break'=4000),
                                     'model.ts.1' = list('width' = 0.1,  'At.which.break'=4000)),
                       'Str2' = list('model.tr.1' = list('width' = 0.01, 'At.which.break'=800),
                                     'model.tr.2' = list('width' = 0.01, 'At.which.break'=1600),
                                     'model.tr.3' = list('width' = 0.01, 'At.which.break'=2400),
                                     'model.tr.4' = list('width' = 0.01, 'At.which.break'=3200),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=4000)),
                       'Str3' = list('model.tr.1' = list('width' = -0.03, 'At.which.break'=1500),
                                     'model.tr.2' = list('width' = -0.01, 'At.which.break'=4000),
                                     'model.tr.3' = list('width' = -0.001, 'At.which.break'=4000),
                                     'model.tr.4' = list('width' = 0.01, 'At.which.break'=1000),
                                     'model.ts.1' = list('width' = 0.0001,  'At.which.break'=2000)),
                       'Str4' = list('model.tr.1' = list('width' = -0.02, 'At.which.break'=4000),
                                     'model.tr.2' = list('width' = 0.008,  'At.which.break'=2000),
                                     'model.tr.3' = list('width' = 0.008,  'At.which.break'=3600),
                                     'model.tr.4' = list('width' = 0.008,  'At.which.break'=1500),
                                     'model.ts.1' = list('width' = -0.02, 'At.which.break'=3000)),
                       'Str5' = list('model.tr.1' = list('width' = -0.8, 'At.which.break'=4000),
                                     'model.tr.2' = list('width' = -0.1, 'At.which.break'=3000),
                                     'model.tr.3' = list('width' = -0.1, 'At.which.break'=1800),
                                     'model.tr.4' = list('width' = -0.1, 'At.which.break'=1000),
                                     'model.ts.1' = list('width' = -0.01, 'At.which.break'=1500)),
                       'Str6' = list('model.tr.1' = list('width' = -0.1, 'At.which.break'=2000),
                                     'model.tr.2' = list('width' = 0.1,  'At.which.break'=1000),
                                     'model.tr.3' = list('width' = 0.1,  'At.which.break'=500),
                                     'model.tr.4' = list('width' = -0.1,  'At.which.break'=1800),
                                     'model.ts.1' = list('width' = 0.01, 'At.which.break'=2500))
                      )
  }
  return(strategies)
}

create.strategy <- function(n.train, n.test, widths, break.points){
  if(length(widths) != n.train + n.test)
    stop("The number of width values does not match the number of models (n.train + n.test)")
  if(length(break.points) != n.train + n.test)
    stop("The number of break.point values does not match the number of models (n.train + n.test)")

  strategies <- lapply(1:(n.train + n.test), function(ix){
    list('width'=widths[ix], 'At.which.break'=break.points[ix])
  })
  names(strategies) <- c(paste0('model.tr.', 1:n.train), paste0('model.ts.', 1:n.test))
  return(strategies)
}

create.scenario <- function(n.train, n.test, mean.values, sigma.values){
  if(length(mean.values) != n.train + n.test)
    stop("The number of mean values does not match the number of models (n.train + n.test)")
  if(length(sigma.values) != n.train + n.test)
    stop("The number of sigma values does not match the number of models (n.train + n.test)")

  scenario <- lapply(1:(n.train + n.test), function(ix){
    list('means'=mean.values[ix], 'sigma'=sigma.values[ix])
  })
  names(scenario) <- c(paste0('model.tr.', 1:n.train), paste0('model.ts.', 1:n.test))
  return(scenario)
}

sigmoid.function <- function(width, break.point, it){
  return(1/(1+exp(width*(break.point-it))))
}

getProbabilities <- function(models.strategies, it){
  r <- sapply(models.strategies, FUN = function(ms){
    sigmoid.function(width=ms$width, break.point = ms$At.which.break, x=it)
  })
  r/sum(r)
}

plotStrategies <- function(strategies, x.min=1, x.max=7000, steps.x = 1){
  require(png)
  x.seq <- seq(x.min, x.max, steps.x)
  num.classes <- length(strategies[[1]])
  if(num.classes == 2){
    sapply(1:length(strategies), function(models.strategies){
      plot(x = x.seq, y=sapply(x.seq, getProbabilities,   models.strategies = strategies[[models.strategies]])[1, ], col='red', ylim = c(0,1), xlab = "Iteration", ylab = "Probability of sampling", cex.axis=1.4, cex.lab=1.8)
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[2, ], col='green')
      legend("topright", inset = 0.2, legend=c("TR 1", "PRED 1"), col=c("red", "green"), pch=c(19,19) , cex=0.8)
    })
  }else if(num.classes == 3){
    sapply(1:length(strategies), function(models.strategies){
      # png(paste0('Documents/EHU/EvolvingClasses/DocsForDirectors/StrategiesClassif3_', models.strategies, '.png'), res = 400, width = 5, height=5, units = "in")
      # par(mai=c(1,1,0.3,0.3))
      plot(x = x.seq, y=sapply(x.seq, getProbabilities,   models.strategies = strategies[[models.strategies]])[1, ], col='red', ylim = c(0,1), xlab = "Iteration", ylab = "Probability of sampling", cex.axis=1.4, cex.lab=1.8, type='l', lwd=12)
      lines(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[2, ], col='green', lwd=12)
      lines(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[3, ], col='blue', lwd=12)
      legend(1, 0.9, legend=c("TR 1", "TR 2", "PRED 1"), col=c("red", "green", "blue"), pch=c(19,19,19) , cex=0.8)
      # dev.off()
    })
  }else if(num.classes == 4){
    sapply(1:length(strategies), function(models.strategies){
      plot(x = x.seq, y=sapply(x.seq, getProbabilities,   models.strategies = strategies[[models.strategies]])[1, ], col='red', ylim = c(0,1), xlab = "Iteration", ylab = "Probability of sampling", main=paste0("Strategy ", models.strategies))
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[2, ], col='blue')
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[3, ], col='green')
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[4, ], col='cyan')
      legend(1, 0.9, legend=c("TR 1", "TR 2", "TR 3", "PRED 1"), col=c("red", "green", "blue", "cyan"), pch=c(19,19,19,19) , cex=0.8)
    })
  }else if(num.classes == 5){
    sapply(1:length(strategies), function(models.strategies){
      plot(x = x.seq, y=sapply(x.seq, getProbabilities,   models.strategies = strategies[[models.strategies]])[1, ], col='red', ylim = c(0,1), xlab = "Iteration", ylab = "Probability of sampling", main=paste0("Strategy ", models.strategies))
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[2, ], col='blue')
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[3, ], col='green')
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[4, ], col='cyan')
      points(x = x.seq, y=sapply(x.seq, getProbabilities, models.strategies = strategies[[models.strategies]])[5, ], col='purple')
      legend(1, 0.9, legend=c("TR 1", "TR 2", "TR 3", "TR 4", "PRED 1"), col=c("red", "green", "blue", "cyan", "purple"), pch=c(19,19,19,19,19) , cex=0.8)
    })
  }
  dev.off()
  return()
}


generateRandomScenario <- function(num.dimensions, num.train, num.test){
  lambda <- runif(1, min = 0, max=100)
  train.models <- lapply(1:num.train, function(nt){
    mean <- runif(2, min=-10, max=20)
    sigma <- rWishart(1, df=4, Sigma = diag(2))[,,1]
    list('means'=mean, 'sigma'=sigma)
  })
  names(train.models) <- paste0('model.tr.', 1:length(train.models))
  test.models <- lapply(1:num.test, function(nt){
    mean <- runif(2, min=-10, max=20)
    sigma <- rWishart(1, df=4, Sigma = diag(2))[,,1]
    list('means'=mean, 'sigma'=sigma)
  })
  names(test.models) <- paste0('model.ts.', 1:length(test.models))
  return(append(train.models, test.models))
}

bayes.error <- function(models, dataset, ix.class = ncol(dataset)){
  tau <- rep(1, length(models)) / length(models)
  dataset.target <- dataset[, ix.class]
  D <- dataset[, -ix.class]
  if(is.null(models[[1]]$invCov)) for(ix in 1:length(models)) models[[ix]]$invCov <- solve(models[[ix]]$sigma)
  else if(is.null(models[[1]]$determ)) for(ix in 1:length(models)) models[[ix]]$determ <- base::det(models[[ix]]$sigma)
  mahalanobis.dist <- sapply(models, function(m){
    apply(D, MARGIN=1, stats::mahalanobis, center=m$means, cov=m$invCov, inverted=T)
  })
  print(mahalanobis.dist)
  densities <- sapply(1:length(models), function(j){
    ((2*pi)^-(ncol(D)/2)) * (models[[j]]$determ^-0.5) * exp(-0.5*mahalanobis.dist[, j])
  })
  densities[densities== 0] <- densities[densities== 0] + 1e-50
  x <- (densities * tau)
  probs <- t(x/rowSums(x))
  probs[is.nan(probs)] <- 0
  if(is.vector(probs)){
    probs <- matrix(probs, nrow = length(mixtures))
  }
  preds <- apply(probs, MARGIN=2, which.max)
  return(sum(preds != dataset.target) / nrow(D))
}

sample.scenario <- function(scenario, strat, n.instances, iteration = 1){
  res <- t(sapply(1:n.instances, function(itimes){
    x <- runif(1)
    ref.probs <- getProbabilities(strat, iteration)
    ix.model <- 1
    while(x >= sum(ref.probs[1:ix.model])){ # Roulette wheel
      ix.model <- ix.model + 1
    }
    D <- mvtnorm::rmvnorm(n = 1, mean = scenario[[ix.model]]$mean, sigma=scenario[[ix.model]]$sigma)
    D.target <- ix.model
    return(cbind(D, 'class'=D.target))
  }))
  res <- as.data.frame(res)
}

sample.realWorld.scenario <- function(strategy, iteration=1){
  x <- runif(1)
  ref.probs <- getProbabilities(strategy, iteration)
  ix.model <- which.max(ref.probs)
  return(ix.model)
}
