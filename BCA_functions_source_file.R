# A collection of source code of select functions from the 
# BCA, RcmdrPlugin.BCA, RcmdrMisc, and Rcmdr.Utilities packages
# See https://cran.r-project.org/web/packages/BCA/index.html or ??BCA for more information

# Originally created by Dan Putler et al, 
#          modified by Ryan Peyman Tavakol and Robert Evert Krider
# To load functions into the global environment, run > source(path_to_file) locally

# List of functions ----
# variable.summary (modified)
# create.samples
# binVariable
# numSummary
# lift.chart
# Nnet & nnSub
# rankScore
# rawProbScore
# adjProbScore


# BCA::variable.summary() ----
variable.summary <- function (dframe) 
{
  dframe <- as.data.frame(dframe)  # modification here: make sure dframe is type data frame, not tibble for example
  if (!is.data.frame(dframe)) 
    stop("The object is not a data frame.")
  Class <- unlist(lapply(1:ncol(dframe), function(i) class(dframe[, 
                                                                  i])))
  NAs <- 100 * (apply(dframe, 2, function(x) sum(as.numeric(is.na(x))))/nrow(dframe))
  varNum <- 1:length(names(dframe))
  Levels <- rep(NA, length(varNum))
  Min.Level <- rep(NA, length(varNum))
  Mean <- rep(NA, length(varNum))
  SD <- rep(NA, length(varNum))
  if (any(Class == "factor")) {
    varNumFac <- varNum[Class == "factor"]
    totLevel <- unlist(lapply(varNumFac, function(i) length(summary(dframe[, 
                                                                           i], maxsum = nrow(dframe)))))
    minLevel <- unlist(lapply(varNumFac, function(i) min(summary(dframe[, 
                                                                        i], maxsum = nrow(dframe)))))
    Levels <- rep(NA, length(varNum))
    Levels[varNumFac] <- totLevel
    Min.Level[varNumFac] <- minLevel
  }
  if (any(Class == "numeric") | any(Class == "integer")) {
    varNumNum <- varNum[Class == "numeric" | Class == "integer"]
    Means <- unlist(lapply(varNumNum, function(i) mean(dframe[, 
                                                              i], na.rm = TRUE)))
    Mean <- rep(NA, length(varNum))
    Mean[varNumNum] <- Means
    SDs <- unlist(lapply(varNumNum, function(i) sd(dframe[, 
                                                          i], na.rm = TRUE)))
    SD[varNumNum] <- SDs
  }
  outInfo <- data.frame(Class = Class, NAs = NAs, Levels = Levels, 
                        Min.Level = Min.Level, Mean = Mean, SD = SD)
  outInfo <- outInfo[order(outInfo$NAs), ]
  names(outInfo) <- c("Class", "%.NA", "Levels", "Min.Level.Size", 
                      "Mean", "SD")
  return(outInfo)
}


# BCA::create.samples() function ----
create.samples <- function (x, est = 0.34, val = 0.33, rand.seed = 1) 
{
  if ((est + val) > 1) 
    stop("The estimation and validation samples exceed 100%.")
  if (est < 0 | val < 0) 
    stop("A negative sample size was provided.")
  nEst <- round(est * nrow(x))
  nVal <- round(val * nrow(x))
  if ((nEst + nVal) < nrow(x)) {
    assignmnts <- c(rep("Estimation", nEst), rep("Validation", 
                                                 nVal), rep("Holdout", (nrow(x) - nEst - nVal)))
  }
  else {
    assignmnts <- c(rep("Estimation", nEst), rep("Validation", 
                                                 nVal))
  }
  set.seed(rand.seed)
  randVar <- runif(nrow(x))
  assignmnts <- assignmnts[order(randVar)]
  return(assignmnts)
}


# BCA::binVariable() function ----
binVariable <- function (x, bins = 4, method = c("intervals", "proportions", 
                                  "natural"), labels = FALSE) 
{
  method <- match.arg(method)
  if (length(x) < bins) {
    stop("The number of bins exceeds the number of data values")
  }
  x <- if (method == "intervals") 
    cut(x, bins, labels = labels)
  else if (method == "proportions") 
    cut(x, quantile(x, probs = seq(0, 1, 1/bins), na.rm = TRUE), 
        include.lowest = TRUE, labels = labels)
  else {
    xx <- na.omit(x)
    breaks <- c(-Inf, tapply(xx, KMeans(xx, bins)$cluster, 
                             max))
    cut(x, breaks, labels = labels)
  }
  as.factor(x)
}


# RcmdrMisc::numSummary() function ----
numSummary <- function (data, statistics = c("mean", "sd", "se(mean)", "IQR", 
                               "quantiles", "cv", "skewness", "kurtosis"), type = c("2", 
                                                                                    "1", "3"), quantiles = c(0, 0.25, 0.5, 0.75, 1), groups) 
{
  sd <- function(x, type, ...) {
    apply(as.matrix(x), 2, stats::sd, na.rm = TRUE)
  }
  IQR <- function(x, type, ...) {
    apply(as.matrix(x), 2, stats::IQR, na.rm = TRUE)
  }
  std.err.mean <- function(x, ...) {
    x <- as.matrix(x)
    sd <- sd(x)
    n <- colSums(!is.na(x))
    sd/sqrt(n)
  }
  cv <- function(x, ...) {
    x <- as.matrix(x)
    mean <- colMeans(x, na.rm = TRUE)
    sd <- sd(x)
    if (any(x <= 0, na.rm = TRUE)) 
      warning("not all values are positive")
    cv <- sd/mean
    cv[mean <= 0] <- NA
    cv
  }
  skewness <- function(x, type, ...) {
    if (is.vector(x)) 
      return(e1071::skewness(x, type = type, na.rm = TRUE))
    apply(x, 2, skewness, type = type)
  }
  kurtosis <- function(x, type, ...) {
    if (is.vector(x)) 
      return(e1071::kurtosis(x, type = type, na.rm = TRUE))
    apply(x, 2, kurtosis, type = type)
  }
  data <- as.data.frame(data)
  if (!missing(groups)) {
    groups <- as.factor(groups)
    counts <- table(groups)
    if (any(counts == 0)) {
      levels <- levels(groups)
      warning("the following groups are empty: ", paste(levels[counts == 
                                                                 0], collapse = ", "))
      groups <- factor(groups, levels = levels[counts != 
                                                 0])
    }
  }
  variables <- names(data)
  if (missing(statistics)) 
    statistics <- c("mean", "sd", "quantiles", "IQR")
  statistics <- match.arg(statistics, c("mean", "sd", "se(mean)", 
                                        "IQR", "quantiles", "cv", "skewness", "kurtosis"), several.ok = TRUE)
  type <- match.arg(type)
  type <- as.numeric(type)
  ngroups <- if (missing(groups)) 
    1
  else length(grps <- levels(groups))
  quantiles <- if ("quantiles" %in% statistics) 
    quantiles
  else NULL
  quants <- if (length(quantiles) >= 1) 
    paste(100 * quantiles, "%", sep = "")
  else NULL
  nquants <- length(quants)
  stats <- c(c("mean", "sd", "se(mean)", "IQR", "cv", "skewness", 
               "kurtosis")[c("mean", "sd", "se(mean)", "IQR", "cv", 
                             "skewness", "kurtosis") %in% statistics], quants)
  nstats <- length(stats)
  nvars <- length(variables)
  result <- list()
  if ((ngroups == 1) && (nvars == 1) && (length(statistics) == 
                                         1)) {
    if (statistics == "quantiles") 
      table <- quantile(data[, variables], probs = quantiles, 
                        na.rm = TRUE)
    else {
      stats <- statistics
      stats[stats == "se(mean)"] <- "std.err.mean"
      table <- do.call(stats, list(x = data[, variables], 
                                   na.rm = TRUE, type = type))
      names(table) <- statistics
    }
    NAs <- sum(is.na(data[, variables]))
    n <- nrow(data) - NAs
    result$type <- 1
  }
  else if ((ngroups > 1) && (nvars == 1) && (length(statistics) == 
                                             1)) {
    if (statistics == "quantiles") {
      table <- matrix(unlist(tapply(data[, variables], 
                                    groups, quantile, probs = quantiles, na.rm = TRUE)), 
                      ngroups, nquants, byrow = TRUE)
      rownames(table) <- grps
      colnames(table) <- quants
    }
    else table <- tapply(data[, variables], groups, statistics, 
                         na.rm = TRUE, type = type)
    NAs <- tapply(data[, variables], groups, function(x) sum(is.na(x)))
    n <- table(groups) - NAs
    result$type <- 2
  }
  else if ((ngroups == 1)) {
    X <- as.matrix(data[, variables])
    table <- matrix(0, nvars, nstats)
    rownames(table) <- if (length(variables) > 1) 
      variables
    else ""
    colnames(table) <- stats
    if ("mean" %in% stats) 
      table[, "mean"] <- colMeans(X, na.rm = TRUE)
    if ("sd" %in% stats) 
      table[, "sd"] <- sd(X)
    if ("se(mean)" %in% stats) 
      table[, "se(mean)"] <- std.err.mean(X)
    if ("IQR" %in% stats) 
      table[, "IQR"] <- IQR(X)
    if ("cv" %in% stats) 
      table[, "cv"] <- cv(X)
    if ("skewness" %in% statistics) 
      table[, "skewness"] <- skewness(X, type = type)
    if ("kurtosis" %in% statistics) 
      table[, "kurtosis"] <- kurtosis(X, type = type)
    if ("quantiles" %in% statistics) {
      table[, quants] <- t(apply(data[, variables, drop = FALSE], 
                                 2, quantile, probs = quantiles, na.rm = TRUE))
    }
    NAs <- colSums(is.na(data[, variables, drop = FALSE]))
    n <- nrow(data) - NAs
    result$type <- 3
  }
  else {
    table <- array(0, c(ngroups, nstats, nvars), dimnames = list(Group = grps, 
                                                                 Statistic = stats, Variable = variables))
    NAs <- matrix(0, nvars, ngroups)
    rownames(NAs) <- variables
    colnames(NAs) <- grps
    for (variable in variables) {
      if ("mean" %in% stats) 
        table[, "mean", variable] <- tapply(data[, variable], 
                                            groups, mean, na.rm = TRUE)
      if ("sd" %in% stats) 
        table[, "sd", variable] <- tapply(data[, variable], 
                                          groups, sd, na.rm = TRUE)
      if ("se(mean)" %in% stats) 
        table[, "se(mean)", variable] <- tapply(data[, 
                                                     variable], groups, std.err.mean, na.rm = TRUE)
      if ("IQR" %in% stats) 
        table[, "IQR", variable] <- tapply(data[, variable], 
                                           groups, IQR, na.rm = TRUE)
      if ("cv" %in% stats) 
        table[, "cv", variable] <- tapply(data[, variable], 
                                          groups, cv)
      if ("skewness" %in% stats) 
        table[, "skewness", variable] <- tapply(data[, 
                                                     variable], groups, skewness, type = type)
      if ("kurtosis" %in% stats) 
        table[, "kurtosis", variable] <- tapply(data[, 
                                                     variable], groups, kurtosis, type = type)
      if ("quantiles" %in% statistics) {
        res <- matrix(unlist(tapply(data[, variable], 
                                    groups, quantile, probs = quantiles, na.rm = TRUE)), 
                      ngroups, nquants, byrow = TRUE)
        table[, quants, variable] <- res
      }
      NAs[variable, ] <- tapply(data[, variable], groups, 
                                function(x) sum(is.na(x)))
    }
    if (nstats == 1) 
      table <- table[, 1, ]
    if (nvars == 1) 
      table <- table[, , 1]
    n <- table(groups)
    n <- matrix(n, nrow = nrow(NAs), ncol = ncol(NAs), byrow = TRUE)
    n <- n - NAs
    result$type <- 4
  }
  result$table <- table
  result$statistics <- statistics
  result$n <- n
  if (any(NAs > 0)) 
    result$NAs <- NAs
  class(result) <- "numSummary"
  result$table  # [RPT] Added '$table' to output results table
}


# lift.chart() function ----



lift.chart <- function (modelList, data, targLevel, trueResp, type = "cumulative", 
          sub = "") 
{
  if (type != "cumulative" & type != "incremental") {
    stop("An improper lift chart type is specified.")
  }
  set.seed(1)
  data <- data[order(runif(nrow(data))), ]
  yvar1 <- rep(NA, length(modelList))
  modAvail <- rep(NA, length(modelList))
  probVar <- NULL
  for (i in 1:length(modelList)) {
    mod <- eval(parse(text = modelList[i]))
    modtype <- class(mod)[1]
    if (modtype != "glm" & modtype != "rpart" & modtype != 
        "nnet.formula" & modtype != "randomForest.formula") {  # [RPT] Added Rforest
      stop("Models can only be estimated using glm, rpart, or nnet.")
    }
    yvar1[i] <- as.character(mod$call$formula)[2]
    xvars <- trimws(unlist(strsplit(as.character(mod$call$formula)[3], " + ", fixed = TRUE)))
    if (!all(xvars %in% names(data))) {
      probVar <- c(probVar, xvars[!(xvars %in% names(data))])
      modAvail[i] <- FALSE
    }
    else {
      modAvail[i] <- TRUE
    }
  }
  if (any(yvar1 != yvar1[1])) {
    stop("Not all the models have the same dependent variable")
  }
  yvar2 <- data[[yvar1[1]]]
  if (!is.factor(yvar2)) {
    stop("The y variable must be a two-level factor.")
  }
  if (length(levels(yvar2)) != 2) {
    stop("The y variable must be a two-level factor.")
  }
  if (!any(as.character(yvar2) == targLevel)) {
    stop(paste("None of the levels of the response variable is \"", 
               targLevel, "\".", sep = ""))
  }
  yvar <- as.numeric(yvar2 == targLevel)
  sampResp <- sum(yvar)/length(yvar)
  print(sampResp)
  if (length(probVar) > 0) {
    probVar <- unique(probVar)
    probModel <- modelList[!modAvail]
    warnString <- paste("The models", paste(probModel, collapse = ", "), 
                        "are not in the lift chart because the variables", 
                        paste(probVar, collapse = ", "), "are not available.")
    Message(message = gettextRcmdr(warnString), type = "warning")
    modelList <- modelList[modAvail]
    if (length(modelList) == 0) {
      Message(message = gettextRcmdr(paste("All models are missing at least one of the variables: ", 
                                           paste(probVar, collapse = ", "), ".", sep = "")), 
              type = "error")
      return()
    }
  }
  sampWt <- (sampResp * (1 - trueResp))/(trueResp * (1 - sampResp))
  nmodels <- length(modelList)
  colr <- rep(palette(), ceiling(nmodels/length(palette())))
  if (type == "cumulative") {
    plot(seq(0.1, 1, 0.1), seq(0.1, 1, 0.1), main = "Weighted Cumulative Response Captured", 
         sub = sub, xlab = "Sample Proportion", ylab = "Percent of Total Response Captured", 
         type = "l", lwd = 2, xaxs = "i", yaxs = "i")
    for (i in 1:nmodels) {
      model1 <- eval(parse(text = modelList[i]))
      modtype <- class(model1)[1]
      model <- eval(model1$call)
      if (modtype == "glm" | modtype == "nnet.formula") {
        if (levels(yvar2)[1] == targLevel) {
          var1 <- yvar[order(predict(model, newdata = data), 
                             decreasing = FALSE)]
        }
        else {
          var1 <- yvar[order(predict(model, newdata = data), 
                             decreasing = TRUE)]
        }
      }
      else {
        var1 <- yvar[order(as.vector(predict(model, 
                                             newdata = data, type = "prob")[, targLevel]), decreasing = TRUE)]  # [RPT] Added ', type = "prob"' to avoid error with RF
      }
      var.ind1 <- rep(1, length(var1))
      var.ind1[var1 == 0] <- sampWt
      var.ind <- cut(cumsum(var.ind1)/sum(var.ind1), seq(0, 
                                                         1, 0.1), include.lowest = TRUE)
      var2 <- as.vector(by(var1, var.ind, sum))
      lines(seq(0.1, 1, 0.1), cumsum(var2)/sum(var2), 
            col = colr[i], lwd = 2)
      points(seq(0.1, 1, 0.1), cumsum(var2)/sum(var2), 
             col = colr[i], pch = i)
    }
    legend("bottomright", legend = modelList, col = colr[1:nmodels], 
           pch = 1:length(modelList), lty = 1, lwd = 2)
  }
  else {
    resp.matrix <- matrix(NA, nrow = 10, ncol = length(modelList))
    for (i in 1:nmodels) {
      model1 <- eval(parse(text = modelList[i]))
      modtype <- class(model1)[1]
      model <- eval(model1$call)
      if (modtype == "glm" | modtype == "nnet.formula") {
        if (levels(yvar2)[1] == targLevel) {
          var1 <- yvar[order(predict(model, newdata = data), 
                             decreasing = FALSE)]
        }
        else {
          var1 <- yvar[order(predict(model, newdata = data), 
                             decreasing = TRUE)]
        }
      }
      else {
        var1 <- yvar[order(as.vector(predict(model, 
                                             newdata = data)[, targLevel]), decreasing = TRUE)]
      }
      var.ind1 <- rep(1, length(var1))
      var.ind1[var1 == 0] <- sampWt
      var.ind <- cut(cumsum(var.ind1)/sum(var.ind1), seq(0, 
                                                         1, 0.1), include.lowest = TRUE)
      var2 <- as.vector(by(var1, var.ind, sum))
      var3 <- as.vector(by(var.ind1, var.ind, sum))
      resp.matrix[, i] <- var2/var3
    }
    max.resp <- max(resp.matrix)
    plot(seq(0.1, 1, 0.1), seq(0, max.resp, length = 10), 
         type = "n", main = "Weighted Incremental Response Rate", 
         sub = sub, xlab = "Sample Percentile", ylab = "Resposne Rate", 
         lwd = 2, xaxs = "i", yaxs = "i")
    lines(seq(0.1, 1, 0.1), rep(trueResp, 10), lwd = 2)
    for (j in 1:nmodels) {
      lines(seq(0.1, 1, 0.1), as.vector(resp.matrix[, 
                                                    j]), col = colr[j], lwd = 2)
      points(seq(0.1, 1, 0.1), as.vector(resp.matrix[, 
                                                     j]), col = colr[j], pch = j)
    }
    print(length(modelList))
    legend("topright", legend = modelList, col = colr[1:nmodels], 
           pch = 1:nmodels, lty = 1, lwd = 2)
  }
  invisible()
}


# RcmdrPlugin.BCA::Nnet() function ----
# Note: depends on nnsub(), trim.blanks functions (see below)
Nnet <- function (formula, data, decay, size, subset = "") 
{
  set.seed(1)
  if (subset == "") {
    nnetObj <- nnet(formula = formula, data = data, decay = decay, 
                    size = size, maxit = 400)
    for (i in 2:10) {
      set.seed(i)
      newNnet <- nnet(formula = formula, data = data, 
                      decay = decay, size = size, maxit = 400)
      if (newNnet$value < nnetObj$value) 
        nnetObj <- newNnet
    }
  }
  else {
    selectRow <- nnSub(data, subset)
    nData <- data[selectRow, ]
    nnetObj <- nnet(formula = formula, data = nData, decay = decay, 
                    size = size, maxit = 400)
    for (i in 2:10) {
      set.seed(i)
      newNnet <- nnet(formula = formula, data = nData, 
                      decay = decay, size = size, maxit = 400)
      if (newNnet$value < nnetObj$value) 
        nnetObj <- newNnet
    }
  }
  nnetObj$call <- call("Nnet", formula = formula, data = substitute(data), 
                       decay = decay, size = size, subset = subset)
  return(nnetObj)
}

# RcmdrPlugin.BCA::nnSub()
nnSub <- function (data, subset) 
{
  rowInd <- 1:nrow(data)
  if (length(unlist(strsplit(subset, "=="))) == 2) {
    compType <- "Equal"
    subSplit <- unlist(strsplit(subset, "=="))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else if (length(unlist(strsplit(subset, "!="))) == 2) {
    compType <- "NotEqual"
    subSplit <- unlist(strsplit(subset, "!="))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else if (length(unlist(strsplit(subset, ">="))) == 2) {
    compType <- "GrtrEqual"
    subSplit <- unlist(strsplit(subset, ">="))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else if (length(unlist(strsplit(subset, "<="))) == 2) {
    compType <- "LessEqual"
    subSplit <- unlist(strsplit(subset, "<="))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else if (length(unlist(strsplit(subset, ">"))) == 2) {
    compType <- "Grtr"
    subSplit <- unlist(strsplit(subset, ">"))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else if (length(unlist(strsplit(subset, "<"))) == 2) {
    compType <- "Less"
    subSplit <- unlist(strsplit(subset, "<"))
    subVar <- trim.blanks(subSplit[1])
    subVal <- eval(parse(text = trim.blanks(subSplit[2])))
  }
  else stop("No appropriate comparison operator was found.")
  subSet <- data[[subVar]]
  if (compType == "Equal") 
    rowInd <- rowInd[subSet == subVal]
  else if (compType == "NotEqual") 
    rowInd <- rowInd[subSet != subVal]
  else if (compType == "GrtrEqual") 
    rowInd <- rowInd[subSet >= subVal]
  else if (compType == "LessEqual") 
    rowInd <- rowInd[subSet <= subVal]
  else if (compType == "Grtr") 
    rowInd <- rowInd[subSet > subVal]
  else rowInd <- rowInd[subSet < subVal]
  rowInd <- rowInd[!is.na(rowInd)]
  return(rowInd)
}

# Rcmdr::trim.blanks()
trim.blanks <- function (text) 
{
  gsub("^ *", "", gsub(" *$", "", text))
}



# BCA::rankScore() function ----
rankScore <- function (model, data, targLevel) 
{
  mod <- eval(parse(text = model))
  modtype <- class(mod)[1]
  if (modtype != "glm" & modtype != "rpart" 
      & modtype != "nnet.formula" & modtype != "randomForest.formula") {  # [REK] add RF
    stop("Scoring can only be done for models estimated using glm, rpart, or nnet.")
  }
  yvar <- as.character(mod$call$formula)[2]
  #origYs <- eval(parse(text = paste("unique(", ActiveDataSet(),  # [RPT] ActiveDataSet() returns name of dataset active 
  #                                  "$", yvar, ")")))
  origYs <- eval(parse(text = paste("unique(", deparse(substitute(data)),  # [RPT] Using deparse(substitute(data)) instead of ActiveDataSet()
                                    "$", yvar, ")")))
  origYs <- as.character(origYs)
  origYs <- origYs[order(origYs)]
  xvars <- unlist(strsplit(as.character(mod$call$formula)[3], 
                           " + ", fixed = TRUE))
  if (!all(xvars %in% names(data))) {
    probVar <- c(xvars[!(xvars %in% names(data))])
    stop(paste("The model variables", paste(probVar, collapse = ", "), 
               "are not in the data set."))
  }
  modelReDo <- eval(mod$call)
  if (modtype == "glm" | modtype == "nnet.formula") {
    if (origYs[1] == targLevel) {
      scoreVar1 <- -1 * predict(modelReDo, newdata = data)
    }
    else {
      scoreVar1 <- predict(modelReDo, newdata = data)
    }
  }
  else {
    scoreVar1 <- predict(modelReDo, newdata = data, type = "prob")[, targLevel] #[REK] add type="prob" 
  }
  score.df <- data.frame(scoreVar = 1:nrow(data), oldOrd = order(scoreVar1, 
                                                                 decreasing = TRUE))
  score.df <- score.df[order(score.df$oldOrd), ]
  scoreVar <- score.df$scoreVar
  return(scoreVar)
}

# BCA::rawProbScore() function ----
rawProbScore <- function (model, data, targLevel) 
{
  mod <- eval(parse(text = model))
  modtype <- class(mod)[1]
  print(modtype)
  if (modtype != "glm" & modtype != "rpart" 
      & modtype != "nnet.formula" & modtype != "randomForest.formula") {   # [REK] add RF
    stop("Scoring can only be done for models estimated using glm, rpart, or nnet.")
  }
  yvar <- as.character(mod$call$formula)[2]
  origYs <- eval(parse(text = paste("unique(", deparse(substitute(data)),  # [RPT] Using deparse(substitute(data)) instead of ActiveDataSet()
                                    "$", yvar, ")")))
  origYs <- as.character(origYs)
  origYs <- origYs[order(origYs)]
  xvars <- unlist(strsplit(as.character(mod$call$formula)[3], 
                           " + ", fixed = TRUE))
  if (!all(xvars %in% names(data))) {
    probVar <- c(xvars[!(xvars %in% names(data))])
    stop(paste("The model variables", paste(probVar, collapse = ", "), 
               "are not in the data set."))
  }
  modelReDo <- eval(mod$call)
  if (modtype == "glm" | modtype == "nnet.formula") {
    if (origYs[1] == targLevel) {
      scoreVar1 <- -1 * predict(modelReDo, newdata = data)
    }
    else {
      scoreVar1 <- predict(modelReDo, newdata = data)
    }
  }
  else {
    scoreVar1 <- predict(modelReDo, newdata = data, type="prob")[, targLevel] #[REK] add type="prob" to avoid error from RF
  }
  if (modtype == "glm") {
    scoreVar <- exp(scoreVar1)/(exp(scoreVar1) + 1)
  }
  else {
    scoreVar <- scoreVar1
  }
  return(scoreVar)
}


# BCA::adjProbScore() function ----
adjProbScore <- function (model, data, targLevel, trueResp) 
{
  mod <- eval(parse(text = model))
  modtype <- class(mod)[1]
  if (modtype != "glm" & modtype != "rpart" 
      & modtype != "nnet.formula" & modtype != "randomForest.formula") {   # [REK] add RF) 
    stop("Scoring can only be done for models estimated using glm, rpart, or nnet.")
  }
  yvar <- as.character(mod$call$formula)[2]
  yvar1 <- eval(parse(text = paste("as.character(", deparse(substitute(data)),  # [RPT] Using deparse(substitute(data)) instead of ActiveDataSet()
                                   "$", yvar, ")")))
  yvar2 <- as.numeric(yvar1 == targLevel)
  #print(yvar1)
  sampResp <- sum(yvar2)/length(yvar2)
  adjWt <- trueResp/sampResp
  origYs <- eval(parse(text = paste("unique(", deparse(substitute(data)), # [RPT] Using deparse(substitute(data)) instead of ActiveDataSet()
                                    "$", yvar, ")")))
  origYs <- as.character(origYs)
  origYs <- origYs[order(origYs)]
  xvars <- unlist(strsplit(as.character(mod$call$formula)[3], 
                           " + ", fixed = TRUE))
  if (!all(xvars %in% names(data))) {
    probVar <- c(xvars[!(xvars %in% names(data))])
    stop(paste("The model variables", paste(probVar, collapse = ", "), 
               "are not in the data set."))
  }
  modelReDo <- eval(mod$call)
  if (modtype == "glm" | modtype == "nnet.formula") {
    if (origYs[1] == targLevel) {
      scoreVar1 <- -1 * predict(modelReDo, newdata = data)
    }
    else {
      scoreVar1 <- predict(modelReDo, newdata = data)
    }
  }
  else {
    scoreVar1 <- predict(modelReDo, newdata = data, type = "prob")[, targLevel] #[REK] added type= "prob" to avoid RF error
  }
  if (modtype == "glm") {
    glmprob <- (exp(scoreVar1)/(exp(scoreVar1) + 1))             #[REK]  new variable glmprob
    scoreVar <- (adjWt * glmprob)/
      ((adjWt * glmprob) + (1-glmprob) * ((1-trueResp)/(1-sampResp)))   #[REK] correct correction
   # scoreVar <- adjWt * (exp(scoreVar1)/(exp(scoreVar1) + 
     #                                      1))               #[REK] old incorrect correction
  }
  else {
    scoreVar <- (adjWt * scoreVar1)/ 
      ((adjWt * scoreVar1) + (1-scoreVar1) * ((1-trueResp)/(1-sampResp)))      #[REK] correct correction                      
    
#    scoreVar <- adjWt * scoreVar1                            #[REK] old incorrect correction
  }
  return(scoreVar)
}


# BCA::bootCVD() function ----
# depends on bootCH() (see below)
bootCVD <- function (x, k, nboot = 100, nrep = 1, method = c("kmn", "kmd", 
                                                  "neuralgas"), col1, col2, dsname) 
{
  print(class(x))
  method = match.arg(method)
  if (method == "kmn") {
    bfc <- bootFlexclust(x = x, k = k, nboot = nboot, nrep = nrep, 
                         FUN = cclust, dist = "euclidean", method = "kmeans")
    the_method <- "K-Means"
  }
  else if (method == "kmd") {
    bfc <- bootFlexclust(x = x, k = k, nboot = nboot, nrep = nrep, 
                         FUN = kcca, family = kccaFamily("kmedians"))
    the_method <- "K-Medians"
  }
  else {
    bfc <- bootFlexclust(x = x, k = k, nboot = nboot, nrep = nrep, 
                         FUN = cclust, dist = "euclidean", method = "neuralgas")
    the_method <- "Neural Gas"
  }
  cat("\nSummary of Rand Indices:\n")
  print(summary(bfc@rand))
  ch <- bootCH(x, k, bfc@cluster1, bfc@cluster2, bfc@centers1, 
               bfc@centers2, method)
  cat("\nSummary of Calinski-Harabas Indices:\n")
  print(summary(ch))
  omfrow <- par()$mfrow
  par(mfrow = c(2, 1))
  boxplot(bfc@rand, main = paste("Adj Rand Index for", dsname, 
                                 "using", the_method), xlab = "Number of Clusters", ylab = "Adjusted Rand", 
          col = col1)
  boxplot(ch, main = paste("C-H Index for", dsname, "using", 
                           the_method), xlab = "Number of Clusters", ylab = "Calinski-Harabas", 
          col = col2)
  par(mfrow = omfrow)
}


# BCA::bootCH() function ---
bootCH <- function (xdat, k_vals, clstr1, clstr2, cntrs1, cntrs2, method = c("kmn", 
                                                                   "kmd", "neuralgas")) 
{
  method = match.arg(method)
  if (method == "kmd") 
    all_centers <- apply(xdat, 2, median)
  else all_centers <- apply(xdat, 2, mean)
  all_dif <- sweep(xdat, 2, all_centers, "-")
  tss <- sum(all_dif^2)
  n_solu <- dim(clstr1)[2]
  nboot <- dim(clstr1)[3]
  n_obs <- dim(clstr1)[1]
  ch_mat <- matrix(NA, nrow = 2 * nboot, ncol = n_solu)
  for (k_ind in 1:n_solu) {
    cent_array1 <- cntrs1[[k_ind]]
    cent_array2 <- cntrs2[[k_ind]]
    cls_asgn_m1 <- clstr1[, k_ind, ]
    cls_asgn_m2 <- clstr2[, k_ind, ]
    k <- k_vals[k_ind]
    ch_reps1 <- rep(NA, nboot)
    ch_reps2 <- rep(NA, nboot)
    for (b_ind in 1:nboot) {
      clus1 <- cls_asgn_m1[, b_ind]
      clus2 <- cls_asgn_m2[, b_ind]
      centrds1 <- cent_array1[, , b_ind]
      centrds2 <- cent_array2[, , b_ind]
      wss1a <- (xdat - centrds1[clus1, ])^2
      wss2a <- (xdat - centrds2[clus2, ])^2
      wss1 <- 0
      wss2 <- 0
      for (m in 1:k) {
        wss1 <- wss1 + sum(wss1a[clus1 == m, ])
        wss2 <- wss2 + sum(wss2a[clus2 == m, ])
      }
      bss1 <- tss - wss1
      bss2 <- tss - wss2
      ch_reps1[b_ind] <- ((n_obs - k)/(k - 1)) * (bss1/wss1)
      ch_reps2[b_ind] <- ((n_obs - k)/(k - 1)) * (bss2/wss2)
    }
    ch_mat[, k_ind] <- c(ch_reps1, ch_reps2)
    dimnames(ch_mat)[[2]] <- as.character(k_vals)
  }
  return(ch_mat)
}
