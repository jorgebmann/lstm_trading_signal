library(IBrokers)
library(quantmod) 
library(rlist)
library(dplyr)
library(kerasR)
library(reticulate)
library(caret)
library(PerformanceAnalytics)


# for reproducible results:
set.seed(123)

euro50 <- read.csv('euro50_ticker.csv', header = T, stringsAsFactors = F, col.names = c('ticker'))

tws <- twsConnect(port = 7497) #port: paper trading: 7497; real trading: 7496

S <- list()
for (i in 1:length(euro50$ticker)) {
  S[[i]] <- try(reqHistoricalData(tws, Contract = twsEquity(euro50$ticker[i], exch = 'SMART', currency = 'EUR'),
                                  barSize = '1 day', duration = '5 Y', 
                                  endDateTime = ''))}

names(S) <- euro50$ticker
# Remove securities with no values:
S_filtered <- Filter(Negate(is.null), S)
S_filtered <- lapply(S_filtered, function(x) x[, c(1:5, 8)])
S_filtered <- lapply(S_filtered, setNames, nm <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Count'))
# check number of timesteps for each security:
l <- sapply(S_filtered, function(x) length(index(x)))
hist(l)
# Remove securities with less than 1000 timesteps:
S_filtered <- lapply(S_filtered, function(x) if (length(index(x)) > 1000) {x = x} else {x = NULL})
S_filtered <- Filter(Negate(is.null), S_filtered)
#list.save(S_filtered, file = 'S_euro50_2020-06-10.rds')

#S <- list.load(file = 'S_euro50_2020-06-10.rds')
# Calculate daily (log) returns for each security:
R <- lapply(S, function(x) x$return = Delt(x$Close, k = 1, type = c('log')))
head(R[[2]])
for (i in 1:length(R)) {
  colnames(R[[i]]) <- names(S)[i]
}
returns <- do.call(cbind, R)
returns_na <- sapply(returns, function(x) sum(is.na(x)))
# Filter out securities with more than 5% NA
cutoff_na <- round(dim(returns)[1] * 0.05)
returns_filtered <- subset(returns, select = c(returns_na < cutoff_na))
dim(returns_filtered)
# We still have some NAs included, lets interpolate their values based on surrounding values:
returns_filtered <- na.approx(returns_filtered)
dim(returns_filtered)
returns_filtered <- returns_filtered[-1,]
#write.csv(returns_filtered, file = 'euro50_return_target.csv')
#returns_filtered <- read.csv('euro50_return_target.csv', stringsAsFactors = F)
returns_filtered <- returns_filtered[,-1]
returns_filtered <- data.frame(returns_filtered)

ncol(returns_filtered)

# set first stock in universe as Target and use shifted time series:
data <- cbind(Target = lead(returns_filtered$ABI, n=1), returns_filtered)
data <- na.omit(data)

# data split of time series: first test set, next validation set, and finally test set (out-of-sample test)
train_split <- round(0.8*length(index(data)))
valid_split <- round(0.1*length(index(data))) + train_split
test_split <- round(0.1*length(index(data))) + valid_split

train <- data[1:train_split-1,]
valid <- data[train_split:valid_split-1,]
test <- data[valid_split:length(index(data)),]

# split into input and outputs and convert to matrix:
train_X <- as.matrix(subset(train, select = -c(Target)))
train_y <- as.matrix(train$Target)
valid_X <- as.matrix(subset(valid, select = -c(Target)))
valid_y <- as.matrix(valid$Target)
test_X <- as.matrix(subset(test, select = -c(Target)))
test_y <- as.matrix(test$Target)

# reshape input to be 3D [samples, timesteps, features]
dim(train_X) <- c(dim(train_X)[1], 1, dim(train_X)[2])
dim(valid_X) <- c(dim(valid_X)[1], 1, dim(valid_X)[2])
dim(test_X) <- c(dim(test_X)[1], 1, dim(test_X)[2])
print(dim(train_X))


# design LSTM neural network:
model = Sequential()
model$add(LSTM(128, input_shape=c(dim(train_X)[2], dim(train_X)[3]), activation = 'relu'))
model$add(Dense(64, activation = 'relu'))
model$add(Dense(32, activation = 'relu'))
model$add(Dense(1, activation = 'linear'))
model$compile(loss='mae', optimizer='adam')
keras_fit(model, train_X, train_y,
          batch_size = 512, epochs = 50, validation_data = list(valid_X, valid_y),
          verbose = 1, shuffle = F)

# predict next days returns:
yhat <- keras_predict(model, test_X)
# concatenate real (reference) and predicted returns:
res <- data.frame(test_y, yhat)
# set TRUE/FALSE values of real and predicted returns:
res$Reference <- as.factor(test_y > 0)
res$Prediction <- as.factor(yhat > 0)
res <- na.omit(res)

# generate a confusion matrix based on TRUE/FALSE values (we should get an accuracy of ca. 58%)
cm <- confusionMatrix(res$Prediction, res$Reference)

# calculated returns as if we would trade our signal:
for (k in 1:length(res$test_y)) {
  # if reference and prediction are positive we win:
  if (res$Reference[k] == T && res$Prediction[k] == T) {
    res$ret[k] = res$test_y[k]
    # if reference is positive but prediction is negative we lose:
    } else if (res$Reference[k] == T && res$Prediction[k] == F) { 
      res$ret[k] = -res$test_y[k] 
      # if reference is negative but prediction is positive we lose:
      } else if (res$Reference[k] == F && res$Prediction[k] == T) {
        res$ret[k] = res$test_y[k] 
        # if reference as prediction are negative we win:
        } else if (res$Reference[k] == F && res$Prediction[k] == F) {
          res$ret[k] = abs(res$test_y[k])
        }
}

# convert simulated returns into time sequences:
ret <- as.ts(res$ret)
# calculate sharpe ratio:
SharpeRatio(ret)
