### make data

nTrials = 12
nSubs = 30

fake <- expand.grid(trial = 1:nTrials, ppt = 1:nSubs, A = c('A1', 'A2'), B = c('B1', 'B2'), stringsAsFactors = T)

B0 <- 1.5 # grand mean
b1 <- .5 # main effect of A
b2 <- -.3 # main effect of B
s <- rnorm(nSubs, 0, 1)

# there are two true main effects and no interaction
logitSuccess <- with(fake, B0 + c(b1, -b1)[A] + c(b2, -b2)[B] + s[ppt])

logistic <- function(x){
  1/(1 + exp(-x))
}

fake$y <- rbinom(n = length(logitSuccess), size = 1, prob = logistic(logitSuccess))

write.csv(fake, 'fakeData.csv')

with(aggregate(y ~ A + B, data = fake, FUN = mean), barplot(y, names.arg = paste(A,B, sep = '-')))

