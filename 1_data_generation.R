train_dataset <- read_excel('training_dataset.xlsx')
# We used the Heart Disease dataset 
#[[Detrano R, Janosi A, Steinbrunn W, Pfisterer M, Schmid J-J, Sandhu S, et al. International application of a new probability algorithm for the diagnosis of coronary artery disease. The American Journal of Cardiology 1989;64:304–10. https://doi.org/10.1016/0002-9149(89)90524-9.]]
#[[Janosi A, Steinbrunn W, Pfisterer M, Detrano R. Heart Disease 1988.]]
#from the Machine Learning Repository 
#[[Aha D. UCI Machine Learning Repository: Heart Disease Data Set n.d. https://archive.ics.uci.edu/ml/datasets/heart+disease (accessed July 24, 2022).]]
#and generated random variables from various probability distributions. 
#The predicted variable is coronary artery disease (>50% lumen stenosis in any major artery). 
#The analysis included 331 cases from the training set, of which 174 were classified as healthy, i.e., without coronary artery disease (the non-event class labeled as 0), and 157 as diseased (the event class labeled as 1).

n <- nrow(train_dataset)
n0 <- sum(train_dataset$disease == 0)
n1 <- sum(train_dataset$disease == 1)

# generate independent vars
set.seed(101801)

rnd_normal   <- rnorm(n)
rnd_uniform  <- runif(n, 0, 10)
rnd_exp      <- rexp(n, 1)
rnd_bernoulli <- rbinom(n, 1, 0.8)
rnd_binomial <- rbinom(n, 6, 0.8)
rnd_poisson  <- rpois(n, 1)
strat_rnd_normal   <- ifelse(train_dataset$disease == 0, rnorm(n0, 10, 2), rnorm(n1, 12, 2))
strat_rnd_uniform  <- ifelse(train_dataset$disease == 0, runif(n0, 0, 6), runif(n1, 2, 8))
strat_rnd_exp      <- ifelse(train_dataset$disease == 0, rexp(n0, 0.5), rexp(n1, 1))
strat_rnd_bernoulli <- ifelse(train_dataset$disease == 0, rbinom(n0, 1, 0.5), rbinom(n1, 1, 0.2))
strat_rnd_binomial <- ifelse(train_dataset$disease == 0, rbinom(n0, 7, 0.6), rbinom(n1, 7, 0.5))
strat_rnd_poisson  <- ifelse(train_dataset$disease == 0, rpois(n0, 1), rpois(n1, 1.6))
N01_N01  <- ifelse(train_dataset$disease == 0, rnorm(n0, 0, 1), rnorm(n1, 0, 1))
N01_N12  <- ifelse(train_dataset$disease == 0, rnorm(n0, 0, 1), rnorm(n1, 1, 2))
N12_N01  <- ifelse(train_dataset$disease == 0, rnorm(n0, 1, 2), rnorm(n1, 0, 1))
N01_N14  <- ifelse(train_dataset$disease == 0, rnorm(n0, 0, 1), rnorm(n1, 1, 4))
N14_N01  <- ifelse(train_dataset$disease == 0, rnorm(n0, 1, 4), rnorm(n1, 0, 1))

train_dataset_rnd <- cbind(train_dataset,
                           rnd_normal,
                           rnd_uniform,
                           rnd_exp,
                           rnd_bernoulli,
                           rnd_binomial,
                           rnd_poisson,
                           strat_rnd_normal,
                           strat_rnd_uniform,
                           strat_rnd_exp,
                           strat_rnd_bernoulli,
                           strat_rnd_binomial,
                           strat_rnd_poisson,
                           N01_N01,
                           N01_N12,
                           N12_N01,
                           N01_N14,
                           N14_N01)
write_xlsx(train_dataset_rnd, 'train_dataset_final.xlsx')
