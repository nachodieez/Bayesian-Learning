library(tidyverse)
library(glmnet)
library(MASS)
library(ggthemes)
library(scales)

require("testit") || !is.null(install.packages("testit")) || require("testit")

rmse_2<-function(y_pred,y){
  (y_pred-y)%>%(\(x)x*x)()|>sum()%>%sqrt()|>(\(x)x/length(y))()%>%
  return 
}

rmse <- function(y_pred, y){ #not designed for readability
  assert("Different lengths for inputs!",
         length(y_pred) == length(y))
  (y_pred - y)             %>%
    (\(x) x / length(x))() |> 
    #· We are testing the limits of R ·#
    (\(x) x * x)()         |>
    sum()                  %>%  
    sqrt()                 
} 

set.seed(1)

data <- read.csv("bayesian_regression_data.csv", sep = ";")

p <- 0.8
n <- nrow(data)
train_idx <- sample(1:n, round(n*p), replace = F)

train <- data[train_idx,]
test <- data[-train_idx,]

fit_full_frequentist <- lm(SalePrice ~ ., data = train)
summary(fit_full_frequentist)
pred_full <- predict(fit_full_frequentist, newdata = test[,-16])

#parece que hay cosas no significativas! vamos a hacer laso

sc_data <- scale(train)
x <- sc_data[,-16]
y <- sc_data[,16]

optimal_lambda <- 0.0

for (i in 1:10) {
  cv_lasso <- cv.glmnet(x, y,
                        alpha = 1, nfolds = 10)
  optimal_lambda <- optimal_lambda + cv_lasso$lambda.1se
}

optimal_lambda <- optimal_lambda / 10

mod_lasso <- glmnet(x, y, alpha = 1, 
                    lambda = optimal_lambda)

idx <- which(mod_lasso$beta != 0)
selected_vars <- rownames(mod_lasso$beta)[idx]

lhs_formula_fit <- "SalePrice ~ "
rhs_formula_fit <- paste0(selected_vars, collapse = " + ")
formula_fit <- as.formula(paste0(lhs_formula_fit, rhs_formula_fit))

fit_frequentist_lasso <- lm(formula_fit, data = train)
names(fit_frequentist_lasso$coefficients)
summary(fit_frequentist_lasso)
pred_lasso <- predict(fit_frequentist_lasso, newdata = test[,-16])

fit_frequentist_step <- stepAIC(fit_full_frequentist,
                                k = log(n))
names(fit_frequentist_step$coefficients)
summary(fit_frequentist_step)
pred_step <- predict(fit_frequentist_step, newdata = test[,-16])

#terrible

y_test <- test$SalePrice
rmse(pred_full, y_test)
rmse(pred_lasso, y_test)
rmse(pred_step, y_test)

ggplot(aes(x = SalePrice), data = train) + 
  geom_histogram(col = "white", fill = "deeppink4", bins = 30) +
  xlab("Count") + ylab("Sale Price") + 
  ggtitle("Histogram of sale price") + 
  scale_x_continuous(labels = function(x) 
    format(x, scientific = F)) + 
  theme_classic()

#this doesn't look normal!!!!




