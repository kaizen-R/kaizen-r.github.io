## Trying to do simple ML, in this case: Classification
## Use case today? URL Classification, binary.

library(ggplot2)
library(gridExtra)
library(stringr)
library(rpart)
library(randomForest)
library(nnet)
library(NeuralNetTools)
library(neuralnet)
library(caret) # Oldy... But good, for RMSE and one-hot-encoding
library(kernlab) # for SVM

## A great repo for CYS data: secrepo.com!
urls_df <- read.csv("https://raw.githubusercontent.com/faizann24/Using-machine-learning-to-detect-malicious-URLs/refs/heads/master/data/data.csv")

# urls_df <- urls_df[,1:2]
urls_df$url <- tolower(urls_df$url)
## Let's have a quick look, a.k.a. "EDA"
dim(urls_df)
table(urls_df$label) ## Imbalance, we might want e.g. to under-sample "good"
urls_df$label_num <- ifelse(urls_df$label == "good", 0, 1)
urls_df$url_length <- nchar(urls_df$url)

## A bit of domain knowledge helps:
nrow(urls_df[urls_df$url_length > 300,]) / nrow(urls_df)
## Let's get rid of this .1%, at least for today's exercise.
urls_df <- urls_df |> dplyr::filter(url_length <= 300)

## Looking at the data is always a good idea...
g_lengths <- ggplot(data = urls_df,
                    aes(x = url_length, colour = label)) +
    geom_density() +
    theme_bw() +
    labs(title=paste("URL lengths by class"))
g_lengths


## Let's do some "Feature Engineering":
urls_df$slashes_count <- stringr::str_count(urls_df$url, "/")
urls_df$dots_count <- stringr::str_count(urls_df$url, "\\.")

## Maybe, you know, IP as a hostname is a bad thing?
urls_df$host_is_ip <- grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}", urls_df$url)

## Bad URLs might make less of an effort to be readable:
urls_df$vowels_prev <- sapply(urls_df$url, function(t_str) {
    t_str2 <- strsplit(tolower(t_str), "")[[1]]
    paste(t_str2[t_str2 %in% c("a", "e", "i", "o", "u")], sep = "", collapse="")
}) |> nchar()
urls_df$vowels_prev <- urls_df$vowels_prev / urls_df$url_length

## I don't know, we'll see about this one:
urls_df$ends_in_slash <- ifelse(str_ends(urls_df$url, "/"), TRUE, FALSE)

## Is there a Port indicated? Y/N
urls_df$contains_port <- sapply(urls_df$url, function(t_str) {
    if(grepl("/", t_str)) {
        grepl(":[0-9]{1,5}$", unlist(strsplit(t_str, split = "/"))[1])
    }
    grepl(":[0-9]{1,5}$", t_str)
})
# urls_df[urls_df$contains_port == TRUE,]

## Is there a GET Parameter, and if so how many?
#urls_df$contains_parameters <- grepl("\\?", urls_df$url)
urls_df$n_params <- sapply(urls_df$url, function(t_str) {
    if(grepl("\\?", t_str)) {
        t_str2 <- str_split(t_str, pattern = "\\?", 2)
        return(str_count(t_str2, "\\="))
    }
    
    0
})
#head(urls_df[urls_df$n_params > 0,], 20)

## Gibberish? i.e. Mixes letters and numbers a lot?

## These next ideas might generate quite a few columns and so I'll table first, 
## and keep top X per group, and mark the rest as "others" to keep
## reasonably low dimensionality

## Domain Extension might help (although, I don't know)
## One hot encoding?
urls_df$domain_ext <- sapply(urls_df$url, function(t_str) {
    if(grepl("\\?", t_str)) {
        t_str <- str_split_i(t_str, pattern = "\\?", 1)
    }
    if(grepl("/", t_str)) {
        t_str <- str_split_i(t_str, pattern = "/", 1)
    }
    if(grepl(":", t_str)) {
        t_str <- str_split_i(t_str, pattern = ":", 1)
    }
    
    str_extract(t_str, "(.*\\.)([a-z0-9]+)", group = 2)
})

urls_df$file_ext <- sapply(urls_df$url, function(t_str) {
    if(grepl("\\?", t_str)) {
        t_str <- str_split_i(t_str, pattern = "\\?", 1)
    }
    if(grepl("/", t_str)) {
        t_str <- str_split_i(t_str, pattern = "/", -1)
        if(grepl("\\.", t_str)) {
            return(str_extract(t_str, "(.*\\.)([a-z0-9]+)", group = 2))
        }
    }
    
    ""
})

## File Extension, if any, is a plausible thing to look at, too.
urls_df$is_common_domain_ext <- urls_df$domain_ext %in% head(names(sort(table(urls_df$domain_ext), decreasing = TRUE)), 100)
urls_df$is_uncommon_domain_ext <- urls_df$domain_ext %in% tail(names(sort(table(urls_df$domain_ext), decreasing = TRUE)), 100)
## The next one is probably useless as signal, as only very few entries will be
## concerned...
urls_df$is_uncommon_file_ext <- !(urls_df$file_ext %in% head(names(sort(table(urls_df$file_ext), decreasing = TRUE)), 50))

## Excluding extension... PENDING!!
urls_df$has_4_consonants_straight <- grepl("[b-df-hj-np-tv-xz]{4,}", urls_df$url)
#urls_df$found_4_consonants <- str_extract(urls_df$url, "[b-df-hj-np-tv-xz]{4,}")
urls_df[sample(1:nrow(urls_df), 20),]

# # For visualization, just a helper.
# urls_df$slashes_count_fuzzy <- urls_df$slashes_count + rnorm(nrow(urls_df), sd = 0.2)
# urls_df$dots_count_fuzzy <- urls_df$dots_count + rnorm(nrow(urls_df), sd = 0.2)

good_urls <- urls_df[urls_df$label == "good",]
bad_urls <- urls_df[urls_df$label == "bad",]
## Undersampling "good" vs "bad"
## At this point, after all what comes next, I iterate with MUCH MORE data, 10x
sample_urls_df <- rbind(bad_urls[sample(1:nrow(bad_urls), size = 10000,replace = FALSE),],
                        good_urls[sample(1:nrow(good_urls), size = 10000,replace = FALSE),])


# ## Now on to some visual exploration of the data:
# 
# ## Let's get a sense of some things:
# grid.arrange(ggplot(data = sample_urls_df,
#                     aes(x = slashes_count_fuzzy, y = dots_count_fuzzy, colour = label)) +
#                  geom_point(aes(alpha = 0.1)),
#              ggplot(data = sample_urls_df,
#                     aes(x = url_length, y = slashes_count_fuzzy, colour = label)) +
#                  geom_point(aes(alpha = 0.1)),
#              ggplot(data = sample_urls_df,
#                     aes(x = url_length, y = dots_count_fuzzy, colour = label)) +
#                  geom_point(aes(alpha = 0.1)),
#              ncol=3)

## Trick, bad in concept, but...
sample_urls_df[, 12:13] <- lapply(sample_urls_df[, 12:13], as.factor)
## To avoid certain variables overshadowing others...
sample_urls_df[, 4:17] <- lapply(sample_urls_df[, 4:17], as.numeric)
sample_urls_df[, 4:17] <- lapply(sample_urls_df[, 4:17], scale)
## Don't do this either. In this case, one of my above transformations was
## not good, the data itself has no "NAs", so... My bad. Quick fix:
sample_urls_df[is.na(sample_urls_df)] <- 0

## Model_dataset
separate_sets <- sample(c(TRUE, FALSE), nrow(sample_urls_df), replace=TRUE, prob=c(0.7,0.3))
t_train <- sample_urls_df[separate_sets, ]
t_test <- sample_urls_df[!separate_sets, ]

visu_sample <- sample(1:nrow(t_train), 200, replace=TRUE)
pairs(t_train[visu_sample, 3:17],
      col = ifelse(t_train[visu_sample, ]$label == "good", 3L, 2L))

## NO FREE LUNCH...

## A Partitioning tree but WITHOUT the bad trick of extensions encoding
## And low depth:
tree_model <- rpart(label ~ url_length + slashes_count + dots_count +
                        host_is_ip + vowels_prev + ends_in_slash + contains_port +
                        n_params + is_common_domain_ext + is_uncommon_domain_ext +
                        is_uncommon_file_ext + has_4_consonants_straight,
                    data = t_train,
                    method = "class",
                    control = rpart.control(cp = 0.05))
tree_model ; plot(tree_model); text(tree_model)
t_test$predicted <- predict(tree_model, t_test, type="class")
table(t_test[, c("label", "predicted")])

## A Partitioning tree but WITHOUT the bad trick of extensions encoding
## And more depth:
tree_model <- rpart(label ~ url_length + slashes_count + dots_count +
                        host_is_ip + vowels_prev + ends_in_slash + contains_port +
                        n_params + is_common_domain_ext + is_uncommon_domain_ext +
                        is_uncommon_file_ext + has_4_consonants_straight,
                    data = t_train[,c(2,c(4:11, 14:ncol(t_train)))],
                    control = rpart.control(cp = 0.001))
tree_model; plot(tree_model); text(tree_model)
t_test$predicted <- predict(tree_model, t_test, type="class")
table(t_test[, c("label", "predicted")])

## Now a Partitioning tree including extensions variables badly encoded
tree_model <- rpart(label ~ .,
                    data = t_train[,c(2,4:ncol(t_train))],
                    control = rpart.control(cp = 0.001))
tree_model; plot(tree_model); text(tree_model)
t_test$predicted <- predict(tree_model, t_test[, 3:ncol(t_test)], type="class")
table(t_test[, c("label", "predicted")])

## Do some Logistic Regression as a first test:
## Probably won't be good
logit_model <- glm(label_num ~ 
                       .,
                   # url_length + slashes_count + dots_count + 
                   # host_is_ip + vowels_prev + ends_in_slash + contains_port +
                   # n_params + is_common_domain_ext + is_uncommon_domain_ext +
                   # is_uncommon_file_ext + has_4_consonants_straight,
                   data = t_train[,3:ncol(t_train)],
                   family = binomial)
t_test$predicted <- predict(logit_model, t_test, type="response")
# Evaluating the model's performance
rmse <- caret::RMSE(t_test$predicted, t_test$label_num)
print(rmse)
t_test$predicted <- ifelse(predict(logit_model, t_test[,2:ncol(t_train)], type="response") >= 0.5, 1, 0)
table(t_test[, c("label_num", "predicted")])
summary(logit_model) ## Interesting to discuss concept of Entropy once again!


## Random Forest are expected to be MUCH better with same training data:
rf_model <- randomForest(label_num ~ 
                             .,
                         # url_length + slashes_count + dots_count + 
                         #     host_is_ip + vowels_prev + ends_in_slash + contains_port +
                         #     n_params + is_common_domain_ext + is_uncommon_domain_ext +
                         #     is_uncommon_file_ext + has_4_consonants_straight,
                    data = t_train[,3:ncol(t_train)])
print(rf_model)
plot(rf_model)
## Time to discuss "over-fitting"! Train error is 0.18
## but with Test data ~25%!
t_test$predicted <- predict(rf_model, t_test[,2:ncol(t_train)], type="response")

rmse <- caret::RMSE(t_test$predicted, t_test$label_num)
print(rmse)
t_test$predicted <- round(predict(rf_model, t_test, type="class"))
table(t_test[, c("label", "predicted")])
confusion_matrix <- confusionMatrix(as.factor(t_test$predicted), as.factor(t_test$label_num))
confusion_matrix

## Can a neural network do any better? Nnet is 3 layers (one hidden)
## I expect results similar to that of the Random Forest
nn_model <- nnet(label_num ~ 
                     .,
                     # url_length + slashes_count + dots_count + 
                     # host_is_ip + vowels_prev + ends_in_slash + contains_port +
                     # n_params + is_common_domain_ext + is_uncommon_domain_ext +
                     # is_uncommon_file_ext + has_4_consonants_straight,
                         data = t_train[,3:ncol(t_train)],
                 size=7,
                 maxit=10000,
                 decay=1e-3,
                 # hidden=4,
                 # learningrate = 0.001,
                 # entropy = TRUE, 
                 #trace = FALSE,
                 linout=F,
                 MaxNWts = 5000)

par(mar = c(1, 5, 0.8, 0.4),
    mai = c(0.8, 0.8, 0.8, 0.4))
plotnet(nn_model)
dev.off()
t_test$predicted <- predict(nn_model, t_test)
table(t_test[, c("label_num", "predicted")])
confusion_matrix <- confusionMatrix(as.factor(round(t_test$predicted)), as.factor(t_test$label_num))
confusion_matrix


## Final test, can we do better at all?
## Mono core, not perfect...
## https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/
dnn_model <- neuralnet(label_num ~ 
                           .,
                           # url_length + slashes_count + dots_count + 
                           # host_is_ip + vowels_prev + ends_in_slash + contains_port +
                           # n_params + is_common_domain_ext + is_uncommon_domain_ext +
                           # is_uncommon_file_ext + has_4_consonants_straight,
                       data = t_train[,3:ncol(t_train)],
                       hidden = c(10, 5, 4),
                       # hidden = c(5,4),
                       ## NO CLUE why I'm doing this here, just testing with more hidden layers :D
                       linear.output = FALSE,
                       #stepmax = 5e+04,
                       threshold = 0.05,
                       # learningrate = 0.01,
                       lifesign = 'full',
                       rep=1)
## That config takes LOOOOONG for that little data :D

plot(dnn_model, col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = T,
     information = T,
     fill = 'lightblue')

t_test$predicted <- predict(dnn_model, t_test)
confusion_matrix <- confusionMatrix(as.factor(round(t_test$predicted)), as.factor(t_test$label_num))
confusion_matrix


# ## DNN, multicore
# library(h2o)
# h2o.init() ## Requires Java, not yet on my Mac, so not today
#https://bradleyboehmke.github.io/HOML/deep-learning.html


## SVM
## https://bradleyboehmke.github.io/HOML/svm.html
svm_model <- train(label_num ~ .,
                   data = t_train[,3:ncol(t_train)],
                   method = "svmRadial",               
                   preProcess = c("center", "scale"),  
                   trControl = trainControl(method = "cv", number = 5),
                   tuneLength = 2)
t_test$predicted <- predict(svm_model, t_test[,3:ncol(t_test)])
confusion_matrix <- confusionMatrix(as.factor(round(t_test$predicted)), as.factor(t_test$label_num))
confusion_matrix
