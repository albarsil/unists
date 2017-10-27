
# Synonym

sentence_train_synonym_1 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.synonym(x[1], x[2]))
sentence_train_synonym_2 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.synonym(x[2], x[1]))

sentence_test_synonym_1 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.synonym(x[1], x[2]))
sentence_test_synonym_2 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.synonym(x[2], x[1]))

sentences_train[,1] <- sentence_train_synonym_1
sentences_train[,2] <- sentence_train_synonym_2

sentences_test[,1] <- sentence_test_synonym_1
sentences_test[,2] <- sentence_test_synonym_2

write.table(sentences_train, "Data/Propor/assin-ptbr-train-synomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
write.table(sentences_test, "Data/Propor/assin-ptbr-test-synomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
print(0)


# Hyponym

sentence_train_hyponym_1 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.hyponym(net, x[1], x[2]))
sentence_train_hynonym_2 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.hyponym(net, x[2], x[1]))

sentence_test_hynonym_1 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.hyponym(net, x[1], x[2]))
sentence_test_hynonym_2 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.hyponym(net, x[2], x[1]))

sentences_train[,1] <- sentence_train_hyponym_1
sentences_train[,2] <- sentence_train_hynonym_2

sentences_test[,1] <- sentence_test_hynonym_1
sentences_test[,2] <- sentence_test_hynonym_2

write.table(sentences_train, "Data/Propor/assin-ptbr-train-hyponomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
write.table(sentences_test, "Data/Propor/assin-ptbr-test-hyponomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
print(0)


# Synonym and Hyponym

sentences_train <- fread("Data/Propor/assin-ptbr-train-synomy.csv", header = TRUE, sep = ";", encoding = "UTF-8")
sentences_test <- fread("Data/Propor/assin-ptbr-test-synomy.csv", header = TRUE, sep = ";", encoding = "UTF-8")

sentence_train_synonym_hyponym_1 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.hyponym(net, x[1], x[2]))
sentence_train_synonym_hynonym_2 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.hyponym(net, x[2], x[1]))

sentence_test_synonym_hynonym_1 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.hyponym(net, x[1], x[2]))
sentence_test_synonym_hynonym_2 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.hyponym(net, x[2], x[1]))

sentences_train[,1] <- sentence_train_synonym_hyponym_1
sentences_train[,2] <- sentence_train_synonym_hynonym_2

sentences_test[,1] <- sentence_test_synonym_hynonym_1
sentences_test[,2] <- sentence_test_synonym_hynonym_2

write.table(sentences_train, "Data/Propor/assin-ptbr-train-synonym-hyponomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
write.table(sentences_test, "Data/Propor/assin-ptbr-test-synonym-hyponomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
print(0)