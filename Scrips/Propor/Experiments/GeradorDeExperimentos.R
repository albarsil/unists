
exp1 <- svm_teste_model(target ~ embeddings + tfidf, pca_embeddings_tfidf_train[,-3], pca_embeddings_tfidf_test[,-3])
exp2 <- svm_teste_model(target ~ pca + tfidf, pca_embeddings_tfidf_train[,-1], pca_embeddings_tfidf_test[,-1])
exp3 <- svm_teste_model(target ~ pca + embeddings + tfidf, pca_embeddings_tfidf_train, pca_embeddings_tfidf_test)

local_propor_data <- LoadProporDataWithoutProcessing(filename = "assin-ptbr-test.xml")
a <- data.frame(Frase1 = local_propor_data[,1], Frase2 = local_propor_data[,2], Alvo = pca_embeddings_tfidf_test$target, embeddings_tfidf = exp1$Result(), pca_tfidf = exp2$Result(), embeddings_pca_tfidf = exp3$Result())

write.table(a, "resultados preliminares.csv", append = F, quote = T, sep = ";", row.names = F, col.names = T, fileEncoding = "UTF-8", dec = ',')
