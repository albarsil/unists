from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.decomposition import PCA
from scipy.stats import pearsonr
from functools import reduce
from sklearn.svm import SVR
from xml.dom import minidom
from numpy import array
import pickle
import nltk
import wordnet as wn
import preprocess
import pandas as pd
import re

"""

Author: Allan Barcelos

"""

def check_word_embeddings(x, word_embeddings, tryWithLower=True):
    if x in word_embeddings:
        return x
    else:
        if tryWithLower:
            if x.lower() in word_embeddings:
                return x.lower()
        
        return 'unk'

def word_overlap(data, field1, field2):
    """Baseline."""
    distances = []
    for index, row in data.iterrows():
        count = 0
        for i in row[field1]:
            if i in row[field2]:
                count += 1
        distances.append(float(count / len(row[field1])))
    return distances


def embeddings_similarity(data, word_embeddings, field1, field2, s_type):
    """Calculate the similarity between the sum of all embeddings."""
    distances = []
    for index, row in data.iterrows():
        e1 = [i if i in word_embeddings else 'unk' for i in row[field1]]
        e2 = [i if i in word_embeddings else 'unk' for i in row[field2]]

        if s_type == 'max':
            partial = []
            for i in e1:
                partial.append(
                    array([word_embeddings.similarity(i, j) for j in e2]).max()
                )
            distances.append(float(array(partial).mean()))
        elif s_type == 'diff':
            distances.append(float(word_embeddings.n_similarity(e1, e2)))
        elif s_type == 'pca':
            e1 = word_embeddings[e1]
            e2 = word_embeddings[e2]

            pc1 = PCA(n_components=1, svd_solver='full')

            pc1.fit(e1)
            e1 = pc1.transform(e1)

            pc1.fit(e2)
            e2 = pc1.transform(e2)
            distances.append(float(cosine_similarity(e1, e2)))
    return distances

def tfidf(data, field1, field2):
    texts = [i for i in data[field1]]
    texts += [i for i in data[field2]]

    vectorizer = TfidfVectorizer()
    vectorizer.fit(texts)
    distances = []

    for index, row in data.iterrows():
        t1 = vectorizer.transform([row[field1]])
        t2 = vectorizer.transform([row[field2]])
        distances.append(float(cosine_similarity(t1, t2)))

    return distances

def ngram_proportion(data, field1, field2):
    distances = []

    for index, row in data.iterrows():
        t1 = nltk.word_tokenize(row[field1])
        t2 = nltk.word_tokenize(row[field2])

        grams_1 = preprocess.ngrams(t1, 3)
        grams_2 = preprocess.ngrams(t2, 3)

        grams_1 = [x.replace(' ', '_') for x in grams_1]
        grams_2 = [x.replace(' ', '_') for x in grams_2]

        local_distances = []

        local_distances = [x if x in grams_2 else None for x in grams_1]
        local_distances.extend([x if x in grams_1 else None for x in grams_2])
        local_distances = list(set(local_distances))

        distances.append(sum([0 if x is None else 1 for x in local_distances])/(len(t1) + len(t2)))

    return distances

def uncommon_word_proportion(data, field1, field2):
    distances = []

    for index, row in data.iterrows():
        t1 = nltk.word_tokenize(row[field1])
        t2 = nltk.word_tokenize(row[field2])

        grams_1 = preprocess.ngrams(t1, 3)
        grams_2 = preprocess.ngrams(t2, 3)

        grams_1 = [x.replace(' ', '_') for x in grams_1]
        grams_2 = [x.replace(' ', '_') for x in grams_2]

        local_distances = []

        local_distances = [x if x in grams_2 else None for x in grams_1]
        local_distances.extend([x if x in grams_1 else None for x in grams_2])
        local_distances.extend([x if x in t2 else None for x in t1])
        local_distances.extend([x if x in t1 else None for x in t2])
        local_distances = list(set(local_distances))

        distances.append(sum([1 if x is None else 0 for x in local_distances])/(len(t1) + len(t2)))

    return distances

def count_shared_antonyms(data, field1, field2, lang='por', deep=2):
    distances = []

    for index, row in data.iterrows():
        t1 = nltk.word_tokenize(row[field1])
        t2 = nltk.word_tokenize(row[field2])

        t1 = list(set(t1))
        t2 = list(set(t2))

        antonyms_t1 = []
        antonyms_t2 = []

        for x in t1:
            antonyms_t1.extend(wn.find_antonyms(x,lang,deep))

        for x in t2:
            antonyms_t2.extend(wn.find_antonyms(x,lang,deep))

        antonyms_t1 = list(set(antonyms_t1))
        antonyms_t2 = list(set(antonyms_t2))

        shared = []

        for x in antonyms_t1:
            if x in t2:
                shared.append(x)
        
        for x in antonyms_t2:
            if x in t1:
                shared.append(x)

        # Pega os elementos únicos
        shared = list(set(shared))

        return len(shared)

def embeddings_matrix_based_similarity(data, word_embeddings, field1, field2):
    """Calculate the similarity between the sum of all embeddings."""
    distances = []
    for index, row in data.iterrows():
        
        e1 = re.findall(r"[\w']+|[.,!?;]", row[field1])
        e2 = re.findall(r"[\w']+|[.,!?;]", row[field2])
        
        e1 = [check_word_embeddings(i, word_embeddings) for i in e1]
        e2 = [check_word_embeddings(i, word_embeddings) for i in e2]

        partial = []
        for i in e1:
            partial.append(
                array([word_embeddings.similarity(i, j) for j in e2])
            )

        k = pd.DataFrame(partial, index=e1, columns=e2)        
        distances.append(sum(k.apply(max, axis=1))/len(k))
    return distances

def embeddings_binary_matrix_based_similarity(data, word_embeddings, field1, field2):
    """Calculate the similarity between the sum of all embeddings."""
    distances = []
    for index, row in data.iterrows():
        e1 = re.findall(r"[\w']+|[.,!?;]", row[field1])
        e2 = re.findall(r"[\w']+|[.,!?;]", row[field2])
        
        e1 = [check_word_embeddings(i, word_embeddings) for i in e1]
        e2 = [check_word_embeddings(i, word_embeddings) for i in e2]

        partial = []
        for i in e1:
            partial.append(
                array([1 if i == j else 0 for j in e2])
            )

        k = pd.DataFrame(partial, index=e1, columns=e2)        
        #distances.append(k.stack().mean())
        distances.append(sum(k.apply(max, axis=1))/len(k))
    return distances