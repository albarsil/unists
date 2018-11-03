
''' Script file import '''
# import config
# import commons
import preprocess
# import machine_learning

from nltk.stem.snowball import PortugueseStemmer
from xml.dom import minidom
import string
import pickle
import numpy
import nltk
import re
import json
import codecs
import commons
import config
import machine_learning as ml
import preprocess
import similarity_methods as similarity
import wordnet
import pandas as pd
from gensim.models import KeyedVectors
import csv
import sys

"""

Author: Allan Barcelos

"""

def to_json(df):
    data = []
    for index, row in df.iterrows():
        new = {}
        new['embeddings_max'] = row['embeddings_max']
        new['embeddings_diff'] = row['embeddings_diff']
        new['embeddings_pca'] = row['embeddings_pca']
        new['tfidf'] = row['tfidf']
        new['word_overlap'] = row['word_overlap']
        new['ngram_proportion'] = row['ngram_proportion']
        new['uncommon_proportion'] = row['uncommon_proportion']
        new['antonyms'] = row['antonyms']
        new['target'] = row['target']

        data.append(new)

    return data

if __name__ == '__main__':
    sentences_1 = commons.read_json_propor_dataset('C:/Users/Asilva/Documents/Git/unists-python/data/splitted_synonym_hyperonym/setP1-synonym-hyperonym.json')
    sentences_2 = commons.read_json_propor_dataset('C:/Users/Asilva/Documents/Git/unists-python/data/splitted_synonym_hyperonym/setP2-synonym-hyperonym.json')
    sentences_3 = commons.read_json_propor_dataset('C:/Users/Asilva/Documents/Git/unists-python/data/splitted_synonym_hyperonym/setP3-synonym-hyperonym.json')

    trainset = pd.concat([sentences_1, sentences_2], axis=0)
    testset = sentences_3
    default_language = 'por'

    # word_embeddings = KeyedVectors.load_word2vec_format('XXXXXXXXXXXXXXXXXXXXX', unicode_errors='ignore')
    data = pd.concat([trainset, testset], axis=0)

    features = pd.DataFrame(columns=['embeddings_max', 'embeddings_diff', 'embeddings_pca', 'tfidf', 'word_overlap', 'ngram_proportion', 'uncommon_proportion', 'antonyms'])

    # features['embeddings_max'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'max')
    # features['embeddings_diff'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'diff')
    # features['embeddings_pca'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'pca')

    features['tfidf'] = similarity.tfidf(data, 't', 'h')
    features['word_overlap'] = similarity.word_overlap(data, 't', 'h')
    features['ngram_proportion'] = similarity.ngram_proportion(data, 't', 'h') # Alta correlação com TF-IDF
    features['uncommon_proportion'] = similarity.uncommon_word_proportion(data, 't', 'h')
    features['antonyms'] = similarity.count_shared_antonyms(data, 't', 'h', lang=default_language, deep=3) # Sem antônimos até nível 3
    features['target'] = list(data['similarity'])

    features.to_csv('C:/Users/Asilva/Documents/Git/unists-python/extracted_features.csv', sep=";",quotechar='"',index=False, quoting=csv.QUOTE_ALL, encoding="utf-8")
        