
''' Script file import '''
import config
import commons
import preprocess
import machine_learning

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
    df['index'] = list(range(1, (len(df) + 1)))

    for index, row in df.iterrows():
        new = {}
        new['index'] = row['index']
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

def generate_result(filename, input_path, output_path):
    data = commons.read_json_propor_dataset(input_path.format(filename))

    features = pd.DataFrame(columns=['embeddings_max', 'embeddings_diff', 'embeddings_pca', 'tfidf', 'word_overlap', 'ngram_proportion', 'uncommon_proportion', 'antonyms'])

    features['embeddings_max'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'max')
    features['embeddings_diff'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'diff')
    features['embeddings_pca'] = similarity.embeddings_similarity(data, word_embeddings, 't', 'h', 'pca')

    features['tfidf'] = similarity.tfidf(data, 't', 'h')
    features['word_overlap'] = similarity.word_overlap(data, 't', 'h')
    features['ngram_proportion'] = similarity.ngram_proportion(data, 't', 'h') # Alta correlação com TF-IDF
    features['uncommon_proportion'] = similarity.uncommon_word_proportion(data, 't', 'h')
    features['antonyms'] = similarity.count_shared_antonyms(data, 't', 'h', lang=default_language, deep=3) # Sem antônimos até nível 3
    features['target'] = list(data['similarity'])

    with codecs.open(output_path.format(filename), 'w', encoding='utf-8') as writer:
        json.dump(to_json(features), writer)
    
    

if __name__ == '__main__':

    print('loading keyvectors')
    word_embeddings = KeyedVectors.load_word2vec_format('/home/allan/word_embeddings/fasttext/cbow_s1000.txt', unicode_errors="ignore")
    default_language = 'por'

    input_base_path = '/home/allan/git/Data/Aleksejs/{0}.json'
    input_synonym_path = '/home/allan/git/Data/Aleksejs/linguistic_relations/Synonyms/{0}-synonym.json'
    input_hyperonym_path = '/home/allan/git/Data/Aleksejs/linguistic_relations/Synonyms_and_Hyperonyms/{0}-synonym-hyperonym.json'

    output_base_path = '/home/allan/git/Data/Aleksejs/fasttext/1000/'

    output_path = output_base_path + '{0}_processed.json'
    output_synonym_path = output_base_path + '{0}-synonym_processed.json'
    output_hyperonym_path = output_base_path + '{0}-synonym-hyperonym_processed.json'

    print('start processing sentences')

    know_files = ('setP1', 'setP2', 'setP3')

    for x in know_files:

        print('Processing for {0}'.format(x))

        generate_result(x, input_base_path, output_path) # base
        generate_result(x, input_synonym_path, output_synonym_path) # synonym
        generate_result(x, input_hyperonym_path, output_hyperonym_path) # synonym and hyperonym

    print('Finished!')