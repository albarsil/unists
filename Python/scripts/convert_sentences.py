import preprocess
import wordnet
import nltk
import codecs
import pickle
import json
import commons

"""

Author: Allan Barcelos

"""

def assin_generalize_synonyms(df, output, lang = 'por', to_json = False):
    data = []
    for index, row in df.iterrows():
        new = {}
        t1 = row['t']
        t2 = row['h']

        # Remove stopwords and tokenize
        t1 = preprocess.filter_stopwords(nltk.word_tokenize(t1))
        t2 = preprocess.filter_stopwords(nltk.word_tokenize(t2))
        
        # Remove spaces and to lower
        t1 = [t.lower().strip() for t in t1]
        t2 = [t.lower().strip() for t in t2]

        # Convert numbers to words
        t1 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t1]
        t2 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t2]

        t1 = ' '.join(t1)
        t2 = ' '.join(t2)
               
        # Replace hyperonyms
        # pth1 = [k if k in nltk.word_tokenize(t2) else t for k in find_hypernyms(t, lang) if t in nltk.word_tokenize(t2) else t for t in nltk.word_tokenize(t1)]
        # pth2 = [k if k in nltk.word_tokenize(t1) else t for k in find_hypernyms(t, lang) if t in nltk.word_tokenize(t1) else t for t in nltk.word_tokenize(t2)]
        pth1 = preprocess.replace_word_synonyms(t1, t2, lang, 3)
        pth2 = preprocess.replace_word_synonyms(t2, t1, lang, 3)

        new['t'] = pth1
        new['h'] = pth2
        
        # Similaridade apenas para conjunto de treinamento
        try:
            new['entailment'] = row['entailment']
            new['similarity'] = row['similarity']
        except:
            pass
        data.append(new)

    with codecs.open(output, "wb", encoding='utf8', errors='ignore') as fp:
        if to_json == True:
            json.dump(data, fp, ensure_ascii=False)
        else:
            pickle.dump(data, fp)

def assin_generalize_hyperonyms(df, output, lang = 'por', to_json = False):
    data = []
    for index, row in df.iterrows():
        new = {}
        t1 = row['t']
        t2 = row['h']

        # Remove stopwords and tokenize
        t1 = preprocess.filter_stopwords(nltk.word_tokenize(t1))
        t2 = preprocess.filter_stopwords(nltk.word_tokenize(t2))
        
        # Remove spaces and to lower
        t1 = [t.lower().strip() for t in t1]
        t2 = [t.lower().strip() for t in t2]

        # Convert numbers to words
        t1 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t1]
        t2 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t2]
               
        # Replace hyperonyms
        # pth1 = [k if k in nltk.word_tokenize(t2) else t for k in find_hypernyms(t, lang) if t in nltk.word_tokenize(t2) else t for t in nltk.word_tokenize(t1)]
        # pth2 = [k if k in nltk.word_tokenize(t1) else t for k in find_hypernyms(t, lang) if t in nltk.word_tokenize(t1) else t for t in nltk.word_tokenize(t2)]
        pth1 = preprocess.replace_word_hypernyms(t1, t2, lang, 3)
        pth2 = preprocess.replace_word_hypernyms(t2, t1, lang, 3)

        new['t'] = pth1
        new['h'] = pth2

        # Similaridade apenas para conjunto de treinamento
        try:
            new['entailment'] = row['entailment']
            new['similarity'] = row['similarity']
        except:
            pass
        data.append(new)

    with codecs.open(output, "wb", encoding='utf8', errors='ignore') as fp:
        if to_json == True:
            json.dump(data, fp, ensure_ascii=False)
        else:
            pickle.dump(data, fp)

def assin_generalize_synonyms_and_hyperonyms(df, output, lang = 'por', to_json = False):
    data = []
    for index, row in df.iterrows():
        
        new = {}
        t1 = row['t']
        t2 = row['h']

        
        # Remove stopwords and tokenize
        t1 = preprocess.filter_stopwords(nltk.word_tokenize(t1))
        t2 = preprocess.filter_stopwords(nltk.word_tokenize(t2))
        
        # Remove spaces and to lower
        t1 = [t.lower().strip() for t in t1]
        t2 = [t.lower().strip() for t in t2]

        # Convert numbers to words
        t1 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t1]
        t2 = [preprocess.number_to_word(s.strip(), lang).replace('vírgula zero', '') if preprocess.is_number_tryexcept(s.strip()) else s for s in t2]

        t1 = ' '.join(t1)
        t2 = ' '.join(t2)
        
        pts1 = preprocess.replace_word_hypernyms_of_synonyms(t1, t2, lang, 3)
        pts2 = preprocess.replace_word_hypernyms_of_synonyms(t2, t1, lang, 3)
              
        new['t'] = pts1
        new['h'] = pts2

        # Similaridade apenas para conjunto de treinamento
        try:
            new['entailment'] = row['entailment']
            new['similarity'] = row['similarity']
        except:
            pass
        
        data.append(new)

    with codecs.open(output, "wb", encoding='utf8', errors='ignore') as fp:
        if to_json == True:
            json.dump(data, fp, ensure_ascii=False)
        else:
            pickle.dump(data, fp)   

if __name__ == '__main__':

    assin_generalize_synonyms_and_hyperonyms(
        read_json_propor_dataset('C:/Users/Asilva/Documents/Git/unists-python/Data/splitted/setP1.json'),
         'C:/Users/Asilva/Documents/Git/unists-python/Data/splitted/linguistic/setP1-synonym-hyperonym.json',
         'por',
         True)

    assin_generalize_synonyms_and_hyperonyms(
        read_json_propor_dataset('C:/Users/Asilva/Documents/Git/unists-python/Data/splitted/setE1.json'),
         'C:/Users/Asilva/Documents/Git/unists-python/Data/splitted/linguistic/setE1-synonym-hyperonym.json',
         'eng',
         True)    