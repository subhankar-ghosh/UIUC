import numpy as np
import pandas as pd
import os
import gensim

'''
 1) Read/load model 
 2) Read function words from list of files
 3) Create data structure to store unique function words
 4) <optional> Create data structure to store vectors of function words
 5) Write function to read/parse each line
 5) a) If multiple words then create positive word list and push list to 3 data structure
 5) b) If single word then simply push it to data structure in 3
 6) Write function to find similar words for each entry in the datastructure 
 7) Push to data structure 2
'''


model_name = "D:\\Research\\Data\\glove.6B\\glove.6B.100d.model"
out_Functional_file_name = "D:\\Research\\Data\\GloVeFunctionalWords_20.txt"
data_path = "D:\\Research\\Data\\EnglishFunctionWordsSet\\"
Functional_word_files = ["EnglishAuxiliaryVerbs.txt", "EnglishConjunctions.txt", \
"EnglishDeterminers.txt", "EnglishPrepositions.txt", "EnglishPronouns.txt", "EnglishQuantifiers.txt"]
set_Functional_word = set()
list_functional_word = []
parent_word_list = []
number_similar_words = 20


def save_to_file(out_file, out_file_name):
    print("saving file...")
    with open(out_file_name, 'w') as thefile:
        for line in out_file:
            thefile.write("%s\n" % line)


def open_file(in_file):
    print("processing file ", in_file, "...")
    word_list = []
    count = 0
    with open(in_file) as f:
        content = f.readlines()
        for line in content:
            word_list.append(line.split())
            count = count + 1
            if(count % 5 == 0):
                print(count, " line")
    return word_list


print("loading model...")
model = gensim.models.KeyedVectors.load(model_name)
print(model)

for function_files in Functional_word_files:
    parent_word_list.extend( open_file(os.path.join(data_path, function_files)))

print("parent_word_list made of size: ",  len(parent_word_list))
word_count = 0
for word in parent_word_list:
    try:
        similar_words = [w[0] for w in model.most_similar(positive = word, topn=number_similar_words)]
        for sim_word in similar_words:
            if sim_word not in set_Functional_word:
                set_Functional_word.add(sim_word)
        word_count = word_count + 1
        if(word_count % 10 == 0):
            print(word_count, " processed...")
    except KeyError:
        pass

list_functional_word = list(set_Functional_word)
print("Final List size: ", len(list_functional_word))
save_to_file(list_functional_word, out_Functional_file_name)
print("process complete")
    
    


