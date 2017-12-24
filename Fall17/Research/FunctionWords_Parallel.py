import numpy as np
import pandas as pd
import urllib.request
import _pickle as cPickle
import time
from joblib import Parallel, delayed
import multiprocessing
from functools import partial


urlstr = "http://abel.lis.illinois.edu/cgi-bin/cohese/search.py?PMIDs="
out_Functional_file_name = "D:\\Research\\Data\\FunctionWord_Parallel"

def getListofWords(pin, start_time = 0):
    urlstr = "http://abel.lis.illinois.edu/cgi-bin/cohese/search.py?PMIDs="
    pmidurl = urlstr + str(pin)
    pmidurl = pmidurl.replace("]", "").replace("[", "")
    if(int(time.time() - start_time) % 120 == 0):
        print("Time elapsed: ", int(time.time() - start_time)) 
    
    with urllib.request.urlopen(pmidurl) as url:
        string_data = url.read().decode("utf-8")
        list_words = _parse(string_data)
        
        try:
            print(pmidurl)
            worddf = pd.DataFrame(list_words, columns=['Word', 'CohessionScore', 'Freq', 'Association'])
            # convert stopwords into cohession score = -1
            worddf.loc[worddf['CohessionScore'] == 'stopword', 'CohessionScore'] = -1
            # remove any remaining spaces
            worddf.Association = worddf.Association.str.strip()
            worddf.Word = worddf.Word.str.strip()
            # convert CohessionScore from object to numeric
            worddf.CohessionScore = pd.to_numeric(worddf.CohessionScore)
            # Filter based on CohessionScore and Association
            filtered_worddf = worddf.loc[(worddf['CohessionScore'] < 0.1)]
            filtered_worddf = filtered_worddf.loc[((filtered_worddf['Association'].str.contains("\\\\N")) | \
                                                (filtered_worddf['Association'].str.contains("None")))]
            #self.Function_words = self.Function_words.append(filtered_worddf)
            #if(Function_words.shape[0] % 50 == 0):
            #    print(self.Function_words.shape[0])
            return filtered_worddf
        except Exception as e:
            return e
    

def _isNum(str):
    if(any(dig.isdigit() for dig in str) == True):
        return True
    else:
        return False

def _parse(s):
    tuples = s.split('), ')
    out = []
    for x in tuples:
        #print(x)
        if(len(x.strip('[]()""\n').split(', ')) != 4):
            print("error string: ", s)
            raise Exception ("Error String")
        a,b,c,d = x.strip('[]()""\n').split(', ')
        a = a.replace("'", "")
        d = d.replace("'", "")
        #print(a, b, c, d)
        if(_isNum(b) == True):
            b = float(b)
        else:
            b = b.replace("'", "")
        out.append([str(a), str(b), float(c), str(d)])
    return out


def save_to_file(out_file, out_file_name):
    print("saving file...")
    with open(out_file_name, 'wb') as thefile:
        cPickle.dump(out_file, thefile)


def RunClear():
    num_cores = multiprocessing.cpu_count()
    pool = multiprocessing.Pool(processes = num_cores - 1)
    pool.close()
    pool.join()
    print("Cleared...")


if __name__ == "__main__":
    clr = False
    if(clr == True):
        RunClear()
    else:
        
        chunk_size = 1
        
        authors_data = pd.DataFrame.from_csv("D:\\Research\\Data\\soloauthors.tsv", sep='\t')
        PMID = authors_data.index
        PMID = np.array(PMID)
        print(len(PMID))

        #chunks = [PMID[i:i + chunk_size] for i in range(0, PMID[1:100].shape[0], chunk_size)]
        #print(len(chunks))
        #for delta in range(190001, 240000, 10000):
        delta = 230001
        start_index = delta
        end_index = delta + 10000
        print(start_index, " to ", end_index)

        RunClear()
        
        num_cores = multiprocessing.cpu_count()
        print(num_cores)

        # create our pool with `num_processes` processes
        pool = multiprocessing.Pool(processes=num_cores-1)
        
        # apply our function to each chunk in the list
        start_time = time.time()
        #func = partial(getListofWords, start_time)
        try:
            result = pool.map(getListofWords, PMID[start_index: end_index])
        except:
            pass
        end_time = time.time()

        pool.close()
        pool.join()

        print(end_time - start_time)

        save_to_file(result, out_Functional_file_name + "_" + str(start_index) + "_" + str(end_index) + ".PICKLE")

        RunClear()
        '''    
        start_time = time.time()
        # apply our function to each chunk in the list
        for pin in PMID[1:100]:
            result_seq = result_seq.append(getListofWords(pin))
        #results = Parallel(n_jobs=num_cores)(delayed(getListofWords)(urlstr + str(pin)) for pin in PMID[1:10])
        end_time = time.time()

        print(end_time - start_time)
        
        '''
        