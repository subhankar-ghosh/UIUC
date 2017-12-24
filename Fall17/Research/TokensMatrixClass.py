import numpy as np
import pandas as pd
import urllib.request
import matplotlib.pyplot as plt

class GetTokens:

    def __init__(self, word_lst = [], freq = {}):
        print("GetTokens Initialized...")
        self.word_list = word_lst
        self.frequency = freq

    def ran(self,pid):
        return 1

    def _isNum(self, str):
        if(any(dig.isdigit() for dig in str) == True):
            return True
        else:
            return False

    def _parse(self, s):
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
            if(self._isNum(b) == True):
                b = float(b)
            else:
                b = b.replace("'", "")
            out.append([str(a), str(b), float(c), str(d)])
        return out

    def CreateWordList(self, sample):
        count = 0
        for s in sample:
            count = count + 1
            #print(s)
            self.GetListofWords(s)
            if(count % 100 == 0):
                print("Record #", count, " processed...")
        return self.word_list

    def CreateOccuranceMatrix(self):
        for word in self.word_list:
            count = self.frequency.get(word, 0)
            self.frequency[word] = count + 1
            print(word,"->",self.frequency[word])
        return self.frequency



    def GetListofWords(self, pinid):
        import urllib.request
        urlstr = "http://abel.lis.illinois.edu/cgi-bin/cohese/search.py?PMIDs="
        pmidurl = urlstr + str(pinid)
        pmidurl = pmidurl.replace("]", "").replace("[", "")
        
        with urllib.request.urlopen(pmidurl) as url:
            string_data = url.read().decode("utf-8")
            list_words = self._parse(string_data)
            
            try:
                #global word_list
                #print(pmidurl)
                worddf = pd.DataFrame(list_words, columns=['Word', 'CohessionScore', 'Freq', 'Association'])
                # convert stopwords into cohession score = -1
                #worddf.loc[worddf['CohessionScore'] == 'stopword', 'CohessionScore'] = 0
                # remove any remaining spaces
                #worddf.Association = worddf.Association.str.strip()
                worddf.Word = worddf.Word.str.strip()
                # convert CohessionScore from object to numeric
                #worddf.CohessionScore = pd.to_numeric(worddf.CohessionScore)
                self.word_list = self.word_list + worddf.Word.tolist()
                #print(self.word_list[1:5])
                #sns.distplot(worddf.CohessionScore, bins = num_bins)
                #plt.axvline(np.median(worddf.CohessionScore))
                #plt.show()                
            except Exception as e:
                pass
                #return 0

