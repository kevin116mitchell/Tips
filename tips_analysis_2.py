# -*- coding: utf-8 -*-
"""
TIPS project for Summer II 2021 
Orange team 13
"""

import openpyxl
wb = openpyxl.load_workbook('TipsData.xlsx')

#Controls how many of the most common words in each year prints, feel free to change
#it to whatever you want
nMostCommonWords = 10

# a list of stopwords
stopwords = ["a", "about", "above", "above", "across", "after", "afterwards", "again", 
             "against", "all", "almost", "alone", "along", "already", "also","although",
             "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another",
             "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",
             "at", "back","be","became", "because","become","becomes", "becoming", "been",
             "before", "beforehand", "behind", "being", "below", "beside", "besides", "between",
             "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant",
             "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done",
             "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere",
             "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere",
             "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for",
             "former", "formerly", "forty", "found", "four", "from", "front", "full", "further",
             "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here",
             "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself",
             "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", 
             "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least",
             "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more",
             "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
             "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor",
             "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto",
             "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part",
             "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming",
             "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty",
             "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still",
             "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then",
             "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they",
             "thick", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru",
             "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un",
             "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what",
             "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein",
             "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom",
             "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours",
             "yourself", "yourselves", "the"]

# =============================================================================
# lists to hold cells' strings
# These will be lists of lists where each sublist will correspond to a year
# starting with 2021 in descending order so len(cells_SI[0]) would give you
# the amount of self improvement tips in 2021 for example.
cells_SI = []
cells_SA = []
cells_PD = []
cells_LS = []
# =============================================================================


#A function to make printing the data easier
def stringMaker(category_list):
    temp_string = "\t"
    for i in range(6):
        temp_string += str(len(category_list[i])) + "\t\t"
    return temp_string

# Loop through each sheet of the workbook
for sheet in wb:
    
    print("\n***" +str(nMostCommonWords) + " most common words for " + sheet.title + "***")
    
    # Combine all cells into one list called "all_cols"
    all_cols = []
    for col in sheet:
        all_cols+=[col[x].value for x in range(len(col))]

    # =============================================================================
    #temporary lists to hold cells' strings' for current year
    temp_SI = []
    temp_SA = []
    temp_PD = []
    temp_LS = []
   
    all_cells=[]
    for i in all_cols:
        # remove all "None" cells and go ahead and collect
        if i != None:
            all_cells.append(i)
            #collect categorical data for current year
            if "{SI}" in i:
                temp_SI.append(i)
            elif "{SA}" in i:
                temp_SA.append(i)
            elif "{PD}" in i:
                temp_PD.append(i)
            elif "{LS}" in i:
                temp_LS.append(i)
    
    #append temp lists to master list         
    cells_SI.append(temp_SI)
    cells_SA.append(temp_SA)
    cells_PD.append(temp_PD)
    cells_LS.append(temp_LS)
    # =============================================================================
    
    
    # Move all words into dictionary and use the count as value
    word_dict={}
    for cell in all_cells:
        only_alpha=""
        for char in cell:
            if ord(char) >= 65 and ord(char) <= 90:
                only_alpha += char
            ## checking for lower case
            elif ord(char) >= 97 and ord(char) <= 122:
                only_alpha += char
            elif char == " ":
                only_alpha += char
    
        word_list = only_alpha.lower().split()
    
        for word in word_list:
            if word not in word_dict and word not in stopwords:
                word_dict[word] = 1
            elif word in word_dict and word not in stopwords:
                word_dict[word] += 1
                
    # =============================================================================
    #ummmm weird ugly printing stuff, I apologize XD
    list_1 = []
    for k in sorted(word_dict, reverse=True, key=word_dict.get):
        list_1.append((k, word_dict[k]))
    list_2 = [x for index, x in enumerate(list_1) if index < nMostCommonWords]
    
    for element in list_2:
        print(str(str(element[1]) + "   " + element[0]))
    # =============================================================================
    
#Finally print out them yummy category counts by year :)
print("\n\n\t\t\t\t\t\t\t2021\t2020\t2019\t2018\t2017\t2016")
print("Self Improvement        :" + stringMaker(cells_SI))
print("Social advice           :" + stringMaker(cells_SA))
print("Professional Development:" + stringMaker(cells_PD))
print("Learning Strategies     :" + stringMaker(cells_LS))

