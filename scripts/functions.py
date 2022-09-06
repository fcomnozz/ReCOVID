import pandas as pd
import numpy as np
from datetime import datetime
from datetime import timedelta
import re
import random
import math
from collecions import Counter


def separate_date(df):
    """
    separates date from string
    """
    df['first_event_date'] = 'NA'
    for i in range(len(df)):
        x = re.split(']', df['code'][i].replace('[', ''))
        df['first_event_date'][i] = x[0]
        df['code'][i] = x[1]
    return df


def split_string(s):
    """
    s is a string code. Returns two lists, with letters (A) and numbers (B).
    """
    S = []
    for x in re.split('(\d+)', s):
        try:
            S.append(int(x))
        except ValueError:
            S.append(x)
    A, B = S[::2], S[1::2]
    return A, B


def case1(A, B, pos, neg):
    """
    Recognises case 1 reinfections (+|-|+).
    Added some lines to detect certain cases 2.
    """
    if A[0] in pos:
        i = 0
        c = B[i]
        # to find case 2 before case 1
        if A[i+1] in pos and c >= 90:
            return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
        while A[i+1] not in neg:
            i += 1
            c += B[i]
            # to find case 2 before case 1
            if A[i+1] in pos and c >= 90:
                return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
        # here the first neg is found
        i += 1
        c += B[i]
        while A[i+1] not in pos:
            i += 1
            c += B[i]
        if c >= 90:
            return 'Case 1', A[i+1:len(A)], B[i+1:len(B)]
        elif c >= 60:
            return 'Possible case 1', A[i+1:len(A)], B[i+1:len(B)]
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)]
    else:
        i = 0
        while A[i] not in pos:
            i += 1
        c = B[i]
        # same as above
        if A[i+1] in pos and c >= 90:
            return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
        while A[i+1] not in neg:
            i += 1
            c += B[i]
            if A[i+1] in pos and c >= 90:
                return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
        i += 1
        c += B[i]
        while A[i+1] not in pos:
            i += 1
            c += B[i]
        if c >= 90:
            return 'Case 1', A[i+1:len(A)], B[i+1:len(B)]
        elif c >= 60:
            return 'Possible case 1', A[i+1:len(A)], B[i+1:len(B)]
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)]


def case2(A, B, pos):
    """
    Recognises case 2 reinfections (+|+)
    """
    c = 0
    if A[0] in pos:
        for i in range(len(B)):
            if A[i+1] not in pos:
                if bool([x for x in A[i+1:len(A)] if(x in pos)]) == False:
                    if c >= 60:
                        return 'Possible Case 2', A[i:len(A)], B[i:len(B)]
                    else:
                        return '', A[i:len(A)], B[i:len(B)]
                c += B[i]
            else:
                if c + B[i] >= 90:
                    return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
                c = B[i]
        if c >= 60:
            return 'Possible Case 2', A[i+1:len(A)], B[i+1:len(B)]
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)]
    else:
        i = 0
        while A[i] not in pos:
            i += 1
        for j in range(i, len(B)):
            if A[j+1] not in pos:
                if bool([x for x in A[j+1:len(A)] if(x in pos)]) == False:
                    if c >= 60:
                        return 'Possible Case 2', A[j:len(A)], B[j:len(B)]
                    else:
                        return '', A[j:len(A)], B[j:len(B)]
                c += B[j]
            else:
                if c + B[j] >= 90:
                    return 'Case 2', A[j+1:len(A)], B[j+1:len(B)]
                c = B[j]
        if c >= 60:
            return 'Possible Case 2', A[j+1:len(A)], B[j+1:len(B)]
        else:
            return '', A[j+1:len(A)], B[j+1:len(B)]


def sum_date(date, days_list):
    return str(datetime.strptime(date, '%Y-%m-%d').date() + timedelta(days = sum(days_list)))


def case1d(A, B, pos, neg, date = ''):
    """
    Recognises case 1 reinfections (+|-|+).
    Added some lines to detect certain cases 2.
    """
    if A[0] in pos:
        i = 0
        c = B[i]
        # to find case 2 before case 1
        #if A[i] in pos and A[i+1] in pos and B[i] >= 90:
        #    return 'Case 2', A[i+1:len(A)], B[i+1:len(B)]
        if A[i+1] in pos and c >= 90:
            return 'Case 2', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
        while A[i+1] not in neg:
            i += 1
            c += B[i]
            # to find case 2 before case 1
            if A[i+1] in pos and c >= 90:
                return 'Case 2', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
        # here the first neg is found
        i += 1
        c += B[i]
        while A[i+1] not in pos:
            i += 1
            c += B[i]
        if c >= 90:
            return 'Case 1', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
        elif c >= 60:
            return 'Possible case 1', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
    else:
        i = 0
        while A[i] not in pos:
            i += 1
        j = i # index of first infection
        c = B[i]
        # same as above
        if A[i+1] in pos and c >= 90:
            return 'Case 2', A[i+1:len(A)], B[i+1:len(B)], sum_date(date, B[:j]), sum_date(date, B[:i+1])
        while A[i+1] not in neg:
            i += 1
            c += B[i]
            if A[i+1] in pos and c >= 90:
                return 'Case 2', A[i+1:len(A)], B[i+1:len(B)], sum_date(date, B[:j]), sum_date(date, B[:i+1])
        i += 1
        c += B[i]
        while A[i+1] not in pos:
            i += 1
            c += B[i]
        if c >= 90:
            return 'Case 1', A[i+1:len(A)], B[i+1:len(B)], sum_date(date, B[:j]), sum_date(date, B[:i+1])
        elif c >= 60:
            return 'Possible case 1', A[i+1:len(A)], B[i+1:len(B)], sum_date(date, B[:j]), sum_date(date, B[:i+1])
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)], sum_date(date, B[:j]), sum_date(date, B[:i+1])


def case2d(A, B, pos, date = ''):
    """
    Recognises case 2 reinfections (+|+)
    """
    c = 0
    if A[0] in pos:
        for i in range(len(B)):
            if A[i+1] not in pos:
                if bool([x for x in A[i+1:len(A)] if(x in pos)]) == False:
                    if c >= 60:
                        return 'Possible Case 2', A[i:len(A)], B[i:len(B)], date, sum_date(date, B[:i+1])
                    else:
                        return '', A[i:len(A)], B[i:len(B)], date, sum_date(date, B[:i+1])
                c += B[i]
            else:
                if c + B[i] >= 90:
                    return 'Case 2', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
                c = B[i]
        if c >= 60:
            return 'Possible Case 2', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
        else:
            return '', A[i+1:len(A)], B[i+1:len(B)], date, sum_date(date, B[:i+1])
    else:
        i = 0
        while A[i] not in pos:
            i += 1
        k = i
        for j in range(i, len(B)):
            if A[j+1] not in pos:
                if bool([x for x in A[j+1:len(A)] if(x in pos)]) == False:
                    if c >= 60:
                        return 'Possible Case 2', A[j:len(A)], B[j:len(B)], sum_date(date, B[:k]), sum_date(date, B[:j])
                    else:
                        return '', A[j:len(A)], B[j:len(B)], sum_date(date, B[:k]), sum_date(date, B[:j])
                c += B[j]
            else:
                if c + B[j] >= 90:
                    return 'Case 2', A[j+1:len(A)], B[j+1:len(B)], sum_date(date, B[:k]), sum_date(date, B[:j+1])
                c = B[j]
        if c >= 60:
            return 'Possible Case 2', A[j+1:len(A)], B[j+1:len(B)], sum_date(date, B[:k]), sum_date(date, B[:j+1])
        else:
            return '', A[j+1:len(A)], B[j+1:len(B)], sum_date(date, B[:k]), sum_date(date, B[:j+1])


def classif(s, pos = ['A', 'C', 'D'], neg = ['B'], date = ''):
    """
    Takes a string and classifies it into reinfection cases 1 or 2
    """
    A, B = split_string(s)
    L = []
    D = []
    d1 = ''
    d2 = ''
    while len(A) > 1:
        # indices stores a list between the first and last positive in the string
        indices = [i for i, x in enumerate(A) if x in pos]
        # b will be used to break the loop if there are no positives in the remaining string
        b = [x for x in A[1:len(A)] if(x in pos)]
        if any(n in A[min(indices):max(indices)] for n in neg):
            if date == '':
                r, A, B = case1(A, B, pos, neg)
            else:
                r, A, B, d1, d2 = case1d(A, B, pos, neg, date)
                date = d2
            if r != '':
                L.append(r)
                D.append([d1, d2])
            #print('Case 1', A)
        elif bool(b) == False:
            break
        else:
            if date == '':
                r, A, B = case2(A, B, pos)
            else:
                r, A, B, d1, d2 = case2d(A, B, pos, date)
                date = d2
            #print('Case 2', A)
            if r != '':
                L.append(r)
                D.append([d1, d2])
    if date != '':
        return L, D
    return L


def two_pos(S, pos = ['A', 'C', 'D']):
    l = []
    for p,c in enumerate(S):
        if(c in pos):
            l.append(p)
    if len(l) == 2:
        return True


def find_sequenced(s):
    if not 'C' in s:
        return 'Not sequenced'
    l = [x for x in s if(x in ['A', 'C', 'D'])]
    if l == ['C', 'C']:
        return 'Both sequenced'
    if l[0] == 'C':
        return 'First PCR sequenced'
    if l[1] == 'C':
        return 'Second PCR sequenced'


def last_date(date, string):
    L, N = split_string(string)
    d = sum(N)
    return str(datetime.strptime(date, '%Y-%m-%d').date() + timedelta(days = d))


def probable_variant(date1, date2):
    a = datetime.strptime('2021-02-08', '%Y-%m-%d').date() # alpha start
    d = datetime.strptime('2021-06-28', '%Y-%m-%d').date() # delta start
    o = datetime.strptime('2021-12-20', '%Y-%m-%d').date() # omicron start
    date1 = datetime.strptime(date1, '%Y-%m-%d').date()
    date2 = datetime.strptime(date2, '%Y-%m-%d').date()
    if date1 < a:
        if date2 < a:
            return 'pre-alpha:pre-alpha'
        if date2 < d:
            return 'pre-alpha:alpha'
        if date2 < o:
            return 'pre-alpha:delta'
        else:
            return 'pre-alpha:omicron'
    if date1 < d:
        if date2 < d:
            return 'alpha:alpha'
        if date2 < o:
            return 'alpha:delta'
        else:
            return 'alpha:omicron'
    if date1 < o:
        if date2 < o:
            return 'delta:delta'
        else:
            return 'delta:omicron'
    else:
        return 'omicron:omicron'
