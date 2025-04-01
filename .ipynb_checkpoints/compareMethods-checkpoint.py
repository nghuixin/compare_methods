# %%
# When you download this as a Python script, Jupyter will automatically insert the environment shebang here. 

def getMeanValue(valueList):
    """
    Calculate the mean (average) value from a list of values.
    Input: list of integers/floats
    Output: mean value
    """
    valueTotal = 0.0
 
    for value in valueList:
        valueTotal += value
    numberValues = len(valueList)
    
    return (valueTotal/numberValues)



def intersection(lst1, lst2):
    return list(set(lst1) & set(lst2))

def MAE(predictions, labels):
    differences = [abs(x-y) for x,y in zip(predictions,labels)]
    return sum(differences) / len(differences)


