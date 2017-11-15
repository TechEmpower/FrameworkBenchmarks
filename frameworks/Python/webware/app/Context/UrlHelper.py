def getQueryNum(queryString):
    try:
        num_queries = int(queryString)
        if num_queries < 1:
            return 1
        if num_queries > 500:
            return 500
        return num_queries
    except ValueError:
        return 1
