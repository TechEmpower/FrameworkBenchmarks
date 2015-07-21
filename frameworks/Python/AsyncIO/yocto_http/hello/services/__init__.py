from . import world

# get from Django hello application
def queries_number(number):
    try:
        queries = int(number)
    except Exception:
        queries = 1
    if queries < 1:
        queries = 1
    if queries > 500:
        queries = 500
    return queries

