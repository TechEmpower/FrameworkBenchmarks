import re

# Replaces all text found using the regular expression to_replace with the supplied replacement.
def replace_text(file, to_replace, replacement):
  with open(file, "r") as conf:
    contents = conf.read()
    replaced_text = re.sub(to_replace, replacement, contents)
  f = open(file, "w")
  f.write(replaced_text)
  f.close()