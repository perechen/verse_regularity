from ru_accent_poet import accent_line
import ru_accent_poet as accent
import os
import re
import shutil


for path, subdirs, files in os.walk("../data/prose/prose_samples"):
    for name in files:
        print(os.path.join(path,name))
        accent.write_file([os.path.join(path, name)])