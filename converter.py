import geemap
import os
from geemap.conversion import *

#work_dir = os.path.join(os.path.expanduser('~'), 'rriggs')
js_dir = 'E:/research/RatingCurveAnalysis/src/js'

js_to_python_dir(in_dir=js_dir, out_dir=js_dir, use_qgis=True)
print("Python scripts saved at: {}".format(js_dir))