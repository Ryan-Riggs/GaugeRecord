import ee 
ee.Initialize()

gages = ee.FeatureCollection('users/rriggs/GRDC_grwlFiltered')

def test (f):
  f.set({'mean':500})
  return(f)

gages = gages.map(test)
random = gages.randomColumn('random', 7, 'uniform')
training = random.filter(ee.Filter.gte('random', .5))
mean = training.aggregate_array('GRWL_wd').reduce(ee.Reducer.mean())
print(mean.getInfo())