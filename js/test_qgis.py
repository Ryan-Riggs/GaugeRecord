import ee 
from ee_plugin import Map 

grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
# def extraFunction(ryan):
x = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.js')
fls = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
flsh = require('users/eeProject/RivWidthCloudPaper:rwc_landsat.js')
fnsLandsat = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
lsFun = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
riverFun = require('users/eeProject/RivWidthCloudPaper:functions_river.js')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
id = "4127800_grdc"
loc = grdc.filter(ee.Filter.eq('Sttn_Nm', id))
filt_gauge = loc
xsections = sword.filterBounds(loc.geometry().buffer(2000))
print('size', xsections.size())
Map.addLayer(xsections.limit(10))

#Calculate distance of water pixels from GRWL centerline for filtering out non river pixels.
def riverFun.ExtractChannel(image):
  # extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
  connectedToCl = image.Not().cumulativeCost({
    'source': ee.Image().toByte().paint(grwl_cline, 1).And(image), # only use the centerline that overlaps with the water mask
    'maxDistance': 4000,
    'geodeticDistance': False
  }).eq(0)
  channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask'])
  #Map.addLayer(channel, {}, 'river mask/channel')
  return (channel)


record = records.filter(ee.Filter.eq('Sttn_Nm', id)).filter(ee.Filter.NotNull(['value'])).filter(ee.Filter.gt('value', 0))

def yrFun(f):
  a = f.get('variable')
  return(f.set({'Date':a}))

record = record.map(yrFun)

# Match common names to the sensor-specific bands:
LT5_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa']
LE7_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa']
LC8_BANDS = ['B2',   'B3',    'B4',  'B5',  'B6',    'B7',    'B10', 'pixel_qa']
STD_NAMES = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'Temp', 'BQA']

# load Landsat 5,7,8 collections:
# TODO(GHA): combine 5, 7, and 8 collections:
LT5 = ee.ImageCollection('LANDSAT/LT5_L1T_SR') \
    .select(LT5_BANDS, STD_NAMES)
LT5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR') \
    .select(LT5_BANDS, STD_NAMES)
LE7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR') \
.select(LE7_BANDS, STD_NAMES).filterDate('1999-01-01', '2003-05-30')
LC8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR') \
   .select(LC8_BANDS, STD_NAMES)
bn = ['B2', 'B3', 'B4', 'B8','B11','B12','QA10','QA60']
collection = LT5.merge(LE7).merge(LC8)
collection = collection.filterBounds(xsections)
collection = collection.filterDate('2010-01-01','2021-12-31')

#
sent = ee.ImageCollection("COPERNICUS/S2_SR").filterBounds(xsections)
sent = sent.select(bn, STD_NAMES)

def func_yoc(f):
  a = f.get('system:time_start')
  b = ee.Date(a).format("yyyy-MM-dd")
  return(f.set({'system:id': b, 'ID':b, 'area': 100}))

sent = sent.map(func_yoc)





#


def func_zwp(f):
  a = f.get('system:time_start')
  b = ee.Date(a).format("yyyy-MM-dd")
  return(f.set({'system:id': b, 'ID':b, 'area': 900}))

collection = collection.map(func_zwp)





collection = collection#.merge(sent)
#record = record.filter(ee.Filter.gte('value', 0)).filter()
def img(f):
  a = f.get('Date')
  b = f.get('value')
  c = ee.Image.constant(b)
  return(ee.Image(c).set({'Date': a, 'Q':b,'system:id':ee.String(a), 'ID':ee.String(a)}))

imgColl = record.map(img)
toyFilter = ee.Filter.equals({
  'leftField': 'ID',
  'rightField': 'ID'
})
# Define the join.
innerJoin = ee.Join.inner('primary', 'secondary')
# Apply the join.
toyJoin = innerJoin.apply(collection,imgColl, toyFilter)

def func_isq(f):
  a = f.get('secondary')
  a = ee.Image(a).get('Q')
  b = f.get('primary')
  c = ee.Image(b).set({'Q':a})
  return(c)

combined = toyJoin.map(func_isq)









def func_elp(f):
  a = ee.Image(f)
  b = a.clip(xsections.geometry())
  return(b)

filtered = combined.map(func_elp)







#/Maybe keep??
filtered = combined
#sent = filtered.filter(ee.Filter.eq('area', 100))
#print(sent.first())
#filtered = filtered.filter(ee.Filter.eq('area', 900))



#Add Fmask values to the images for cloud filtering.
def Unpack(qualityBand, startingBit, bitWidth):
  # unpacking bit information from the quality band given the bit position and width
  # see: https:#groups.google.com/forum/#!starred/google-earth-engine-developers/iSV4LwzIW7A
  return(qualityBand \
  .rightShift(startingBit) \
  .bitwiseAnd(ee.Number(2).pow(bitWidth).subtract(1).int()))

def UnpackAllSR(bitBand):
  # apply Unpack function for multiple pixel qualities
  bitInfoSR = {
    'Cloud': [5, 1],
    'CloudShadow': [3, 1],
    'SnowIce': [4, 1],
    'Water': [2, 1]
  }
  unpackedImage = ee.Image()
  for key in bitInfoSR:
    unpackedImage = ee.Image.cat(
      unpackedImage, Unpack(bitBand, bitInfoSR[key][0], bitInfoSR[key][1]) \
      .rename(key))

  return(unpackedImage.select(Object.keys(bitInfoSR)))

def AddFmaskSR(image):
  # add fmask as a separate band to the input image
  temp = UnpackAllSR(image.select(['BQA']))

  # construct the fmask
  fmask = (temp.select(['Water']).rename(['fmask']) \
  .where(temp.select(['SnowIce']), ee.Image(3)) \
  .where(temp.select(['CloudShadow']), ee.Image(2)) \
  .where(temp.select(['Cloud']), ee.Image(4))) \
  .mask(temp.select(['Cloud']).gte(0)); 

  return(image.addBands(fmask))

flagged = ee.ImageCollection(filtered).map(AddFmaskSR)


def sentinelCloud(f):
  temp = f.select('BQA').rename('fmask')
  temp = temp.eq(3).Or(temp.eq(7)).Or(temp.eq(8)).Or(temp.eq(9)).Or(temp.eq(10))
  fmask = temp.where(temp.eq(7), ee.Image(2)) \
  .where(temp.eq(11), ee.Image(3)) \
  .where(temp.eq(8).Or(temp.eq(9).Or(temp.eq(10))), ee.Image(4))
  #.mask(temp.eq(8).Or(temp.eq(9).Or(temp.eq(10))).gte(0))
  fmask = fmask.where(fmask.eq(1), ee.Image(2))
  return(f.addBands(fmask))

#sent = ee.ImageCollection(sent).map(sentinelCloud)
flagged = flagged#.merge(sent)

def running(xsections):
xsections = ee.FeatureCollection(xsections)

def cloudFunction (f):
  cld = f.select('fmask').gt(2)
  min = cld#.mask(cld)
  min = min.reduceRegion({'geometry': xsections, 'reducer': ee.Reducer.mean()})
  add = f.set({'cloud': ee.Number(min.get('fmask')).multiply(100)})
  return(add)


filtered= flagged.filterBounds(xsections).map(cloudFunction)
filtered = filtered.filter(ee.Filter.NotNull(['cloud'])).filter(ee.Filter.lte('cloud', 10))

def waterJones(f):
  img = x.ClassifyWaterJones2019(f)
  out = img.copyProperties(f)
  return(out.set({'system:time_start': f.get('system:time_start')}))

waterMask = filtered.map(waterJones)#(x.ClassifyWaterJones2019)

filt = xsections
#Determine connectivity of water pixels to grwl centerline and filter out any water pixels more than 4km away from centerline.
def riverFun.GetCenterline(clDataset, bound):
  # filter the GRWL centerline based on area of interest
  cl = clDataset.filterBounds(bound)
  return(cl)

riverFun.GetCenterline(grwl_cline, xsections)
riverMask = waterMask.map(riverFun.ExtractChannel)

line2pt =  function(f){
        f = f.set({'lineGeometry': f.geometry()})
        l = f.geometry().coordinates()
        g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
        return(f.setGeometry(g))
}


def func_lrw(x):
  flags = x.reduceRegions(xsections.map(line2pt),ee.Reducer.max().combine(ee.Reducer.count(),"", True))
  flags = flags.first()
  flagsMax = flags.get('max')
  pts = flags.get('count')
  return(x.set({'flags':flagsMax, 'endPoints':pts}))

flagging = riverMask.map(func_lrw)







flaggingFiltered = flagging.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))

def func_pvd(x):
  wd = x.eq(1).reduceRegion({'geometry': xsections, 'reducer': ee.Reducer.mean()})
  length = xsections.geometry().length()
  wd = ee.Number(wd.get('channelMask')).multiply(ee.Number(length))
  return(x.set({'width':wd}))

widths = flaggingFiltered.map(func_pvd)







widths = widths.filter(ee.Filter.gt('width', 0))#.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))

def func_bjh(f):
  a = f.get('Q')
  b = ee.Number(a).round()
  return(f.set({'Q': b}))

final_filtered = widths.map(func_bjh)






random = final_filtered.randomColumn()
training = random.filter(ee.Filter.gte('random', .3))
testing = random.filter(ee.Filter.lt('random', .3))

lr = training.reduceColumns(ee.Reducer.linearFit(), ['width','Q'])

b = lr.get('offset')#.errorMatrix('Q', 'classification').accuracy()
slope = lr.get('scale')


def func_tnf(x):
  wd = ee.Number(x.get('width')).multiply(ee.Number(slope))
  wd = wd.add(b)
  return(x.set({'classification':ee.Number(wd)}))

r = testing.map(func_tnf)






actual = testing.aggregate_array('Q')
mean = actual.reduce(ee.Reducer.mean())


def func_rdt(f):
  model = f.get('classification')
  actual = f.get('Q')
  error = ee.Number(model).subtract(ee.Number(actual))
  rel = ee.Number(error).divide(ee.Number(mean))
  sqr = ee.Number(rel).pow(2)
  return(f.set({'sqr': sqr}))

sqr = r.map(func_rdt)









meanSqr = sqr.aggregate_array('sqr').reduce(ee.Reducer.mean())
rrmse = ee.Number(meanSqr).sqrt()
rrmse = ee.Number(rrmse).multiply(100)
out = xsections.set({'rrmse':rrmse.round()})


def func_wnu(f):
  model = f.get('classification')
  actual = f.get('Q')
  error = ee.Number(model).subtract(ee.Number(actual))
  error = ee.Number(error).pow(2)
  return(f.set({'error': error}))

nse1 = r.map(func_wnu)









def func_pjl(f):
  model = f.get('classification')
  actual = f.get('Q')
  rel = ee.Number(actual).subtract(ee.Number(mean))
  rel = ee.Number(rel).pow(2)
  return(f.set({'rel': rel}))

nse2 = r.map(func_pjl)







meanNSE1 = nse1.aggregate_array('error').reduce(ee.Reducer.mean())
meanNSE2 = nse2.aggregate_array('rel').reduce(ee.Reducer.mean())
nse = ee.Number(meanNSE1).divide(ee.Number(meanNSE2))
nse = ee.Number(1).subtract(nse)
return(xsections.first().set({'rrmse':out.get('rrmse'),'nse':nse, 'slope':slope, 'yInt':b}))


run = xsections.limit(1).map(running)
print('run', run)

#Map.addLayer(run.sort('rrmse'))




