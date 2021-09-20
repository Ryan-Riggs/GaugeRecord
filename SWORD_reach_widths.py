import ee
#ee.Authenticate()
ee.Initialize()

x = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.py'
fls = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
flsh = 'users/eeProject/RivWidthCloudPaper:rwc_landsat.py'
fnsLandsat = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
lsFun = 'users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.py'
riverFun = 'users/eeProject/RivWidthCloudPaper:functions_river.py'
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
#fc_3spc = ee.FeatureCollection("users/rriggs/east_validation_1spc_3x/eastern_merged")
fc_3spc = ee.FeatureCollection("users/rriggs/SA_sj_min_100") ##South America. 
fc_3spc = ee.FeatureCollection("users/rriggs/na_sj_using_R_min_100")
#gauges = ee.FeatureCollection("users/rriggs/Gauge_points") ##USGS Gauges 
#gauges = ee.FeatureCollection("users/rriggs/Canadian_gauges_100m")
#gauges = ee.FeatureCollection("users/rriggs/South_AM_100m_stationid")
#fc_3spc = ee.FeatureCollection("users/rriggs/Africa_min_100min_3x_1spc")
#gauges = ee.FeatureCollection("users/rriggs/LwrMiss_points")
#gauges = ee.FeatureCollection("users/rriggs/MissBasinPoints")
#gauges = ee.FeatureCollection("users/rriggs/NApoints")
gauges = ee.FeatureCollection("users/rriggs/GSIM_plus_India")


jrcSummary = ee.Image("JRC/GSW1_0/GlobalSurfaceWater")
occ = jrcSummary.select('occurrence')
jrcMeta = ee.Image("JRC/GSW1_0/Metadata")
nobs = jrcMeta.select('valid_obs')
change = jrcSummary.select('change_abs')
change = change.abs()
waterMax = occ.gte(0)
grwl = ee.FeatureCollection("users/eeProject/grwl")
# create image with bands ranging from quantile 0-100
quantileBreaks = ee.List.sequence(0, 100, 1)
#import folium
def creatQuantileImage(l, prev):
    return(ee.Image(prev).addBands(occ.gte(ee.Image.constant(l)).rename([ee.String('q').cat(ee.Number(l).format('%03d'))])))


# rename the flags so that their value won't be replaced in the next step
# when calculating the mean river mask values
def appendStringFlag(l):
    return(ee.String(l).cat('_flag'))

def Mndwi(image):
    return(image.normalizedDifference(['Green', 'Swir1']).rename('mndwi'))

def Mbsrv(image):
    return(image.select(['Green']).add(image.select(['Red'])).rename('mbsrv'))

def Mbsrn(image):
    return(image.select(['Nir']).add(image.select(['Swir1'])).rename('mbsrn'))

def Ndvi(image):
    return(image.normalizedDifference(['Nir', 'Red']).rename('ndvi'))

def Awesh(image):
    return(image.expression('Blue + 2.5 * Green + (-1.5) * mbsrn + (-0.25) * Swir2', {
    'Blue': image.select(['Blue']),
    'Green': image.select(['Green']),
    'mbsrn': Mbsrn(image).select(['mbsrn']),
    'Swir2': image.select(['Swir2'])}))

def Dswe(i):
    mndwi = Mndwi(i)
    mbsrv = Mbsrv(i)
    mbsrn = Mbsrn(i)
    awesh = Awesh(i)
    swir1 = i.select(['Swir1'])
    nir = i.select(['Nir'])
    ndvi = Ndvi(i)
    blue = i.select(['Blue'])
    swir2 = i.select(['Swir2'])

    t1 = mndwi.gt(0.124)
    t2 = mbsrv.gt(mbsrn)
    t3 = awesh.gt(0)
    t4 = (mndwi.gt(-0.44)
    .And(swir1.lt(900))
    .And(nir.lt(1500))
    .And(ndvi.lt(0.7)))
    t5 = (mndwi.gt(-0.5)
    .And(blue.lt(1000))
    .And(swir1.lt(3000))
    .And(swir2.lt(1000))
    .And(nir.lt(2500)))

    t = t1.add(t2.multiply(10)).add(t3.multiply(100)).add(t4.multiply(1000)).add(t5.multiply(10000))

    noWater = (t.eq(0)
    .Or(t.eq(1))
    .Or(t.eq(10))
    .Or(t.eq(100))
    .Or(t.eq(1000)))
    hWater = (t.eq(1111)
    .Or(t.eq(10111))
    .Or(t.eq(11011))
    .Or(t.eq(11101))
    .Or(t.eq(11110))
    .Or(t.eq(11111)))
    mWater = (t.eq(111)
    .Or(t.eq(1011))
    .Or(t.eq(1101))
    .Or(t.eq(1110))
    .Or(t.eq(10011))
    .Or(t.eq(10101))
    .Or(t.eq(10110))
    .Or(t.eq(11001))
    .Or(t.eq(11010))
    .Or(t.eq(11100)))
    pWetland = t.eq(11000)
    lWater = (t.eq(11)
    .Or(t.eq(101))
    .Or(t.eq(110))
    .Or(t.eq(1001))
    .Or(t.eq(1010))
    .Or(t.eq(1100))
    .Or(t.eq(10000))
    .Or(t.eq(10001))
    .Or(t.eq(10010))
    .Or(t.eq(10100)))

    iDswe = (noWater.multiply(0)
    .add(hWater.multiply(1))
    .add(mWater.multiply(2))
    .add(pWetland.multiply(3))
    .add(lWater.multiply(4)))

    return(iDswe.rename(['dswe']))

def ClassifyWaterJones2019(img):
    dswe = Dswe(img)
    waterMask = dswe.eq(1).Or(dswe.eq(2)).rename(['waterMask'])
    out = waterMask.copyProperties(img)
    return(out.set({'system:time_start':img.get('system:time_start')}))

def Ndvi(image):
    # // calculate ndvi
    ndvi = image.normalizedDifference(['Nir', 'Red']).rename('ndvi')
    return ndvi

def Evi(image):
    # calculate the enhanced vegetation index
    evi = image.expression('2.5 * (Nir - Red) / (1 + Nir + 6 * Red - 7.5 * Blue)', {
    'Nir': image.select(['Nir']),
    'Red': image.select(['Red']),
    'Blue': image.select(['Blue'])
    })
    return evi.rename(['evi'])

def Mndwi(image):
    # calculate mndwi
    mndwi = image.normalizedDifference(['Green', 'Swir1']).rename('mndwi')
    return mndwi

def ClassifyWaterZou2018(image):
    mndwi = Mndwi(image)
    ndvi = Ndvi(image)
    evi = Evi(image)
    water = (mndwi.gt(ndvi).Or(mndwi.gt(evi))).And(evi.lt(0.1)).rename(['waterMask'])
    out = water.copyProperties(image)
    return(out.set({'system:time_start':image.get('system:time_start')}))



def switchGeometryLine2Endpoints(f):
        f = f.set({'lineGeometry': f.geometry()})
        l = f.geometry().coordinates()
        g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
        return(f.setGeometry(g))

def switchGeometryEndpoints2Line(f):
        return(f.setGeometry(f.get('lineGeometry')).set('lineGeometry', None))

def GetCenterline(clDataset, bound):
    # // filter the GRWL centerline based on area of interest
    cl = clDataset.filterBounds(bound)
    return(cl)

def ExtractChannel(image):
    # // extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
    connectedToCl = (image.Not().cumulativeCost(
        source = ee.Image().toByte().paint(grwl_cline, 1).And(image), #// only use the centerline that overlaps with the water mask
        maxDistance = 4000,
        geodeticDistance = False).eq(0))

    channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask'])
    return(channel)


def RemoveIsland(channel):
    # /* fill in island as water if the size (number of pixels) of the island is smaller than FILL_SIZE */
    fill = channel.Not().selfMask().connectedPixelCount(333).lt(333)
    river = channel.where(fill, ee.Image(1)).rename(['riverMask'])
    return(river)

def ExtractRiver(imgIn, clData, maxDist, minIslandRemoval):
    waterMask = imgIn.select(['waterMask'])
    bound = waterMask.geometry()
    cl = GetCenterline(clData, bound)
    channelMask = ExtractChannel(waterMask, cl, maxDist)
    riverMask = RemoveIsland(channelMask, minIslandRemoval)
    return(imgIn.addBands(channelMask).addBands(riverMask))

def widths(image):
  width = (image.eq(1).reduceRegions(
  collection = filt, 
  reducer= ee.Reducer.mean(),
  ))
  flags = (image.reduceRegions(
  collection= width.map(switchGeometryLine2Endpoints), 
  reducer= ee.Reducer.max(),
))
  return(flags)


def filt_lines (f):
  return f.set('geo_type', f.geometry().type())

def maximum_no_of_tasks(MaxNActive, waitingPeriod):
	"""maintain a maximum number of active tasks
	"""
	import time
	import ee
	ee.Initialize()

	time.sleep(10)
	## initialize submitting jobs
	ts = list(ee.batch.Task.list())

	NActive = 0
	for task in ts:
		if ('RUNNING' in str(task) or 'READY' in str(task)):
			NActive += 1
	## wait if the number of current active tasks reach the maximum number
	## defined in MaxNActive
	while (NActive >= MaxNActive):
		time.sleep(waitingPeriod) # if reach or over maximum no. of active tasks, wait for 2min and check again
		ts = list(ee.batch.Task.list())
		NActive = 0
		for task in ts:
			if ('RUNNING' in str(task) or 'READY' in str(task)):
				NActive += 1
	return()
 
 ##changed to 2000 from 5000
def buffer_zone (f):
  return f.buffer(2000)

def buffer_zone1 (f):
  return f.buffer(ee.Number(filter_gauge.first().get('GRWL_wd')).multiply(3))

def  ftr_coll (f):
  return ee.FeatureCollection(f)

def distance_fun (f):
  l = f.geometry().distance(filter_gauge.geometry())
  d = f.set('distance', l)
  return(d)

def distance_fun_poly (f):
  l = f.geometry().distance(far.geometry())
  d = f.set('distance_1', l)
  return(d)

def Unpack(bitBand, startingBit, bitWidth):
    # unpacking bit bands
    # see: https://groups.google.com/forum/#!starred/google-earth-engine-developers/iSV4LwzIW7A
    return (ee.Image(bitBand)
            .rightShift(startingBit)
            .bitwiseAnd(ee.Number(2).pow(ee.Number(bitWidth)).subtract(ee.Number(1)).int()))
def UnpackAllSR(bitBand):
    # apply Unpack function for multiple pixel qualities
    bitInfoSR = {
    'Cloud': [5, 1],
    'CloudShadow': [3, 1],
    'SnowIce': [4, 1],
    'Water': [2, 1]
    }
    unpackedImage = ee.Image.cat([Unpack(bitBand, bitInfoSR[key][0], bitInfoSR[key][1]).rename([key]) for key in bitInfoSR])
    return unpackedImage
def AddFmaskSR(image):
    # // add fmask as a separate band to the input image
    temp = UnpackAllSR(image.select(['BQA']))

    fmask = (temp.select(['Water']).rename(['fmask'])
    .where(temp.select(['SnowIce']), ee.Image(3))
    .where(temp.select(['CloudShadow']), ee.Image(2))
    .where(temp.select(['Cloud']), ee.Image(4))
    .mask(temp.select(['Cloud']).gte(0)))

    return image.addBands(fmask)

riverMask = ExtractChannel(waterMax)
quantileImage = ee.Image(quantileBreaks.iterate(creatQuantileImage, ee.Image())).select('^q.*').updateMask(riverMask).unmask(0)
bn = ee.List(quantileImage.bandNames()).map(appendStringFlag)
reducer = ee.Reducer.anyNonZero().forEach(bn)

collection = fc_3spc.map(filt_lines)
fc_3spc = collection.filter(ee.Filter.eq('geo_type', 'LineString'))

#fc_3spc = fc_3spc.filterBounds(rc_roi)
LT5_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
LE7_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
LC8_BANDS = ['B2',   'B3',    'B4',  'B5',  'B6',    'B7',    'B10', 'pixel_qa'];
STD_NAMES = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'Temp', 'BQA'];
bn = ['B2', 'B3', 'B4', 'B8','B11','B12','QA10','SCL']

#// load Landsat 5,7,8 collections:
#// TODO(GHA): combine 5, 7, and 8 collections:
LT5 = ee.ImageCollection('LANDSAT/LT5_L1T_SR').select(LT5_BANDS, STD_NAMES);
LT5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR').select(LT5_BANDS, STD_NAMES); 
LE7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR').select(LE7_BANDS, STD_NAMES).filterDate('1999-01-01', '2003-05-30')
##.select(LE7_BANDS, STD_NAMES);
LC8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR').select(LC8_BANDS, STD_NAMES);

collection = LC8.merge(LT5).merge(LE7)

def sentinelFunction (f):
  return(f.copyProperties(f).set({'area':100}))

def landsatFunction (f):
  return(f.copyProperties(f).set({'area':900}))

def sentinelCloudFunction(f):
  temp = f.select('BQA').rename('fmask')
  temp = temp.eq(3).Or(temp.eq(7)).Or(temp.eq(8)).Or(temp.eq(9)).Or(temp.eq(10))
  fmask = temp.where(temp.eq(7), ee.Image(2)).where(temp.eq(11), ee.Image(3)).where(temp.eq(8).Or(temp.eq(9).Or(temp.eq(10))), ee.Image(4))
  fmask = fmask.where(fmask.eq(1), ee.Image(3))
  return(f.addBands(fmask))

sent = ee.ImageCollection("COPERNICUS/S2_SR")
sentinel = sent.select(bn, STD_NAMES)
modist = ee.ImageCollection("MODIS/006/MOD09GQ")
modisa = ee.ImageCollection("MODIS/006/MYD09GQ")
modis = modist.merge(modisa)


p=['1_In',
'2_In',
'4_In',
'5_In',
'6_In',
'7_In',
'10_In',
'12_In',
'13_In',
'14_In',
'15_In',
'16_In',
'17_In',
'19_In',
'21_In',
'24_In',
'25_In',
'26_In',
'27_In',
'30_In',
'32_In',
'33_In',
'34_In',
'36_In',
'37_In',
'38_In',
'39_In',
'41_In',
'42_In',
'43_In',
'47_In',
'48_In',
'49_In',
'50_In',
'52_In',
'53_In',
'55_In',
'56_In',
'57_In',
'59_In',
'60_In',
'61_In',
'62_In',
'63_In',
'65_In',
'69_In',
'70_In',
'71_In',
'72_In',
'73_In',
'74_In',
'76_In',
'79_In',
'81_In',
'82_In',
'83_In',
'84_In',
'85_In',
'87_In',
'88_In',
'89_In',
'90_In',
'91_In',
'92_In',
'93_In',
'95_In',
'96_In',
'98_In',
'99_In',
'100_In',
'101_In',
'102_In',
'103_In',
'104_In',
'106_In',
'108_In',
'112_In',
'114_In',
'115_In',
'116_In',
'117_In',
'118_In',
'119_In',
'122_In',
'123_In',
'124_In',
'125_In',
'128_In',
'129_In',
'134_In',
'135_In',
'136_In',
'137_In',
'140_In',
'145_In',
'146_In',
'147_In',
'149_In',
'152_In',
'156_In',
'157_In',
'158_In',
'160_In',
'164_In',
'166_In',
'174_In',
'176_In',
'177_In',
'181_In',
'187_In',
'190_In',
'194_In',
'207_In',
'215_In',
'216_In',
'220_In',
'221_In',
'225_In',
'228_In',
'229_In',
'232_In',
'233_In',
'234_In',
'236_In',
'239_In',
'240_In',
'241_In',
'245_In',
'247_In',
'248_In',
'252_In',
'253_In',
'255_In',
'258_In',
'263_In',
'265_In',
'266_In',
'267_In',
'269_In',
'271_In',
'272_In',
'284_In',
'285_In',
'286_In',
'290_In',
'291_In',
'292_In',
'293_In',
'294_In',
'295_In',
'300_In',
'304_In',
'305_In',
'307_In',
'310_In',
'311_In',
'313_In',
'315_In',
'316_In',
'317_In',
'321_In',
'322_In',
'324_In',
'325_In',
'326_In',
'329_In',
'330_In',
'334_In',
'336_In',
'354_In',
'359_In',
'365_In',
'367_In',
'368_In',
'369_In',
'370_In',
'372_In',
'373_In',
'376_In',
'378_In',
'380_In',
'381_In',
'382_In',
'383_In',
'384_In',
'386_In',
'387_In',
'388_In',
'389_In',
'398_In',
'405_In',
'407_In',
'410_In',
'411_In',
'412_In',
'415_In',
'418_In',
'419_In',
'421_In',
'427_In',
'429_In',
'433_In',
'434_In',
'436_In',
'437_In',
'438_In',
'440_In',
'441_In',
'442_In',
'443_In',
'444_In',
'447_In',
'448_In',
'450_In',
'451_In',
'452_In',
'453_In',
'456_In',
'457_In',
'460_In',
'462_In',
'463_In',
'464_In',
'465_In',
'466_In',
'467_In',
'468_In',
'470_In',
'472_In',
'474_In',
'475_In',
'476_In',
'477_In',
'478_In',
'484_In',
'485_In',
'486_In',
'489_In',
'490_In',
'492_In',
'495_In',
'496_In',
'500_In',
'501_In',
'506_In',
'507_In',
'508_In',
'509_In',
'510_In',
'515_In',
'516_In',
'517_In',
'518_In',
'520_In',
'523_In',
'527_In',
'528_In',
'529_In',
'530_In',
'532_In',
'534_In',
'537_In',
'538_In',
'539_In',
'542_In',
'544_In',
'549_In',
'554_In',
'556_In',
'557_In',
'560_In',
'561_In',
'564_In',
'565_In',
'567_In',
'568_In',
'569_In',
'570_In',
'571_In',
'573_In',
'574_In',
'575_In',
'578_In',
'579_In',
'582_In',
'583_In',
'584_In',
'585_In',
'588_In',
'589_In',
'590_In',
'594_In',
'596_In',
'597_In',
'600_In',
'602_In',
'606_In',
'608_In',
'609_In',
'611_In',
'612_In',
'614_In',
'615_In',
'616_In',
'619_In',
'621_In',
'627_In',
'628_In',
'629_In',
'636_In',
'637_In',
'643_In',
'645_In',
'647_In',
'652_In',
'654_In',
'655_In',
'656_In',
'658_In',
'676_In',
'677_In',
'679_In',
'681_In',
'682_In',
'686_In',
'687_In',
'689_In',
'690_In',
'691_In',
'692_In',
'695_In',
'699_In',
'707_In',
'709_In',
'711_In',
'713_In',
'718_In',
'720_In',
'721_In',
'723_In',
'724_In',
'725_In',
'726_In',
'727_In',
'733_In',
'736_In',
'740_In',
'741_In',
'743_In',
'745_In',
'746_In',
'748_In',
'749_In',
'750_In',
'753_In',
'754_In',
'755_In',
'756_In',
'758_In',
'759_In',
'764_In',
'767_In',
'771_In',
'775_In',
'784_In',
'797_In',
'803_In',
'804_In',
'805_In',
'806_In',
'808_In',
'809_In',
'810_In',
'812_In',
'815_In',
'818_In',
'821_In',
'824_In',
'826_In',
'828_In',
'831_In',
'832_In',
'834_In',
'835_In',
'839_In',
'840_In',
'841_In',
'842_In',
'844_In',
'846_In',
'847_In',
'848_In',
'850_In',
'856_In',
'857_In',
'858_In',
'859_In',
'860_In',
'861_In',
'864_In',
'865_In',
'867_In',
'869_In',
'871_In',
'873_In',
'874_In',
'878_In',
'881_In',
'885_In',
'887_In',
'888_In',
'892_In',
'899_In',
'901_In',
'905_In',
'906_In',
'907_In',
'913_In',
'918_In',
'920_In',
'921_In',
'922_In',
'923_In',
'924_In',
'926_In',
'930_In',
'931_In',
'933_In',
'934_In',
'935_In',
'936_In',
'937_In',
'938_In',
'939_In',
'940_In',
'946_In',
'947_In',
'948_In',
'956_In',
'957_In',
'958_In',
'959_In',
'962_In',
'968_In',
'972_In',
'973_In',
'976_In',
'977_In',
'981_In',
'983_In',
'985_In',
'990_In',
'992_In',
'994_In',
'996_In',
'998_In',
'999_In',
'1000_In',
'1004_In',
'1006_In',
'1007_In',
'1011_In',
'1012_In',
'1015_In',
'1017_In',
'1018_In',
'1024_In',
'1025_In',
'1030_In',
'1031_In',
'1033_In',
'1034_In',
'1035_In',
'1036_In',
'1037_In',
'1038_In',
'1040_In',
'1043_In',
'1044_In',
'1046_In',
'1047_In',
'1049_In',
'1052_In',
'1055_In',
'1057_In',
'1060_In',
'1061_In',
'1063_In',
'1064_In',
'1065_In',
'1069_In',
'1072_In',
'1073_In',
'1074_In',
'1075_In',
'1080_In',
'1082_In',
'1083_In',
'1088_In',
'1089_In',
'1090_In',
'1096_In',
'1103_In',
'1104_In',
'1105_In',
'1108_In',
'1109_In',
'1111_In',
'1112_In',
'1113_In',
'1114_In',
'1115_In',
'1116_In',
'1117_In',
'1118_In',
'1120_In',
'1125_In',
'1127_In',
'1131_In',
'1132_In',
'1133_In',
'1135_In',
'1136_In',
'1137_In',
'1138_In',
'1141_In',
'1142_In',
'1144_In',
'1145_In',
'1146_In',
'1153_In',
'1156_In',
'1166_In',
'1170_In',
'1171_In',
'1172_In',
'1173_In',
'1176_In',
'1177_In',
'1178_In',
'1180_In',
'1181_In',
'1183_In',
'1185_In',
'1187_In',
'1190_In',
'1192_In',
'1193_In',
'1197_In',
'1200_In',
'1202_In',
'1203_In',
'1204_In',
'1208_In',
'1210_In',
'1211_In',
'1212_In',
'1213_In',
'1214_In',
'1215_In',
'1220_In',
'1221_In',
'1223_In',
'1225_In',
'1227_In',
'1228_In',
'1229_In',
'1232_In',
'1233_In',
'1234_In',
'1235_In',
'1236_In',
'1237_In',
'1239_In',
'1241_In',
'1242_In',
'1244_In',
'1248_In',
'1250_In',
'1252_In',
'1253_In',
'1254_In',
'1255_In',
'1257_In',
'1261_In',
'1262_In',
'1263_In',
'1267_In',
'1268_In',
'1269_In',
'1271_In',
'1272_In',
'1273_In',
'1274_In',
'1275_In',
'1276_In',
'1277_In',
'1282_In',
'1284_In',
'1285_In',
'1288_In',
'1289_In',
'1290_In',
'1292_In',
'1294_In',
'1295_In',
'1296_In',
'1297_In',
'1300_In',
'1301_In',
'1304_In',
'1305_In',
'1306_In',
'1308_In',
'1314_In',
'1315_In',
'1317_In',
'1318_In',
'1319_In',
'1320_In',
'1321_In',
'1322_In',
'1323_In',
'1324_In',
'1328_In',
'1329_In',
'1330_In',
'1331_In',
'1333_In',
'1334_In',
'1335_In',
'1337_In',
'1339_In',
'1340_In',
'1341_In',
'1342_In',
'1345_In',
'1346_In',
'1347_In',
'1349_In',
'1350_In',
'1351_In',
'1352_In',
'1353_In',
'1354_In',
'1356_In',
'1357_In',
'1358_In',
'1359_In',
'1362_In',
'1363_In',
'1364_In',
'1366_In',
'1367_In',
'1370_In',
'1376_In',
'1381_In',
'1382_In',
'1384_In',
'1385_In',
'1386_In',
'1388_In',
'1390_In',
'1392_In',
'1396_In',
'1400_In',
'1401_In',
'1402_In',
'1403_In',
'1404_In',
'1405_In',
'1406_In',
'1407_In',
'1408_In',
'1409_In',
'1410_In',
'1411_In',
'1412_In',
'1415_In',
'1416_In',
'1417_In',
'1418_In',
'1419_In',
'1420_In',
'1421_In',
'1422_In',
'1423_In',
'1424_In',
'1425_In',
'1426_In',
'1429_In',
'1432_In',
'1435_In',
'1436_In',
'1438_In',
'1439_In',
'1440_In',
'1441_In',
'1445_In',
'1447_In',
'1448_In',
'1449_In',
'1450_In',
'1451_In',
'1452_In',
'1453_In',
'1454_In',
'1458_In',
'1459_In',
'1460_In',
'1462_In',
'1463_In',
'1465_In',
'1468_In',
'1470_In',
'1472_In',
'1473_In',
'1474_In',
'1476_In',
'1477_In',
'1479_In',
'1480_In',
'1481_In',
'1482_In',
'1483_In',
'1485_In',
'1487_In',
'1490_In',
'1491_In',
'1492_In',
'1494_In',
'1495_In',
'1496_In',
'1501_In',
'1503_In',
'1504_In',
'1506_In',
'1509_In',
'1512_In',
'1513_In',
'1514_In',
'1516_In',
'1519_In',
'1520_In',
'1521_In',
'1522_In',
'1524_In',
'1525_In',
'1526_In',
'1527_In',
'1528_In',
'1529_In',
'1531_In',
'1532_In',
'1534_In',
'1535_In',
'1536_In',
'1538_In',
'1539_In',
'1540_In',
'1542_In',
'1544_In',
'1545_In',
'1546_In',
'1553_In',
'1556_In',
'1557_In',
'1558_In',
'1559_In',
'1563_In',
'1566_In',
'1568_In',
'1569_In',
'1571_In',
'1574_In',
'1575_In',
'1576_In',
'1579_In',
'1581_In',
'1582_In',
'1583_In',
'1584_In',
'1587_In',
'1593_In',
'1595_In',
'1596_In',
'1597_In',
'1598_In',
'1599_In',
'1600_In',
'1603_In',
'1604_In',
'1606_In',
'1607_In',
'1608_In',
'1609_In',
'1610_In',
'1611_In',
'1613_In',
'1616_In',
'1617_In',
'1618_In',
'1619_In',
'1620_In',
'1621_In',
'1622_In',
'1623_In',
'1625_In',
'1626_In',
'1628_In',
'1630_In',
'1631_In',
'1633_In',
'1637_In',
'1638_In',
'1639_In',
'1642_In',
'1643_In',
'1644_In',
'1645_In',
'1650_In',
'1651_In',
'1652_In',
'1653_In',
'1655_In',
'1657_In',
'1658_In',
'1660_In',
'1661_In',
'1662_In',
'1665_In',
'1666_In',
'1667_In',
'1668_In',
'1669_In',
'1670_In',
'1671_In',
'1672_In',
'1674_In',
'1675_In',
'1676_In',
'1677_In',
'1678_In',
'1679_In',
'1680_In',
'1681_In',
'1683_In',
'1684_In',
'1686_In',
'1687_In',
'1688_In',
'1689_In',
'1690_In',
'1691_In',
'1695_In',
'1705_In',
'1720_In',
'1724_In',
'1729_In',
'1733_In',
'1757_In',
'1759_In',
'1769_In',
'1772_In',
'1776_In',
'1777_In',
'1779_In',
'1783_In',
'1787_In',
'1791_In',
'1796_In',
'1801_In',
'1803_In',
'1804_In',
'1807_In',
'1813_In',
'1814_In',
'1815_In',
'1816_In',
'1817_In',
'1818_In',
'1823_In',
'1827_In',
'1828_In',
'1831_In',
'1833_In',
'1834_In',
'1841_In',
'1844_In',
'1853_In',
'1856_In',
'1865_In',
'1866_In',
'1867_In',
'1869_In',
'1870_In',
'1872_In',
'1874_In',
'1875_In',
'1877_In',
'1878_In',
'1881_In',
'1883_In',
'1886_In',
'1890_In',
'1893_In',
'1895_In',
'1898_In',
'1905_In',
'1906_In',
'1907_In',
'1908_In',
'1915_In',
'1930_In',
'1933_In',
'1943_In',
'1945_In',
'1948_In',
'1950_In',
'1959_In',
'1962_In',
'1963_In',
'1974_In',
'1976_In',
'1979_In',
'1986_In',
'1989_In',
'1994_In',
'1996_In',
'2000_In',
'2001_In',
'2012_In',
'2013_In',
'2016_In',
'2023_In',
'2024_In',
'2025_In',
'2029_In',
'2031_In',
'2032_In',
'2033_In',
'2034_In',
'2035_In',
'2036_In',
'2039_In',
'2047_In',
'2053_In',
'2055_In',
'2059_In',
'2060_In',
'2062_In',
'2064_In',
'2066_In',
'2068_In',
'2069_In',
'2076_In',
'2079_In',
'2088_In',
'2092_In',
'2102_In',
'2103_In',
'2104_In',
'2106_In',
'2107_In',
'2108_In',
'2109_In',
'2110_In',
'2112_In',
'2114_In',
'2115_In',
'2116_In',
'2118_In',
'2124_In',
'2125_In',
'2126_In',
'2127_In',
'2128_In',
'2129_In',
'2132_In',
'2135_In',
'2138_In',
'2139_In',
'2140_In',
'2141_In',
'2142_In',
'2143_In',
'2144_In',
'2145_In',
'2146_In',
'2147_In',
'2148_In',
'2149_In',
'2150_In',
'2152_In',
'2153_In',
'2154_In',
'2156_In',
'2157_In',
'2159_In',
'2160_In',
'2161_In',
'2162_In',
'2163_In',
'2166_In',
'2168_In',
'2169_In',
'2170_In',
'2171_In',
'2173_In',
'2174_In',
'2175_In',
'2177_In',
'2178_In',
'2181_In',
'2184_In',
'2185_In',
'2189_In',
'2190_In',
'2192_In',
'2195_In',
'2198_In',
'2199_In',
'2200_In',
'2202_In',
'2203_In',
'2206_In',
'2207_In',
'2208_In',
'2209_In',
'2210_In',
'2217_In',
'2218_In',
'2221_In',
'2223_In',
'2224_In',
'2228_In',
'2230_In',
'2231_In',
'2232_In',
'2233_In',
'2235_In',
'2236_In',
'2238_In',
'2241_In',
'2242_In',
'2243_In',
'2245_In',
'2246_In',
'2247_In',
'2250_In',
'2252_In',
'2253_In',
'2254_In',
'2256_In',
'2257_In',
'2260_In',
'2261_In',
'2266_In',
'2267_In',
'2268_In',
'2270_In',
'2276_In',
'2278_In',
'2281_In',
'2282_In',
'2283_In',
'2288_In',
'2291_In',
'2292_In',
'2294_In',
'2295_In',
'2296_In',
'2299_In',
'2300_In',
'2302_In',
'2305_In',
'2306_In',
'2309_In',
'2313_In',
'2316_In',
'2317_In',
'2318_In',
'2319_In',
'2320_In',
'2322_In',
'2323_In',
'2328_In',
'2331_In',
'2334_In',
'2336_In',
'2337_In',
'2338_In',
'2346_In',
'2347_In',
'2356_In',
'2364_In',
'2365_In',
'2367_In',
'2368_In',
'2369_In',
'2370_In',
'2377_In',
'2380_In',
'2381_In',
'2383_In',
'2386_In',
'2388_In',
'2390_In',
'2391_In',
'2398_In',
'2403_In',
'2404_In',
'2408_In',
'2412_In',
'2417_In',
'2419_In',
'2421_In',
'2426_In',
'2427_In',
'2428_In',
'2429_In',
'2430_In',
'2431_In',
'2432_In',
'2433_In',
'2438_In',
'2442_In',
'2445_In',
'2446_In',
'2449_In',
'2451_In',
'2453_In',
'2460_In',
'2462_In',
'2464_In',
'2465_In',
'2471_In',
'2472_In',
'2479_In',
'2482_In',
'2483_In',
'2485_In',
'2490_In',
'2491_In',
'2492_In',
'2495_In',
'2498_In',
'2501_In',
'2504_In',
'2510_In',
'2513_In',
'2514_In',
'2516_In',
'2518_In',
'2519_In',
'2520_In',
'2524_In',
'2525_In',
'2526_In',
'2528_In',
'2529_In',
'2530_In',
'2536_In',
'2537_In',
'2538_In',
'2539_In',
'2542_In',
'2545_In',
'2547_In',
'2554_In',
'2557_In',
'2558_In',
'2559_In',
'2560_In',
'2561_In',
'2562_In',
'2563_In',
'2565_In',
'2574_In',
'2578_In',
'2579_In',
'2582_In',
'2584_In',
'2585_In',
'2587_In',
'2588_In',
'2591_In',
'2593_In',
'2594_In',
'2596_In',
'2599_In',
'2600_In',
'2602_In',
'2603_In',
'2607_In',
'2608_In',
'2609_In',
'2610_In',
'2611_In',
'2613_In',
'2617_In',
'2618_In',
'2619_In',
'2624_In',
'2630_In',
'2631_In',
'2632_In',
'2634_In',
'2635_In',
'2637_In',
'2640_In',
'2641_In',
'2642_In',
'2645_In',
'2647_In',
'2650_In',
'2653_In',
'2654_In',
'2656_In',
'2657_In',
'2658_In',
'2659_In',
'2665_In',
'2666_In',
'2667_In',
'2670_In',
'2671_In',
'2672_In',
'2675_In',
'2676_In',
'2684_In',
'2686_In',
'2688_In',
'2689_In',
'2690_In',
'2697_In',
'2698_In',
'2700_In',
'2707_In',
'2710_In',
'2711_In',
'2712_In',
'2715_In',
'2717_In',
'2718_In',
'2719_In',
'2721_In',
'2722_In',
'2725_In',
'2726_In',
'2727_In',
'2732_In',
'2733_In',
'2734_In',
'2738_In',
'2739_In',
'2740_In',
'2741_In',
'2743_In',
'2744_In',
'2748_In',
'2754_In',
'2755_In',
'2757_In',
'2761_In',
'2762_In',
'2767_In',
'2771_In',
'2773_In',
'2776_In',
'2779_In',
'2788_In',
'2789_In',
'2790_In',
'2793_In',
'2794_In',
'2796_In',
'2799_In',
'2802_In',
'2804_In',
'2806_In',
'2808_In',
'2810_In',
'2812_In',
'2813_In',
'2814_In',
'2818_In',
'2819_In',
'2820_In',
'2828_In',
'2830_In',
'2831_In',
'2833_In',
'2834_In',
'2837_In',
'2838_In',
'2843_In',
'2844_In',
'2845_In',
'2849_In',
'2851_In',
'2853_In',
'2855_In',
'2856_In',
'2864_In',
'2865_In',
'2871_In',
'2900_In',
'2923_In',
'2924_In',
'2927_In',
'2954_In',
'2955_In',
'2991_In',
'2996_In',
'3000_In',
'3016_In',
'3026_In',
'3027_In',
'3039_In',
'3052_In',
'3068_In',
'3087_In',
'3090_In',
'3105_In',
'3127_In',
'3151_In',
'3152_In',
'3171_In',
'3173_In',
'3177_In',
'3178_In',
'3179_In',
'3180_In',
'3187_In',
'3195_In',
'3196_In',
'3206_In',
'3223_In',
'3234_In',
'3239_In',
'3241_In',
'3243_In',
'3244_In',
'3246_In',
'3248_In',
'3253_In',
'3255_In',
'3265_In',
'3266_In',
'3268_In',
'3292_In',
'3297_In',
'3349_In',
'3355_In',
'3426_In',
'3449_In',
'3512_In',
'3572_In',
'3618_In',
'3619_In',
'3655_In',
'3660_In',
'3661_In',
'3705_In',
'3723_In',
'3724_In',
'3725_In',
'3744_In',
'3773_In',
'3774_In',
'3788_In',
'3847_In',
'3871_In',
'3888_In',
'3889_In',
'3890_In',
'3898_In',
'3922_In',
'3949_In',
'3979_In',
'3990_In',
'4121_In',
'4124_In',
'4126_In',
'4130_In',
'4131_In',
'4132_In',
'4133_In',
'4134_In',
'4135_In',
'4138_In',
'4147_In',
'4149_In',
'4152_In',
'4153_In',
'4157_In',
'4158_In',
'4160_In',
'4161_In',
'4162_In',
'4163_In',
'4164_In',
'4165_In',
'4167_In',
'4168_In',
'4169_In',
'4171_In',
'4173_In',
'4174_In',
'4175_In',
'4176_In',
'4178_In',
'4180_In',
'4181_In',
'4182_In',
'4184_In',
'4187_In',
'4188_In',
'4190_In',
'4191_In',
'4192_In',
'4193_In',
'4194_In',
'4196_In',
'4199_In']

#p = ['3844100_grdc', 'EC_0000008_gsim']

distFilter = ee.Filter.contains(
  leftField ='.geo',
  rightField = '.geo'
  )

ee.Filter.contains()

distSaveAll = ee.Join.saveAll(
    matchesKey ='points',
    measureKey = 'distance');


  
def ij (state):
  nPowerPlants = ee.List(state.get('points'))
  mean = ee.FeatureCollection(nPowerPlants).aggregate_mean('width_m')
  return ee.FeatureCollection(state.set({'width_m': mean}))


def connected (f):
  con = f.clip(buff1) ####changed this from f.clipToBoundsAndScale(filt) &&& changed from polygon to buff1
  return con

def grwl_filter_fun (f):
  con = f.clip(polygon)
  return con


##Changed f.eq(1) to f.
def effective_width_1 (f):                     
  lng = (f.reduceRegions(
  collection= grwl_cline.filterBounds(filt), 
  reducer= ee.Reducer.count(),
  ))
  return lng 


##Changed collection = fi1.flatten().geometry()  & con.select('channelMask').eq(1) This function is causing the issues. It will not allow for the properties to be set. 
def effective_width_2 (con):
  sum = (con.select('channelMask').eq(1).reduceRegions(
  collection= buff1, ##changed from polygon 
  reducer= ee.Reducer.sum().unweighted(),  ##Unweighted added for flagging technique. 
  ))
  time = con.get('system:time_start')
  return sum.copyProperties(con).set({'system:time_start':time}) #do sum.flatte() if needed. 

##changed from 22
def Area_fun (feature):
  d = ee.Number(feature.get('sum'))
  id = feature.getString('system:index').slice(0,24)
  return feature.set({'Area': ee.Number(d).multiply(ee.Number(feature.get('area'))), 'id': id, 'system:time_start':feature.get('system:time_start'), 'length':cline_updated.length()})


##changed from 22
def len_fun (feature):
  d = ee.Number(feature.get('count'))
  id = feature.getString('system:index').slice(0,24)
  return feature.set({'length': cline_updated.length(), 'id':id})

toyFilter = ee.Filter.equals(leftField = 'system:time_start',
                             rightField ='system:time_start');


innerJoin = ee.Join.inner()


def fc_function (f):
  p = ee.Feature(f.get('primary'))
  s = ee.Feature(f.get('secondary'))
  properties = p.copyProperties(s)
  return properties


def effW_fun (feature):
  Area = ee.Number(feature.get('Area'))
  Length = ee.Number(feature.get('length'))
  return feature.set({'Effective_width': Area.divide(Length)})


##'COMID': distinct_comid.first().get('COMID')
def fields (f):
  return f.set({'width_m':grwl_filt.first().get('GRWL_wd'), 'ID': filter_gauge.first().get('Sttn_Nm'), 'c':'NA'})


def set_id (f):
  return f.set({'id': fc_id})


#Changed collection from pts
def flagging (f):
  a = (f.reduceRegions(
      collection = pts.geometry(),
      reducer = ee.Reducer.max(),
  ))
  return a
##changed from 22
def flagged_collection_fun (f):
  id = f.getString('system:index').slice(0,24)
  return f.set({'id': id})



def feature_fun (f):
  a = ee.Geometry.Point(f)
  b = ee.Feature(a, {'id': 1, 'e/o':2})
  return(b)

def id_fun (f):
  a = f.get('system:index')
  b = f.set({'id': a})
  return(b)
def getOddNumbers (f):
  number = ee.Number.parse(f.get('id'))
  remainder = number.mod(2)
  val = number.multiply(remainder)
  c = f.set({'e/o':val})
  return(c)

def farthest_distance_function (f):
  a = ee.Feature(f).distance(ee.Feature(farthest_pt))
  b = f.set('distance', a)
  return(b)

def farthest_distance_function_even (f):
  a = ee.Feature(f).distance(ee.Feature(farthest_even))
  b = f.set('distance', a)
  return(b)

def comid_fun (f):
  #a = 'COMID{}'.format(f)##.slice(0,6)
  a = str(f)
  return a

def difference_fun (f):
  p = ee.Feature(f.get('primary'))
  a = ee.Number(p.get('Area'))
  s = ee.Feature(f.get('secondary'))
  b = ee.Number(s.get('Area'))
  return s.set({'Difference': a.subtract(b)})

def change_fun (f):
  a = median_change.get('change_abs')
  b = f.set({'change':a})
  b = b.set(comid_dict)
  return b



def MaskFunction (f):
  a = f.eq(1)
  b = f.mask(a)
  return b

def cloudFunction (f):
  cld = f.select('fmask').gt(2)
  min = cld.mask(cld)
  min = (min.reduceRegion(
      geometry = polygon,
      reducer = ee.Reducer.sum().unweighted(),
  ))
  area = polygon.area()
  div= (ee.Number(min.get('fmask')).multiply(f.get('area'))).divide(ee.Number(area))
  add = f.set({'cloud': div.multiply(100)})
  return(add)

def eff_width2_function (f):
  ft = ee.FeatureCollection(f).first()
  return ee.Feature(ft).copyProperties(f).set({'system:time_start': f.get('system:time_start')})

def modisFunction (f):
  a = f.select('sur_refl_b02').reduceRegion(
  reducer = ee.Reducer.mean(), 
  geometry = polygon)
  return(ee.Feature(polygon).set({'c':a.get('sur_refl_b02'), 'system:time_start':f.get('system:time_start'), 'Difference':'NA', 'Effective_width':'NA', 'cloud':0, 'width_m':filter_gauge.first().get('GRWL_wd'), 'ID':filter_gauge.first().get('SITE_NUM')}))


##skipping any errors. 
##GRWL_buffer
for i in range(len(p)):
  #filter_gauge = gauges.filter(ee.Filter.eq('Sttn_Nm', p[i])) ##Changed from SITE_NUM
  filter_gauge = gauges.filterMetadata('Sttn_Nm', 'equals', str(p[i]))
  buff1 = filter_gauge.geometry()#.buffer(ee.Number(filter_gauge.first().get('GRWL_wd')).multiply(3))#.map(buffer_zone1)
  #fi1 = buff1.map(ftr_coll)
  filt = buff1

  grwl_filt = swordBuff.filterBounds(filt)
  grwl_cline = sword.filter(ee.Filter.eq('reach_id', grwl_filt.get('reach_id')))
  
  grwl_filt_geom = swordBuff.geometry()#.buffer(ee.Number(filter_gauge.first().get('GRWL_wd')).multiply(ee.Number(1.5)))
  intersect = grwl_filt_geom#.intersection(buff1)
##Flagging is going to be an issue. 
  intersectBuffer = intersect.buffer(30).intersection(buff1)
  buff1  = intersectBuffer
  polygon = intersect
  filtered = collection.filterDate('1984-01-01', '2021-12-31').sort('system:time_start').filterBounds(filt)#.filterMetadata('CLOUD_COVER', 'less_than',10)
  filtered = filtered.map(landsatFunction)
  sn = sentinel.filterBounds(filt).map(sentinelFunction)
  filtered = filtered#.merge(sn)

#print(filtered.getInfo())
  filt_con = filtered.map(connected)
  sn_con = sn.map(connected)
  cline_updated = grwl_filt.geometry().intersection(polygon)

  flagged = filt_con.map(AddFmaskSR)
  flagged_sn = sn_con.map(sentinelCloudFunction)
  flagged = flagged.merge(flagged_sn)




  cloud_filter = flagged.map(cloudFunction)#.filterMetadata('cloud', 'less_than', 2)

  filt_con = cloud_filter
  filtered = filt_con
  waterMask = filtered.map(ClassifyWaterZou2018)
  GetCenterline(grwl_cline, filt)
  riverMask = waterMask.map(ExtractChannel)

  connected_mask = riverMask#.map(connected)
  connected_test = connected_mask.map(grwl_filter_fun)
    

  eff_width1 = connected_test.map(effective_width_1)

  connected_test = connected_test.map(MaskFunction)
  connected_mask = connected_mask.map(MaskFunction)

  eff_width2 = connected_test.map(effective_width_2)
    

  eff_width2_filt = connected_mask.map(effective_width_2)
  #circle = eff_width2_filt.flatten().map(Area_fun)
    
  eff_width2 = eff_width2.map(eff_width2_function)
  eff_width2_filt = eff_width2_filt.map(eff_width2_function)

  area_map = eff_width2.map(Area_fun)
    
  circle = eff_width2_filt.map(Area_fun)

  fc_test = area_map
  difference = innerJoin.apply(circle, fc_test, toyFilter)
    



  diff_t = difference.map(difference_fun)

  fc_test = diff_t


  testing = fc_test.map(effW_fun)
  fields_vals = testing.map(fields)
  selection = fields_vals.select(['Effective_width', 'ID', 'id', 'COMID', 'width_m', 'Difference','system:time_start', 'cloud'])
  sel = selection.filter(ee.Filter.gt('Effective_width', 0)).filter(ee.Filter.eq('Difference', 0)).filter(ee.Filter.lte('cloud', 10))
  #sel = sel.filter(ee.Filter.eq('Difference',0))
  selection = sel
  
  #c = modis.map(modisFunction) 

  #selection = c.merge(selection)

  task = (ee.batch.Export.table.toDrive(
  collection = selection,
  description = 'widths_' + '_' + str(p[i]),
  folder = 'SWORD_30m_widths',
  fileNamePrefix = 'Gauge_' + '_' + str(p[i]),
  fileFormat = 'csv'
  ))

  task.start()
#print(output.first())
  print('task', p[i], 'has started')
  maximum_no_of_tasks(10, 90)