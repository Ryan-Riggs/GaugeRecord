import ee 
ee.Initialize()
#from ee_plugin import Map 

sword = ee.FeatureCollection('users/rriggs/AllxSections3')
records = ee.FeatureCollection('users/rriggs/grdc_gee1')
grdc = ee.FeatureCollection('users/rriggs/GSIM_plus_India')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
# def extraFunction(ryan):
#x = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.js')
# fls = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# flsh = require('users/eeProject/RivWidthCloudPaper:rwc_landsat.js')
# fnsLandsat = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# lsFun = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js')
# riverFun = require('users/eeProject/RivWidthCloudPaper:functions_river.js')
grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')

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
	
	
p = ['6246612_grdc',
'6258226_grdc',
'6273302_grdc',
'6273303_grdc',
'6273614_grdc',
'6274153_grdc',
'6274155_grdc',
'6274157_grdc',
'6274353_grdc',
'6274677_grdc',
'6274691_grdc',
'6279500_grdc',
'6335020_grdc',
'6335030_grdc',
'6335050_grdc',
'6335060_grdc',
'6335070_grdc',
'6335080_grdc',
'6335100_grdc',
'6335113_grdc',
'6335115_grdc',
'6335150_grdc',
'6335170_grdc',
'6335180_grdc',
'6335200_grdc',
'6335240_grdc',
'6335301_grdc',
'6335302_grdc',
'6335303_grdc',
'6335304_grdc',
'6335400_grdc',
'6335500_grdc',
'6335530_grdc',
'6335600_grdc',
'6335601_grdc',
'6335602_grdc',
'6335695_grdc',
'6336050_grdc',
'6336500_grdc',
'6336800_grdc',
'6336900_grdc',
'6337100_grdc',
'6337200_grdc',
'6337250_grdc',
'6337400_grdc',
'6337501_grdc',
'6337507_grdc',
'6337514_grdc',
'6337515_grdc',
'6337516_grdc',
'6337517_grdc',
'6337518_grdc',
'6337519_grdc',
'6338100_grdc',
'6338110_grdc',
'6338130_grdc',
'6338140_grdc',
'6340110_grdc',
'6340120_grdc',
'6340130_grdc',
'6340140_grdc',
'6340150_grdc',
'6340160_grdc',
'6340170_grdc',
'6340180_grdc',
'6340190_grdc',
'6340300_grdc',
'6340500_grdc',
'6340510_grdc',
'6340600_grdc',
'6340610_grdc',
'6340620_grdc',
'6340625_grdc',
'6342110_grdc',
'6342201_grdc',
'6342500_grdc',
'6342501_grdc',
'6342510_grdc',
'6342512_grdc',
'6342513_grdc',
'6342525_grdc',
'6342600_grdc',
'6342610_grdc',
'6342620_grdc',
'6342800_grdc',
'6342900_grdc',
'6342910_grdc',
'6342920_grdc',
'6342925_grdc',
'6343100_grdc',
'6343500_grdc',
'6343510_grdc',
'6343900_grdc',
'6348200_grdc',
'6348201_grdc',
'6348300_grdc',
'6348350_grdc',
'6348600_grdc',
'6348610_grdc',
'6348800_grdc',
'6349200_grdc',
'6357010_grdc',
'6357500_grdc',
'6372100_grdc',
'6373010_grdc',
'6373020_grdc',
'6373040_grdc',
'6373101_grdc',
'6373102_grdc',
'6373219_grdc',
'6373300_grdc',
'6373304_grdc',
'6373307_grdc',
'6373400_grdc',
'6373430_grdc',
'6373910_grdc',
'6401070_grdc',
'6401080_grdc',
'6401090_grdc',
'6401111_grdc',
'6401120_grdc',
'6401130_grdc',
'6401140_grdc',
'6401150_grdc',
'6401200_grdc',
'6401250_grdc',
'6401440_grdc',
'6401500_grdc',
'6401610_grdc',
'6401702_grdc',
'6401703_grdc',
'6401800_grdc',
'6421101_grdc',
'6421102_grdc',
'6421501_grdc',
'6421900_grdc',
'6435060_grdc',
'6442450_grdc',
'6442500_grdc',
'6444090_grdc',
'6444100_grdc',
'6444310_grdc',
'6444380_grdc',
'6444600_grdc',
'6457010_grdc',
'6457100_grdc',
'6457200_grdc',
'6457205_grdc',
'6457660_grdc',
'6457670_grdc',
'6457680_grdc',
'6457690_grdc',
'6457700_grdc',
'6457800_grdc',
'6457810_grdc',
'6457820_grdc',
'6457830_grdc',
'6457840_grdc',
'6457850_grdc',
'6457870_grdc',
'6457890_grdc',
'6458010_grdc',
'6458402_grdc',
'6458404_grdc',
'6458406_grdc',
'6458450_grdc',
'6458460_grdc',
'6458500_grdc',
'6458550_grdc',
'6458600_grdc',
'6458810_grdc',
'6458924_grdc',
'6502101_grdc',
'6502151_grdc',
'6503120_grdc',
'6503121_grdc',
'6503151_grdc',
'6503201_grdc',
'6503280_grdc',
'6503290_grdc',
'6503291_grdc',
'6503300_grdc',
'6503301_grdc',
'6503350_grdc',
'6503351_grdc',
'6503352_grdc',
'6503851_grdc',
'6503852_grdc',
'6542100_grdc',
'6542200_grdc',
'6544100_grdc',
'6545050_grdc',
'6545100_grdc',
'6545101_grdc',
'6545102_grdc',
'6545150_grdc',
'6545190_grdc',
'6545200_grdc',
'6545500_grdc',
'6545501_grdc',
'6545700_grdc',
'6546610_grdc',
'6546802_grdc',
'6547500_grdc',
'6559100_grdc',
'6559101_grdc',
'6559110_grdc',
'6559180_grdc',
'6573102_grdc',
'6573434_grdc',
'6573800_grdc',
'6574011_grdc',
'6574150_grdc',
'6574152_grdc',
'6574156_grdc',
'6574203_grdc',
'6574351_grdc',
'6574352_grdc',
'6574360_grdc',
'6574362_grdc',
'6574364_grdc',
'6590700_grdc',
'6603120_grdc',
'6603300_grdc',
'6604201_grdc',
'6604310_grdc',
'6604360_grdc',
'6604450_grdc',
'6605300_grdc',
'6605560_grdc',
'6605570_grdc',
'6605590_grdc',
'6605615_grdc',
'6605690_grdc',
'6605695_grdc',
'6605750_grdc',
'6605780_grdc',
'6605800_grdc',
'6605815_grdc',
'6605820_grdc',
'6605830_grdc',
'6605950_grdc',
'6606310_grdc',
'6606400_grdc',
'6606401_grdc',
'6606600_grdc',
'6606650_grdc',
'6606655_grdc',
'6606701_grdc',
'6608210_grdc',
'6608501_grdc',
'6609500_grdc',
'6609510_grdc',
'6642200_grdc',
'6644200_grdc',
'6658100_grdc',
'6667100_grdc',
'6687900_grdc',
'6688150_grdc',
'6688650_grdc',
'6690100_grdc',
'6691500_grdc',
'6691650_grdc',
'6695250_grdc',
'6695400_grdc',
'6729142_grdc',
'6729260_grdc',
'6729317_grdc',
'6729371_grdc',
'6729401_grdc',
'6729402_grdc',
'6729450_grdc',
'6729455_grdc',
'6729530_grdc',
'6729595_grdc',
'6730100_grdc',
'6730330_grdc',
'6730500_grdc',
'6730501_grdc',
'6731020_grdc',
'6731137_grdc',
'6731501_grdc',
'6731515_grdc',
'6731555_grdc',
'6731601_grdc',
'6731680_grdc',
'6731693_grdc',
'6731907_grdc',
'6731910_grdc',
'6731920_grdc',
'6733570_grdc',
'6742200_grdc',
'6742201_grdc',
'6742400_grdc',
'6742451_grdc',
'6742452_grdc',
'6742500_grdc',
'6742552_grdc',
'6742600_grdc',
'6742700_grdc',
'6742701_grdc',
'6742900_grdc',
'6742912_grdc',
'6744200_grdc',
'6744201_grdc',
'6744500_grdc',
'6744600_grdc',
'6830100_grdc',
'6830102_grdc',
'6830104_grdc',
'6830510_grdc',
'6830511_grdc',
'6830512_grdc',
'6832750_grdc',
'6832752_grdc',
'6832754_grdc',
'6832755_grdc',
'6832907_grdc',
'6832909_grdc',
'6832917_grdc',
'6842200_grdc',
'6842400_grdc',
'6842700_grdc',
'6842800_grdc',
'6842900_grdc',
'6854101_grdc',
'6854105_grdc',
'6854203_grdc',
'6854320_grdc',
'6854400_grdc',
'6854500_grdc',
'6854590_grdc',
'6854591_grdc',
'6854592_grdc',
'6854600_grdc',
'6854602_grdc',
'6854620_grdc',
'6854700_grdc',
'6854701_grdc',
'6854702_grdc',
'6854703_grdc',
'6854704_grdc',
'6854705_grdc',
'6854706_grdc',
'6854707_grdc',
'6854708_grdc',
'6854709_grdc',
'6854710_grdc',
'6854712_grdc',
'6854713_grdc',
'6854715_grdc',
'6854718_grdc',
'6854720_grdc',
'6854800_grdc',
'6854802_grdc',
'6854900_grdc',
'6854902_grdc',
'6855400_grdc',
'6855401_grdc',
'6855409_grdc',
'6855480_grdc',
'6864800_grdc',
'6865600_grdc',
'6871100_grdc',
'6887300_grdc',
'6887350_grdc',
'6887400_grdc',
'6890150_grdc',
'6934100_grdc',
'6935020_grdc',
'6935051_grdc',
'6935052_grdc',
'6935053_grdc',
'6935054_grdc',
'6935055_grdc',
'6935145_grdc',
'6935146_grdc',
'6935300_grdc',
'6935301_grdc',
'6935302_grdc',
'6935310_grdc',
'6935313_grdc',
'6935314_grdc',
'6935400_grdc',
'6935401_grdc',
'6935500_grdc',
'6935600_grdc',
'6939050_grdc',
'6939200_grdc',
'6939500_grdc',
'6939550_grdc',
'6943100_grdc',
'6948100_grdc',
'6948120_grdc',
'6955430_grdc',
'6970100_grdc',
'6970105_grdc',
'6970250_grdc',
'6970400_grdc',
'6970600_grdc',
'6970630_grdc',
'6970700_grdc',
'6970705_grdc',
'6970757_grdc',
'6970808_grdc',
'6970809_grdc',
'6970893_grdc',
'6971100_grdc',
'6971130_grdc',
'6971150_grdc',
'6971200_grdc',
'6971401_grdc',
'6971451_grdc',
'6971500_grdc',
'6971550_grdc',
'6971600_grdc',
'6971710_grdc',
'6972300_grdc',
'6975050_grdc',
'6975100_grdc',
'6976700_grdc',
'6976800_grdc',
'6978250_grdc',
'6978251_grdc',
'6983350_grdc',
'6983750_grdc',
'6983800_grdc',
'6984500_grdc',
'6984800_grdc',
'6987100_grdc']

#p = ['1134200_grdc','1134650_grdc']
def exportation (gauge, buffer):
	id = gauge
	loc = grdc.filter(ee.Filter.eq('Sttn_Nm', id))
	#loc = grdc.filter(ee.Filter.eq('Sttn_Nm',id))
	filt_gauge = loc
	xsections = sword.filterBounds(loc.geometry().buffer(buffer))
	#record = records.filter(ee.Filter.eq('Sttn_Nm', id)).filter(ee.Filter.notNull(['value'])).filter(ee.Filter.gt('value', 0))

	#print('size', xsections.size())
	#Map.addLayer(xsections.limit(10))

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




	def ExtractChannel(image):
		# // extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
		connectedToCl = (image.Not().cumulativeCost(
			source = ee.Image().toByte().paint(grwl_cline, 1).And(image), #// only use the centerline that overlaps with the water mask
			maxDistance = 4000,
			geodeticDistance = False).eq(0))

		channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask'])
		return(channel)



	def yrFun(f):
	  a = f.get('variable')
	  return(f.set({'Date':a}))

	#record = record.map(yrFun)

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
	bn = ['B2', 'B3', 'B4', 'B8','B11','B12','QA10','SCL']
	collection = LT5.merge(LE7).merge(LC8)
	collection = collection.filterBounds(xsections)
	collection = collection.filterDate('1984-01-01','2021-12-31')

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


	collection = collection.merge(sent)
	# #record = record.filter(ee.Filter.gte('value', 0)).filter()
	# def img(f):
	  # a = f.get('Date')
	  # b = f.get('value')
	  # c = ee.Image.constant(b)
	  # return(ee.Image(c).set({'Date': a, 'Q':b,'system:id':ee.String(a), 'ID':ee.String(a)}))

	# #imgColl = record.map(img)
	# toyFilter = ee.Filter.equals(
	  # leftField= 'ID',
	  # rightField= 'ID'
	# )
	# # Define the join.
	# innerJoin = ee.Join.inner('primary', 'secondary')
	# # Apply the join.
	# toyJoin = innerJoin.apply(collection,imgColl, toyFilter)

	# def func_isq(f):
	  # a = f.get('secondary')
	  # a = ee.Image(a).get('Q')
	  # b = f.get('primary')
	  # c = ee.Image(b).set({'Q':a})
	  # return(c)

	# combined = toyJoin.map(func_isq)

	# def func_elp(f):
	  # a = ee.Image(f)
	  # b = a.clip(xsections.geometry())
	  # return(b)

	#filtered = combined.map(func_elp)


	#/Maybe keep??
	filtered = collection
	sent = filtered.filter(ee.Filter.eq('area', 100))
	#print(sent.first())
	filtered = filtered.filter(ee.Filter.eq('area', 900))



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

	flagged = ee.ImageCollection(filtered).map(AddFmaskSR)


	def sentinelCloud(f):
		temp = f.select('BQA').rename('fmask')
		water = temp.where(temp.gt(6).Or(temp.eq(3)), ee.Image(7))
		all = water.where(water.lte(6), ee.Image(1))
		fmask = all.where(all.eq(7), ee.Image(2))
		return f.addBands(fmask)
	
	sent = ee.ImageCollection(sent).map(sentinelCloud)
	flagged = flagged.merge(sent)

	xsections = ee.FeatureCollection(xsections)

	waterMask = flagged.map(ClassifyWaterZou2018)#(x.ClassifyWaterJones2019)

	filt = xsections
	#Determine connectivity of water pixels to grwl centerline and filter out any water pixels more than 4km away from centerline.
	def GetCenterline(clDataset, bound):
		# // filter the GRWL centerline based on area of interest
		cl = clDataset.filterBounds(bound)
		return(cl)
	#riverFun.GetCenterline(grwl_cline, xsections)
	riverMask = waterMask.map(ExtractChannel)
	toyFilter = ee.Filter.equals(
	  leftField= 'system:index',
	  rightField= 'system:index'
	)
	# Define the join.
	innerJoin = ee.Join.inner('primary', 'secondary')
	# Apply the join.
	toyJoin = innerJoin.apply(riverMask,flagged, toyFilter)
	
	def combination (f):
		a = f.get('secondary')
		a = ee.Image(a)
		b = f.get('primary')
		c = ee.Image(b).addBands(a)
		return(c)
		
	combined = toyJoin.map(combination)


	def line2pt (f):
		f = f.set({'lineGeometry': f.geometry()})
		l = f.geometry().coordinates()
		g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
		return(f.setGeometry(g))

	toyFilter = ee.Filter.equals(
		  leftField= 'node_id',
		  rightField= 'node_id'
		)
	innerJoin = ee.Join.inner('primary', 'secondary');
	def jng(f):
		a = f.get('primary')
		b = f.get('secondary')
		c = ee.Feature(a).copyProperties(b)
		return(c.set({'cloud':c.get('mean')}))


	def jng1(f):
	    a = f.get('primary')
	    b = f.get('secondary')
	    c = ee.Feature(a).copyProperties(b)
	    return(c.set({'width':c.get('mean'), 'length':ee.Feature(b).geometry().length()}))







	def processing(f):
		time = f.get('system:time_start')
		cld = ee.Image(f).select('fmask').gt(1)
		min = cld
		min = min.reduceRegions(xsections,ee.Reducer.mean())
		flags = ee.Image(f).select('channelMask').reduceRegions(xsections.map(line2pt),ee.Reducer.max().combine(ee.Reducer.count(),"", True))
		wd = ee.Image(f).select('channelMask').eq(1).reduceRegions(xsections,ee.Reducer.mean())
		toyJoin = innerJoin.apply(flags,min, toyFilter);
		combined = toyJoin.map(jng)
		secondJoin = innerJoin.apply(combined, ee.FeatureCollection(wd), toyFilter).map(jng1)
		def tm (f):
			a = f.set({'system:time_start':time, 'Sttn_Nm':id})
			return(a)
		return(secondJoin.map(tm))
		

	outputs = ee.ImageCollection(combined).map(processing)
	##Filtering makes it slower. 
	outputs = outputs.flatten()#.filter(ee.Filter.notNull(['width', 'cloud'])).filter(ee.Filter.lte('cloud', .25)).filter(ee.Filter.eq('count', 2)).filter(ee.Filter.eq('max',0)).filter(ee.Filter.gt('width', 0))
	return(outputs)#).filter(ee.Filter.notNull(['width', 'cloud'])))

for i in range(len(p)):
	out = exportation(p[i],2000)
	task = (ee.batch.Export.table.toDrive(
	collection = out,
	description = 'widths_' + '_' + str(p[i]),
	folder = 'GRDC_xsection_widths_updated',
	fileNamePrefix = 'Gauge_' + '_' + str(p[i]),
	fileFormat = 'csv'
	))

	task.start()
	#print(output.first())
	print('task', p[i], 'has started')
	maximum_no_of_tasks(10,90)



