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
	
	
p = ['4213563_grdc', 
'4213566_grdc', 
'4213569_grdc', 
'4213570_grdc', 
'4213573_grdc', 
'4213575_grdc', 
'4213650_grdc', 
'4213680_grdc', 
'4213681_grdc', 
'4213682_grdc', 
'4213683_grdc', 
'4213684_grdc', 
'4213710_grdc', 
'4213711_grdc', 
'4213712_grdc', 
'4213730_grdc', 
'4213740_grdc', 
'4213800_grdc', 
'4213801_grdc', 
'4213802_grdc', 
'4213803_grdc', 
'4213804_grdc', 
'4213810_grdc', 
'4213905_grdc', 
'4214005_grdc', 
'4214010_grdc', 
'4214025_grdc', 
'4214036_grdc', 
'4214040_grdc', 
'4214050_grdc', 
'4214051_grdc', 
'4214060_grdc', 
'4214090_grdc', 
'4214100_grdc', 
'4214105_grdc', 
'4214106_grdc', 
'4214260_grdc', 
'4214261_grdc', 
'4214262_grdc', 
'4214270_grdc', 
'4214275_grdc', 
'4214298_grdc', 
'4214300_grdc', 
'4214310_grdc', 
'4214350_grdc', 
'4214351_grdc', 
'4214360_grdc', 
'4214390_grdc', 
'4214440_grdc', 
'4214450_grdc', 
'4214480_grdc', 
'4214490_grdc', 
'4214510_grdc', 
'4214515_grdc', 
'4214517_grdc', 
'4214519_grdc', 
'4214520_grdc', 
'4214530_grdc', 
'4214531_grdc', 
'4214540_grdc', 
'4214550_grdc', 
'4214551_grdc', 
'4214555_grdc', 
'4214560_grdc', 
'4214570_grdc', 
'4214580_grdc', 
'4214590_grdc', 
'4214610_grdc', 
'4214620_grdc', 
'4214621_grdc', 
'4214650_grdc', 
'4214655_grdc', 
'4214670_grdc', 
'4214680_grdc', 
'4214690_grdc', 
'4214701_grdc', 
'4214750_grdc', 
'4214760_grdc', 
'4214770_grdc', 
'4214775_grdc', 
'4214800_grdc', 
'4214801_grdc', 
'4214810_grdc', 
'4214830_grdc', 
'4214900_grdc', 
'4214920_grdc', 
'4214940_grdc', 
'4214943_grdc', 
'4214950_grdc', 
'4215200_grdc', 
'4215210_grdc', 
'4215220_grdc', 
'4215260_grdc', 
'4215320_grdc', 
'4215425_grdc', 
'4215426_grdc', 
'4215660_grdc', 
'4215670_grdc', 
'4215700_grdc', 
'4215705_grdc', 
'4215706_grdc', 
'4220500_grdc', 
'4220501_grdc', 
'4231200_grdc', 
'4231250_grdc', 
'4231600_grdc', 
'4231601_grdc', 
'4231602_grdc', 
'4231610_grdc', 
'4231620_grdc', 
'4231621_grdc', 
'4231630_grdc', 
'4231650_grdc', 
'4232700_grdc', 
'4232750_grdc', 
'4235100_grdc', 
'4235101_grdc', 
'4235300_grdc', 
'4235301_grdc', 
'4236010_grdc', 
'4236600_grdc', 
'4236700_grdc', 
'4243050_grdc', 
'4243080_grdc', 
'4243100_grdc', 
'4243101_grdc', 
'4243150_grdc', 
'4243151_grdc', 
'4243215_grdc', 
'4243230_grdc', 
'4243240_grdc', 
'4243270_grdc', 
'4243280_grdc', 
'4243290_grdc', 
'4243300_grdc', 
'4243301_grdc', 
'4243315_grdc', 
'4243320_grdc', 
'4243330_grdc', 
'4243340_grdc', 
'4243360_grdc', 
'4244370_grdc', 
'4244410_grdc', 
'4244430_grdc', 
'4244450_grdc', 
'4244460_grdc', 
'4244500_grdc', 
'4244550_grdc', 
'4244580_grdc', 
'4244600_grdc', 
'4244605_grdc', 
'4244610_grdc', 
'4244620_grdc', 
'4244630_grdc', 
'4244635_grdc', 
'4244640_grdc', 
'4244655_grdc', 
'4244730_grdc', 
'4244760_grdc', 
'4244770_grdc', 
'4244788_grdc', 
'4244795_grdc', 
'4244820_grdc', 
'4244840_grdc', 
'4244850_grdc', 
'4244870_grdc', 
'4244880_grdc', 
'4244890_grdc', 
'4244915_grdc', 
'4244940_grdc', 
'4244945_grdc', 
'4245100_grdc', 
'4245250_grdc', 
'4245500_grdc', 
'4245920_grdc', 
'4245950_grdc', 
'4247100_grdc', 
'4247101_grdc', 
'4281600_grdc', 
'4352100_grdc', 
'4353300_grdc', 
'4355080_grdc', 
'4355100_grdc', 
'4355200_grdc', 
'4355300_grdc', 
'4355310_grdc', 
'4355350_grdc', 
'4355400_grdc', 
'4358202_grdc', 
'4358210_grdc', 
'4358220_grdc', 
'4358300_grdc', 
'4358500_grdc', 
'4358800_grdc', 
'4362201_grdc', 
'4362600_grdc', 
'4462300_grdc', 
'4462500_grdc', 
'4462700_grdc', 
'4664800_grdc', 
'4772210_grdc', 
'4772250_grdc', 
'4773800_grdc', 
'4873450_grdc', 
'4876800_grdc', 
'4997050_grdc', 
'5222500_grdc', 
'5223100_grdc', 
'5224500_grdc', 
'5224600_grdc', 
'5225500_grdc', 
'5225600_grdc', 
'5226800_grdc', 
'5230200_grdc', 
'5230300_grdc', 
'5230500_grdc', 
'5231500_grdc', 
'5231550_grdc', 
'5231600_grdc', 
'5231700_grdc', 
'5654140_grdc', 
'5654190_grdc', 
'5654300_grdc', 
'5654340_grdc', 
'5654500_grdc', 
'5654550_grdc', 
'5654600_grdc', 
'5657500_grdc', 
'5660110_grdc', 
'5660500_grdc', 
'6111100_grdc', 
'6112090_grdc', 
'6113100_grdc', 
'6113110_grdc', 
'6114300_grdc', 
'6116100_grdc', 
'6119010_grdc', 
'6122100_grdc', 
'6122110_grdc', 
'6122300_grdc', 
'6123100_grdc', 
'6123300_grdc', 
'6123400_grdc', 
'6123710_grdc', 
'6125360_grdc', 
'6128100_grdc', 
'6128701_grdc', 
'6128702_grdc', 
'6136200_grdc', 
'6139284_grdc', 
'6139370_grdc', 
'6139390_grdc', 
'6139391_grdc', 
'6139400_grdc', 
'6139500_grdc', 
'6139501_grdc', 
'6139502_grdc', 
'6139682_grdc', 
'6139770_grdc', 
'6139790_grdc', 
'6139960_grdc', 
'6140250_grdc', 
'6140300_grdc', 
'6140400_grdc', 
'6140401_grdc', 
'6142110_grdc', 
'6142120_grdc', 
'6142150_grdc', 
'6142200_grdc', 
'6142551_grdc', 
'6142620_grdc', 
'6142660_grdc', 
'6142680_grdc', 
'6144200_grdc', 
'6144300_grdc', 
'6157100_grdc', 
'6158100_grdc', 
'6172050_grdc', 
'6172052_grdc', 
'6172200_grdc', 
'6172350_grdc', 
'6210410_grdc', 
'6211050_grdc', 
'6211100_grdc', 
'6211150_grdc', 
'6211500_grdc', 
'6211520_grdc', 
'6212400_grdc', 
'6212410_grdc', 
'6212430_grdc', 
'6212440_grdc', 
'6212450_grdc', 
'6212460_grdc', 
'6212470_grdc', 
'6212500_grdc', 
'6212505_grdc', 
'6212510_grdc', 
'6212515_grdc', 
'6212740_grdc', 
'6212750_grdc', 
'6212810_grdc', 
'6212820_grdc', 
'6212840_grdc', 
'6213500_grdc', 
'6213520_grdc', 
'6213530_grdc', 
'6213600_grdc', 
'6213650_grdc', 
'6213700_grdc', 
'6213750_grdc', 
'6213770_grdc', 
'6213780_grdc', 
'6213800_grdc', 
'6213850_grdc', 
'6213900_grdc', 
'6216500_grdc', 
'6216510_grdc', 
'6216610_grdc', 
'6216630_grdc', 
'6217100_grdc', 
'6217110_grdc', 
'6217120_grdc', 
'6217135_grdc', 
'6220201_grdc', 
'6220204_grdc', 
'6220210_grdc', 
'6221100_grdc', 
'6221101_grdc', 
'6221102_grdc', 
'6221630_grdc', 
'6226200_grdc', 
'6226300_grdc', 
'6226310_grdc', 
'6226400_grdc', 
'6226430_grdc', 
'6226550_grdc', 
'6226600_grdc', 
'6226650_grdc', 
'6226800_grdc', 
'6227510_grdc', 
'6228800_grdc', 
'6228810_grdc', 
'6228820_grdc', 
'6232900_grdc', 
'6232901_grdc', 
'6232910_grdc', 
'6232911_grdc', 
'6232913_grdc', 
'6232915_grdc', 
'6232930_grdc', 
'6233170_grdc', 
'6233201_grdc', 
'6233203_grdc', 
'6233221_grdc', 
'6233315_grdc', 
'6233401_grdc', 
'6233465_grdc', 
'6233501_grdc', 
'6233502_grdc', 
'6233510_grdc', 
'6233521_grdc', 
'6233523_grdc', 
'6233551_grdc', 
'6233650_grdc', 
'6233680_grdc', 
'6233710_grdc', 
'6233750_grdc', 
'6233754_grdc', 
'6233780_grdc', 
'6233800_grdc', 
'6233850_grdc', 
'6235530_grdc', 
'6235535_grdc', 
'6242100_grdc', 
'6242150_grdc', 
'6242180_grdc', 
'6242250_grdc', 
'6242300_grdc', 
'6242400_grdc', 
'6242401_grdc', 
'6242630_grdc', 
'6242700_grdc', 
'6242800_grdc', 
'6242820_grdc', 
'6242830_grdc', 
'6243030_grdc', 
'6243050_grdc', 
'6243245_grdc', 
'6243400_grdc', 
'6243450_grdc', 
'6243850_grdc', 
'6246400_grdc', 
'6246590_grdc', 
'6246600_grdc', 
'6246601_grdc', 
'6246610_grdc', 
'6246611_grdc', 
'6246612_grdc', 
'6258226_grdc', 
'6261100_grdc', 
'6261150_grdc', 
'6273302_grdc', 
'6273303_grdc', 
'6273614_grdc', 
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
'6335602_grdc', 
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
'6340500_grdc', 
'6340510_grdc', 
'6340600_grdc', 
'6340610_grdc', 
'6340620_grdc', 
'6340625_grdc', 
'6342201_grdc', 
'6342500_grdc', 
'6342501_grdc', 
'6342510_grdc', 
'6342512_grdc', 
'6342513_grdc', 
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
'6349600_grdc', 
'6357010_grdc', 
'6357500_grdc', 
'6372100_grdc', 
'6373010_grdc', 
'6373020_grdc', 
'6373040_grdc', 
'6373102_grdc', 
'6373219_grdc', 
'6373300_grdc', 
'6373304_grdc', 
'6373307_grdc', 
'6373400_grdc', 
'6373430_grdc', 
'6395100_grdc', 
'6401070_grdc', 
'6401080_grdc', 
'6401090_grdc', 
'6401111_grdc', 
'6401120_grdc', 
'6401130_grdc', 
'6401150_grdc', 
'6401200_grdc', 
'6401250_grdc', 
'6401440_grdc', 
'6401500_grdc', 
'6401610_grdc', 
'6401701_grdc', 
'6401702_grdc', 
'6401703_grdc', 
'6401800_grdc', 
'6421102_grdc', 
'6421501_grdc', 
'6421900_grdc', 
'6435060_grdc', 
'6438500_grdc', 
'6442450_grdc', 
'6442500_grdc', 
'6444100_grdc', 
'6444310_grdc', 
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
'6457840_grdc', 
'6457850_grdc', 
'6457870_grdc', 
'6457890_grdc', 
'6458010_grdc', 
'6458404_grdc', 
'6458406_grdc', 
'6458450_grdc', 
'6458460_grdc', 
'6458500_grdc', 
'6458550_grdc', 
'6458600_grdc', 
'6458810_grdc', 
'6458924_grdc', 
'6502100_grdc', 
'6502101_grdc', 
'6502151_grdc', 
'6503121_grdc', 
'6503150_grdc', 
'6503151_grdc', 
'6503200_grdc', 
'6503201_grdc', 
'6503280_grdc', 
'6503291_grdc', 
'6503300_grdc', 
'6503301_grdc', 
'6503350_grdc', 
'6503351_grdc', 
'6503850_grdc', 
'6503851_grdc', 
'6503852_grdc', 
'6542200_grdc', 
'6542600_grdc', 
'6544100_grdc', 
'6545050_grdc', 
'6545100_grdc', 
'6545101_grdc', 
'6545102_grdc', 
'6545190_grdc', 
'6545200_grdc', 
'6545500_grdc', 
'6545501_grdc', 
'6546610_grdc', 
'6546801_grdc', 
'6546802_grdc', 
'6547500_grdc', 
'6559100_grdc', 
'6559101_grdc', 
'6559180_grdc', 
'6573102_grdc', 
'6573434_grdc', 
'6574011_grdc', 
'6574150_grdc', 
'6574152_grdc', 
'6574156_grdc', 
'6574351_grdc', 
'6574352_grdc', 
'6574362_grdc', 
'6590700_grdc', 
'6603120_grdc', 
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
'6644200_grdc', 
'6644250_grdc', 
'6658100_grdc', 
'6660300_grdc', 
'6660350_grdc', 
'6660480_grdc', 
'6681300_grdc', 
'6687900_grdc', 
'6688150_grdc', 
'6688650_grdc', 
'6690100_grdc', 
'6695200_grdc', 
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
'6742450_grdc', 
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
'6887400_grdc', 
'6890150_grdc', 
'6930170_grdc', 
'6935020_grdc', 
'6935050_grdc', 
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
'6935400_grdc', 
'6935401_grdc', 
'6935500_grdc', 
'6939050_grdc', 
'6939200_grdc', 
'6939500_grdc', 
'6943100_grdc', 
'6948100_grdc', 
'6948120_grdc', 
'6955430_grdc', 
'6970100_grdc', 
'6970110_grdc', 
'6970115_grdc', 
'6970125_grdc', 
'6970135_grdc', 
'6970136_grdc', 
'6970138_grdc', 
'6970141_grdc', 
'6970145_grdc', 
'6970146_grdc', 
'6970166_grdc', 
'6970175_grdc', 
'6970180_grdc', 
'6970250_grdc', 
'6970252_grdc', 
'6970270_grdc', 
'6970272_grdc', 
'6970273_grdc', 
'6970275_grdc', 
'6970325_grdc', 
'6970400_grdc', 
'6970401_grdc', 
'6970402_grdc', 
'6970410_grdc', 
'6970420_grdc', 
'6970450_grdc', 
'6970458_grdc', 
'6970460_grdc', 
'6970466_grdc', 
'6970470_grdc', 
'6970480_grdc', 
'6970502_grdc', 
'6970505_grdc', 
'6970521_grdc', 
'6970561_grdc', 
'6970583_grdc', 
'6970630_grdc', 
'6970680_grdc', 
'6970682_grdc', 
'6970684_grdc', 
'6970700_grdc', 
'6970702_grdc', 
'6970703_grdc', 
'6970704_grdc', 
'6970705_grdc', 
'6970720_grdc', 
'6970722_grdc', 
'6970735_grdc', 
'6970745_grdc', 
'6970751_grdc', 
'6970775_grdc', 
'6970802_grdc', 
'6970803_grdc', 
'6970807_grdc', 
'6970808_grdc', 
'6970809_grdc', 
'6970810_grdc', 
'6970813_grdc', 
'6970825_grdc', 
'6970831_grdc', 
'6970832_grdc', 
'6970850_grdc', 
'6970856_grdc', 
'6970861_grdc', 
'6970862_grdc', 
'6970863_grdc', 
'6970870_grdc', 
'6970871_grdc', 
'6970874_grdc', 
'6970875_grdc', 
'6970876_grdc', 
'6970880_grdc', 
'6970881_grdc', 
'6970890_grdc', 
'6970893_grdc', 
'6970895_grdc', 
'6970900_grdc', 
'6970902_grdc', 
'6970910_grdc', 
'6971100_grdc', 
'6971130_grdc', 
'6971137_grdc', 
'6971140_grdc', 
'6971150_grdc', 
'6971151_grdc', 
'6971200_grdc', 
'6971310_grdc', 
'6971400_grdc', 
'6971401_grdc', 
'6971405_grdc', 
'6971435_grdc', 
'6971440_grdc', 
'6971445_grdc', 
'6971450_grdc', 
'6971452_grdc', 
'6971500_grdc', 
'6971600_grdc', 
'6971710_grdc', 
'6972300_grdc', 
'6975080_grdc', 
'6976700_grdc', 
'6978250_grdc', 
'6978251_grdc', 
'6983350_grdc', 
'6983750_grdc', 
'6983800_grdc', 
'6984500_grdc', 
'6984800_grdc']

#p = ['1134200_grdc','1134650_grdc']
def exportation (gauge, buffer):
	id = gauge
	loc = grdc.filter(ee.Filter.eq('Sttn_Nm', id))
	#loc = grdc.filter(ee.Filter.eq('Sttn_Nm',id))
	filt_gauge = loc
	xsections = sword.filterBounds(loc.geometry().buffer(buffer))
	record = records.filter(ee.Filter.eq('Sttn_Nm', id)).filter(ee.Filter.notNull(['value'])).filter(ee.Filter.gt('value', 0))

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





	collection = collection#.merge(sent)
	#record = record.filter(ee.Filter.gte('value', 0)).filter()
	def img(f):
	  a = f.get('Date')
	  b = f.get('value')
	  c = ee.Image.constant(b)
	  return(ee.Image(c).set({'Date': a, 'Q':b,'system:id':ee.String(a), 'ID':ee.String(a)}))

	imgColl = record.map(img)
	toyFilter = ee.Filter.equals(
	  leftField= 'ID',
	  rightField= 'ID'
	)
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

	#filtered = combined.map(func_elp)


	#/Maybe keep??
	filtered = combined
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
	  temp = temp.eq(3).Or(temp.eq(7)).Or(temp.eq(8)).Or(temp.eq(9)).Or(temp.eq(10))
	  fmask = temp.where(temp.eq(7), ee.Image(2)).where(temp.eq(11), ee.Image(3)).where(temp.eq(8).Or(temp.eq(9).Or(temp.eq(10))), ee.Image(4))
	  fmask = fmask.where(fmask.eq(1), ee.Image(2))
	  return(f.addBands(fmask))
	sent = ee.ImageCollection(sent).map(sentinelCloud)
	flagged = flagged.merge(sent)

	xsections = ee.FeatureCollection(xsections)

	waterMask = filtered.map(ClassifyWaterZou2018)#(x.ClassifyWaterJones2019)

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
	toyJoin = innerJoin.apply(collection,imgColl, toyFilter)
	
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

	def processing = function(f){
		cld = ee.Image(f).select('fmask').gt(2)
		min = cld//.mask(cld)
		min = min.reduceRegions(xsections,ee.Reducer.mean())
		flags = ee.Image(f).select('channelMask').reduceRegions(xsections.map(line2pt),ee.Reducer.max().combine(ee.Reducer.count(),"", true))
		flags = flags.first()
		flagsMax = flags.get('max')
		pts = flags.get('count')
		wd = ee.Image(f).select('channelMask').eq(1).reduceRegion({geometry: xsections,reducer: ee.Reducer.mean()})
		length = xsections.geometry().length()
		q = f.get('Q')
		t = f.get('system:time_start')
		def setting (l):
		    return(l.set({'Q':q, 'system:time_start':t, 'cloud':ee.Number(l.get('mean')), 'width':wd.get('channelMask'),'length':length, 'endPoints':pts,'flags':flagsMax}))})
		x = min.map(setting)
	return(x)


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	def func_lrw(x):
	  flags = x.reduceRegions(
	  collection = xsections.map(line2pt),
	  reducer = ee.Reducer.max().combine(ee.Reducer.count(),"", True))
	  flags = flags.first()
	  flagsMax = flags.get('max')
	  pts = flags.get('count')
	  return(x.set({'flags':flagsMax, 'endPoints':pts}))

	flagging = riverMask.map(func_lrw)
	flaggingFiltered = flagging.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))

	def func_pvd(x):
	  wd = x.eq(1).reduceRegion(geometry= xsections, 
	  reducer= ee.Reducer.mean())
	  length = xsections.geometry().length()
	  wd = ee.Number(wd.get('channelMask')).multiply(ee.Number(length))
	  return(x.set({'width':wd}))

	widths = flaggingFiltered.map(func_pvd)
	widths = widths.filter(ee.Filter.gt('width', 0)).filter(ee.Filter.notNull(['width', 'Q'])).filter(ee.Filter.gte('Q',0))#.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))

	def func_bjh(f):
	  a = f.get('Q')
	  b = ee.Number(a).round()
	  return(f.set({'Q': b}))

	final_filtered = ee.FeatureCollection(widths).map(func_bjh)
	random = ee.FeatureCollection(final_filtered).randomColumn('random',7,'uniform')
	training = ee.FeatureCollection(random).filter(ee.Filter.gte('random', .3))
	testing = ee.FeatureCollection(random).filter(ee.Filter.lt('random', .3))
	# lr = ee.FeatureCollection(training).reduceColumns(ee.Reducer.linearFit(), ['width','Q'])
	# b = lr.get('offset')#.errorMatrix('Q', 'classification').accuracy()
	# slope = lr.get('scale')

	# def func_tnf(x):
	  # wd = ee.Number(x.get('width')).multiply(ee.Number(slope))
	  # wd = wd.add(b)
	  # return(x.set({'classification':ee.Number(wd)}))

	# r = ee.FeatureCollection(testing).map(func_tnf)
	# r = r.filter(ee.Filter.gte('classification',0))#.filter(ee.Filter.notNull['classification'])
	#classifier = ee.Classifier.smileRandomForest(training.size()).train(training,'Q',['width'])
	classifier = ee.Algorithms.If(training.size().gte(2).And(training.distinct('Q').size().gte(2)),ee.Classifier.smileRandomForest(training.size()).train(training,'Q',['width']), None) 
	#r = testing.classify(classifier)
	def fill (f):
	  a = f.set({'classifier':-9999})
	  return(a)
	r = ee.Algorithms.If(training.size().gte(2).And(training.distinct('Q').size().gte(2)), testing.classify(classifier),ee.FeatureCollection(testing).map(fill))

	r = ee.FeatureCollection(r)
	r = r.filter(ee.Filter.notNull(['Q','classification','width'])).filter(ee.Filter.gte('classification',0))
	
	actual = testing.aggregate_array('Q')
	mean = actual.reduce(ee.Reducer.mean())

	# def func_rdt(f):
	  # model = f.get('classification')
	  # actual = f.get('Q')
	  # error = ee.Number(model).subtract(ee.Number(actual))
	  # rel = ee.Number(error).divide(ee.Number(mean))
	  # sqr = ee.Number(rel).pow(2)
	  # #return(f.set({'sqr': sqr}))
	  # return(ee.Algorithms.If(error,f.set({'sqr': sqr}), f.set({'sqr': None})))

	# sqr = r.map(func_rdt)
	# meanSqr = sqr.aggregate_array('sqr').reduce(ee.Reducer.mean())
	# rrmse = ee.Number(meanSqr).sqrt()
	# rrmse = ee.Number(rrmse).multiply(100)
	# out = xsections.set({'rrmse':rrmse})

	def func_wnu(f):
	  model = f.get('classification')
	  actual = f.get('Q')
	  error = ee.Number(model).subtract(ee.Number(actual))
	  error = ee.Number(error).pow(2)
	  #return(f.set({'error': error}))
	  return(ee.Algorithms.If(error,f.set({'error': error}), f.set({'error': None})))

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
	#nse = ee.Number(meanNSE1).divide(ee.Number(meanNSE2))
	#nse = ee.Algorithms.If(meanNSE1, ee.Number(meanNSE1).divide(ee.Number(meanNSE2)), 9999)

	#nse = ee.Number(1).subtract(nse)
	nse = ee.Algorithms.If(meanNSE1, ee.Number(1).subtract(ee.Number(meanNSE1).divide(ee.Number(meanNSE2))), -9999)
	nse = ee.Algorithms.If(meanNSE2, nse, -9999)
	output=xsections.first().set({'Sttn_Nm':id,'nse':nse, 'buffer':ee.Number(buffer), 'testing':testing.size(), 'training':training.size()})#, 'slope':slope, 'yInt':b})
	return(ee.Algorithms.If(training.size().gte(2),output, xsections.first().set({'Sttn_Nm':id,'nse':-9999,'buffer':ee.Number(buffer), 'testing':testing.size(), 'training':training.size()})))
run = xsections.map(running)
out = run.sort('nse', False)
out = out.limit(10)
#out = ee.Algorithms.If(ee.Number(out.first().get('nse')).lt(0), exec(open('E:/research/RatingCurveAnalysis/src/outerBuffer.py').read()),out)
return(out)


# together = ee.List(p).slice(0,3,1).map(exportation,2000)
# task = (ee.batch.Export.table.toAsset(together, 'all', 'users/rriggs/testRun'))
# task.start()

# def all(f):
  # out = exportation(f,2000)
  # return(ee.Algorithms.If(ee.Number(out.first().get('nse')).lt(0).And(out.size().eq(0)), exportation(f,10000), out))

# gauges = ee.FeatureCollection('users/rriggs/GSIM_plus_India')
# gaugesFilt = gauges.filter(ee.Filter.stringContains('Sttn_Nm','grdc'))

# results = gaugesFilt.limit(10).map(all)
# task = (ee.batch.Export.table.toAsset(ee.FeatureCollection(results.flatten()), 'testingAll', 'users/rriggs/testRun'))
# task.start()




for i in range(len(p)):
	out = exportation(p[i],2000)
	out = ee.Algorithms.If(ee.Number(out.first().get('nse')).lt(0), exportation(p[i],5000), out)

	
	
	
	
	task = (ee.batch.Export.table.toDrive(
	collection = out,
	description = 'widths_' + '_' + str(p[i]),
	folder = 'GRDC_RandomForestRC_performance',
	fileNamePrefix = 'Gauge_' + '_' + str(p[i]),
	fileFormat = 'csv'
	))

	task.start()
	#print(output.first())
	print('task', p[i], 'has started')
	maximum_no_of_tasks(10,30)



