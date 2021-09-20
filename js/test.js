var grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
//var extraFunction = function(ryan){
var x = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_waterClassification_Jones2019.js')
var fls = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js');
var flsh = require('users/eeProject/RivWidthCloudPaper:rwc_landsat.js');
var fnsLandsat = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js');
var lsFun = require('users/eeProject/RivWidthCloudPaper:functions_Landsat578/functions_landsat.js');
var riverFun = require('users/eeProject/RivWidthCloudPaper:functions_river.js');
var grwl_cline = ee.FeatureCollection('users/eeProject/GRWL_summaryStats')
var id = "4127800_grdc"
var loc = grdc.filter(ee.Filter.eq('Sttn_Nm', id))
var filt_gauge = loc
var xsections = sword.filterBounds(loc.geometry().buffer(2000))
print('size', xsections.size())
Map.addLayer(xsections.limit(10))

//Calculate distance of water pixels from GRWL centerline for filtering out non river pixels. 
riverFun.ExtractChannel = function(image) {
  // extract the channel water bodies from the water mask, based on connectivity to the reference centerline.
  var connectedToCl = image.not().cumulativeCost({
    source: ee.Image().toByte().paint(grwl_cline, 1).and(image), // only use the centerline that overlaps with the water mask
    maxDistance: 4000,
    geodeticDistance: false
  }).eq(0);
  var channel = image.updateMask(connectedToCl).unmask(0).updateMask(image.gte(0)).rename(['channelMask']);
  //Map.addLayer(channel, {}, 'river mask/channel')
  return (channel);
};

var record = records.filter(ee.Filter.eq('Sttn_Nm', id)).filter(ee.Filter.notNull(['value'])).filter(ee.Filter.gt('value', 0))

var yrFun = function(f){
  var a = f.get('variable')
  return(f.set({'Date':a}))
}
var record = record.map(yrFun)

// Match common names to the sensor-specific bands:
var LT5_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
var LE7_BANDS = ['B1',   'B2',    'B3',  'B4',  'B5',    'B7',    'B6', 'pixel_qa'];
var LC8_BANDS = ['B2',   'B3',    'B4',  'B5',  'B6',    'B7',    'B10', 'pixel_qa'];
var STD_NAMES = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'Temp', 'BQA'];

// load Landsat 5,7,8 collections:
// TODO(GHA): combine 5, 7, and 8 collections:
var LT5 = ee.ImageCollection('LANDSAT/LT5_L1T_SR')
    .select(LT5_BANDS, STD_NAMES);
var LT5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
    .select(LT5_BANDS, STD_NAMES); 
var LE7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
.select(LE7_BANDS, STD_NAMES).filterDate('1999-01-01', '2003-05-30');
var LC8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
   .select(LC8_BANDS, STD_NAMES);
var bn = ['B2', 'B3', 'B4', 'B8','B11','B12','QA10','QA60']
var collection = LT5.merge(LE7).merge(LC8);
var collection = collection.filterBounds(xsections)
var collection = collection.filterDate('2010-01-01','2021-12-31')

/*
var sent = ee.ImageCollection("COPERNICUS/S2_SR").filterBounds(xsections)
var sent = sent.select(bn, STD_NAMES)
var sent = sent.map(function(f){
  var a = f.get('system:time_start')
  var b = ee.Date(a).format("yyyy-MM-dd")
  return(f.set({'system:id': b, 'ID':b, 'area': 100}))
})
*/

var collection = collection.map(function(f){
  var a = f.get('system:time_start')
  var b = ee.Date(a).format("yyyy-MM-dd")
  return(f.set({'system:id': b, 'ID':b, 'area': 900}))
})
var collection = collection//.merge(sent)
//var record = record.filter(ee.Filter.gte('value', 0)).filter()
var img = function(f){
  var a = f.get('Date')
  var b = f.get('value')
  var c = ee.Image.constant(b)
  return(ee.Image(c).set({'Date': a, 'Q':b,'system:id':ee.String(a), 'ID':ee.String(a)}))
}
var imgColl = record.map(img)
var toyFilter = ee.Filter.equals({
  leftField: 'ID',
  rightField: 'ID'
});
// Define the join.
var innerJoin = ee.Join.inner('primary', 'secondary');
// Apply the join.
var toyJoin = innerJoin.apply(collection,imgColl, toyFilter);
var combined = toyJoin.map(function(f){
  var a = f.get('secondary')
  var a = ee.Image(a).get('Q')
  var b = f.get('primary')
  var c = ee.Image(b).set({'Q':a})
  return(c)
})

var filtered = combined.map(function(f){
  var a = ee.Image(f)
  var b = a.clip(xsections.geometry())
  return(b)
})


///Maybe keep??
var filtered = combined
//var sent = filtered.filter(ee.Filter.eq('area', 100))
//print(sent.first())
//var filtered = filtered.filter(ee.Filter.eq('area', 900))



//Add Fmask values to the images for cloud filtering. 
var Unpack = function(qualityBand, startingBit, bitWidth) {
  // unpacking bit information from the quality band given the bit position and width
  // see: https://groups.google.com/forum/#!starred/google-earth-engine-developers/iSV4LwzIW7A
  return(qualityBand
  .rightShift(startingBit)
  .bitwiseAnd(ee.Number(2).pow(bitWidth).subtract(1).int()));
};
var UnpackAllSR = function(bitBand) {
  // apply Unpack function for multiple pixel qualities
  var bitInfoSR = {
    'Cloud': [5, 1],
    'CloudShadow': [3, 1], 
    'SnowIce': [4, 1],
    'Water': [2, 1]
  };
  var unpackedImage = ee.Image();
  for (var key in bitInfoSR) {
    unpackedImage = ee.Image.cat(
      unpackedImage, Unpack(bitBand, bitInfoSR[key][0], bitInfoSR[key][1])
      .rename(key));
  }
  return(unpackedImage.select(Object.keys(bitInfoSR)));
};
var AddFmaskSR = function(image) {
  // add fmask as a separate band to the input image
  var temp = UnpackAllSR(image.select(['BQA']));

  // construct the fmask
  var fmask = (temp.select(['Water']).rename(['fmask'])
  .where(temp.select(['SnowIce']), ee.Image(3))
  .where(temp.select(['CloudShadow']), ee.Image(2))
  .where(temp.select(['Cloud']), ee.Image(4)))
  .mask(temp.select(['Cloud']).gte(0)); // mask the fmask so that it has the same footprint as the quality (BQA) band

  return(image.addBands(fmask));
};
var flagged = ee.ImageCollection(filtered).map(AddFmaskSR)


var sentinelCloud = function(f){
  var temp = f.select('BQA').rename('fmask')
  var temp = temp.eq(3).or(temp.eq(7)).or(temp.eq(8)).or(temp.eq(9)).or(temp.eq(10))
  var fmask = temp.where(temp.eq(7), ee.Image(2))
  .where(temp.eq(11), ee.Image(3))
  .where(temp.eq(8).or(temp.eq(9).or(temp.eq(10))), ee.Image(4))
  //.mask(temp.eq(8).or(temp.eq(9).or(temp.eq(10))).gte(0))
  var fmask = fmask.where(fmask.eq(1), ee.Image(2))
  return(f.addBands(fmask))
}
//var sent = ee.ImageCollection(sent).map(sentinelCloud)
var flagged = flagged//.merge(sent)

var running = function(xsections){
var xsections = ee.FeatureCollection(xsections)

var cloudFunction =function(f){
  var cld = f.select('fmask').gt(2)
  var min = cld//.mask(cld)
  var min = min.reduceRegion({geometry: xsections,reducer: ee.Reducer.mean()})
  var add = f.set({'cloud': ee.Number(min.get('fmask')).multiply(100)})
  return(add)
}

var filtered= flagged.filterBounds(xsections).map(cloudFunction)
var filtered = filtered.filter(ee.Filter.notNull(['cloud'])).filter(ee.Filter.lte('cloud', 10))

var waterJones = function(f){
  var img = x.ClassifyWaterJones2019(f)
  var out = img.copyProperties(f)
  return(out.set({'system:time_start': f.get('system:time_start')}))
}
var waterMask = filtered.map(waterJones)//(x.ClassifyWaterJones2019)

var filt = xsections
//Determine connectivity of water pixels to grwl centerline and filter out any water pixels more than 4km away from centerline. 
riverFun.GetCenterline = function(clDataset, bound) {
  // filter the GRWL centerline based on area of interest
  var cl = clDataset.filterBounds(bound); 
  return(cl);
};
riverFun.GetCenterline(grwl_cline, xsections)
var riverMask = waterMask.map(riverFun.ExtractChannel)

var line2pt =  function(f){
        var f = f.set({'lineGeometry': f.geometry()})
        var l = f.geometry().coordinates()
        var g = ee.Geometry.MultiPoint(l, 'EPSG:4326')
        return(f.setGeometry(g))
}

var flagging = riverMask.map(function(x){
  var flags = x.reduceRegions(xsections.map(line2pt),ee.Reducer.max().combine(ee.Reducer.count(),"", true))
  var flags = flags.first()
  var flagsMax = flags.get('max')
  var pts = flags.get('count')
  return(x.set({'flags':flagsMax, 'endPoints':pts}))
})
var flaggingFiltered = flagging.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))
var widths = flaggingFiltered.map(function(x){
  var wd = x.eq(1).reduceRegion({geometry: xsections,reducer: ee.Reducer.mean()})
  var length = xsections.geometry().length()
  var wd = ee.Number(wd.get('channelMask')).multiply(ee.Number(length))
  return(x.set({'width':wd}))
})

var widths = widths.filter(ee.Filter.gt('width', 0))//.filter(ee.Filter.eq('endPoints', 2)).filter(ee.Filter.eq('flags',0))
var final_filtered = widths.map(function(f){
  var a = f.get('Q')
  var b = ee.Number(a).round()
  return(f.set({'Q': b}))
})

var random = final_filtered.randomColumn()
var training = random.filter(ee.Filter.gte('random', .3))
var testing = random.filter(ee.Filter.lt('random', .3))

var lr = training.reduceColumns(ee.Reducer.linearFit(), ['width','Q'])

var b = lr.get('offset')//.errorMatrix('Q', 'classification').accuracy()
var slope = lr.get('scale')

var r = testing.map(function(x){
  var wd = ee.Number(x.get('width')).multiply(ee.Number(slope))
  var wd = wd.add(b)
  return(x.set({'classification':ee.Number(wd)}))
})

var actual = testing.aggregate_array('Q')
var mean = actual.reduce(ee.Reducer.mean())

var sqr = r.map(function(f){
  var model = f.get('classification')
  var actual = f.get('Q')
  var error = ee.Number(model).subtract(ee.Number(actual))
  var rel = ee.Number(error).divide(ee.Number(mean))
  var sqr = ee.Number(rel).pow(2)
  return(f.set({'sqr': sqr}))
})

var meanSqr = sqr.aggregate_array('sqr').reduce(ee.Reducer.mean())
var rrmse = ee.Number(meanSqr).sqrt()
var rrmse = ee.Number(rrmse).multiply(100)
var out = xsections.set({'rrmse':rrmse.round()})

var nse1 = r.map(function(f){
  var model = f.get('classification')
  var actual = f.get('Q')
  var error = ee.Number(model).subtract(ee.Number(actual))
  var error = ee.Number(error).pow(2)
  return(f.set({'error': error}))
})

var nse2 = r.map(function(f){
  var model = f.get('classification')
  var actual = f.get('Q')
  var rel = ee.Number(actual).subtract(ee.Number(mean))
  var rel = ee.Number(rel).pow(2)
  return(f.set({'rel': rel}))
})  
var meanNSE1 = nse1.aggregate_array('error').reduce(ee.Reducer.mean())
var meanNSE2 = nse2.aggregate_array('rel').reduce(ee.Reducer.mean())
var nse = ee.Number(meanNSE1).divide(ee.Number(meanNSE2))
var nse = ee.Number(1).subtract(nse)
return(xsections.first().set({'rrmse':out.get('rrmse'),'nse':nse, 'slope':slope, 'yInt':b}))
}

var run = xsections.limit(1).map(running)
print('run', run)

//Map.addLayer(run.sort('rrmse'))




