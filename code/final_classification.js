// classify land use in Latvia with LUCAS (2012) data

// SET UP DATA ----
// choose bands 
var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7'];

// import all points of known land-use type from LUCAS 
var fc_total  = ee.FeatureCollection('users/izzyrich/full_2012');
var forestry  = ee.FeatureCollection('users/izzyrich/forestry');
var abandoned  = ee.FeatureCollection('users/izzyrich/abandoned');
var extensive  = ee.FeatureCollection('users/izzyrich/extensive');
var intensive  = ee.FeatureCollection('users/izzyrich/intensive');
var water  = ee.FeatureCollection('users/izzyrich/water');
var wetlands  = ee.FeatureCollection('users/izzyrich/wetlands');
var artificial  = ee.FeatureCollection('users/izzyrich/artificial');

// function to clour correct
var cloudMaskL457 = function(image) {
  var qa = image.select('pixel_qa');
  // If the cloud bit (5) is set and the cloud confidence (7) is high
  // or the cloud shadow bit is set (3), then it's a bad pixel.
  var cloud = qa.bitwiseAnd(1 << 5)
          .and(qa.bitwiseAnd(1 << 7))
          .or(qa.bitwiseAnd(1 << 3))
  // Remove edge pixels that don't occur in all bands
  var mask2 = image.mask().reduce(ee.Reducer.min());
  return image.updateMask(cloud.not()).updateMask(mask2);
};

// import shape of Latvia polygon to define ROI
var latvia_poly = ee.FeatureCollection('users/izzyrich/latvia_poly');

// MAKE GRID FOR Latvia

// 1) Create bounding box 
var lon_start = 20.97139;
var lon_end = 29.24051;
var lat_start = 55.66372; 
var lat_end = 58.08577;

// 2) Decide no. of (in this case: equally sized) cells
var num_cells = 200;
var lon_edge = (lon_end-lon_start)/Math.sqrt(num_cells);
var lat_edge = (lat_end-lat_start)/Math.sqrt(num_cells);

// 3) Create the grid
var polys = [];
var polys_line = [];
var cell_id = 0;
for (var lon = lon_start; lon < lon_end; lon += lon_edge) {
  var x1 = lon;
  var x2 = lon + lon_edge;
  for (var lat = lat_start; lat < lat_end; lat += lat_edge) {
    cell_id = cell_id + 1;
    var y1 = lat;
    var y2 = lat + lat_edge;

    polys.push(ee.Feature(ee.Geometry.Rectangle(x1, y1, x2, y2), {label: cell_id}));
  }
}
var grid = ee.FeatureCollection(polys);
var filtered = grid.filterBounds(latvia_poly);
Map.addLayer(filtered);

// add buffer of 90 metres to create polygons
var fc_total = fc_total.map(function(f) {
  return f.buffer(90);
});

var forestry = forestry.map(function(f) {
  return f.buffer(90);
});


var abandoned = abandoned.map(function(f) {
  return f.buffer(90);
});


var extensive = extensive.map(function(f) {
  return f.buffer(90);
});

var intensive = intensive.map(function(f) {
  return f.buffer(90);
});

var water = water.map(function(f) {
  return f.buffer(90);
});


var wetlands = wetlands.map(function(f) {
  return f.buffer(90);
});

var artificial = artificial.map(function(f) {
  return f.buffer(90);
});

// add layer to map
Map.addLayer(fc_total, {}, 'total');

// add satellite imagery for 2011 - surface reflectance
var landsatCollection = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2011-06-01', '2011-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median = landsatCollection.map(cloudMaskL457).median();

// clip on size of latvia 
// var clipped = median.clip(grid);

var clipped = median.clip(latvia_poly);

// visualise Landsat imagery
Map.addLayer(clipped, {bands: ['B3', 'B2', 'B1']}, 'L7 Image', false);
//Map.setCenter(24.11, 56.948);

// START CLASSIFICATION ----

// set random seed
var n = 0;

// get random columns for each land-use type
var randomForestry = forestry.randomColumn('random', n);
var randomAbandoned = abandoned.randomColumn('random', n);
var randomExtensive = extensive.randomColumn('random', n);
var randomIntensive = intensive.randomColumn('random', n);
var randomWater = water.randomColumn('random', n);
var randomWetlands = wetlands.randomColumn('random', n);
var randomArtificial = artificial.randomColumn('random', n);

// split up data for testing and training - 80% for training and 20% for testing
var split = 0.8;

// stratified training and testing samples
var trainingSample = randomForestry.filter(ee.Filter.lt('random', split))
  .merge(randomAbandoned.filter(ee.Filter.lt('random', split)))
  .merge(randomExtensive.filter(ee.Filter.lt('random', split)))
  .merge(randomIntensive.filter(ee.Filter.lt('random', split)))
  .merge(randomWater.filter(ee.Filter.lt('random', split)))
  .merge(randomWetlands.filter(ee.Filter.lt('random', split)))
  .merge(randomArtificial.filter(ee.Filter.lt('random', split)));
  
var testingSample = randomForestry.filter(ee.Filter.gte('random', split))
  .merge(randomAbandoned.filter(ee.Filter.gte('random', split)))
  .merge(randomExtensive.filter(ee.Filter.gte('random', split)))
  .merge(randomIntensive.filter(ee.Filter.gte('random', split)))
  .merge(randomWater.filter(ee.Filter.gte('random', split)))
  .merge(randomWetlands.filter(ee.Filter.gte('random', split)))
  .merge(randomArtificial.filter(ee.Filter.gte('random', split)));

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

print(trainingSample, 'trainingSample');

// trained with 80% of our data
var trainedClassifier = ee.Classifier.randomForest({
  numberOfTrees: 30
})
  .train(training, 'class');

// classify FeatureCollection
var classified = clipped.classify(trainedClassifier, 'classification');

// Define a palette for the Land Use classification.
var palette = ['FF7f0E', // 0, forestry, orange
'8C564B', // 1, abandoned, brown
'2CA02C', // 2, extensive, green
'9467BD', // 3, intensive, purple
'1F77B4', // 4, water, blue
'E377C2', // 5, wetlands, pink
'7F7F7F', // 6, artificial, grey
];

// Display the classification result and the input image.
//Map.setCenter(24.11,56.948);
// Map.addLayer(classified, {min: 0, max: 6, palette: palette}, 'Classification');

// Get a confusion matrix representing resubstitution accuracy.
 print('RF error matrix: ', trainedClassifier.confusionMatrix());
 print('RF accuracy: ', trainedClassifier.confusionMatrix().accuracy()); 

// Sample input to get validation data
var validation = clipped.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated = validation.classify(trainedClassifier)


// Get error of testing data + export to table to save time
var testError = validated.errorMatrix('class', 'classification');
var exportconfusionMatrix = ee.Feature(null, {matrix: testError.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix),
  description: 'exportconfusionMatrix_2011',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy = testError.accuracy(); 
var exporttestAccuracy = ee.Feature(null, {matrix: testAccuracy});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy),
  description: 'testAccuracy_2011',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified,
  description: 'landusemap2011',
  scale: 30
});

// Make image into feature collection
var fcclassified = ee.FeatureCollection([classified]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified.select(['classification']),
  description: 'classified',
  assetId: 'classified',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified,
  description:'landusemap2011',
  fileFormat: 'KML'
});

///// 1989 ///////
// add satellite imagery for 1989 - surface reflectance
var landsatCollection_1989 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1989-06-01', '1989-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1989 = landsatCollection_1989.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1989 = median_1989.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1989.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1989
var classified_1989 = clipped_1989.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1989 = clipped_1989.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1989 = validation_1989.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1989 = validated_1989.errorMatrix('class', 'classification');
var exportconfusionMatrix_1989 = ee.Feature(null, {matrix: testError_1989.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1989),
  description: 'exportconfusionMatrix_1989',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1989 = testError_1989.accuracy(); 
var exporttestAccuracy_1989 = ee.Feature(null, {matrix: testAccuracy_1989});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1989),
  description: 'testAccuracy_1989',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1989,
  description: 'landusemap1989',
  scale: 100
});

// Make image into feature collection
var fcclassified_1989 = ee.FeatureCollection([classified_1989]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1989.select(['classification']),
  description: 'classified_1989',
  assetId: 'classified_1989',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1989,
  description:'landusemap1989',
  fileFormat: 'KML'
});

///// 1990 ///////
// add satellite imagery for 1990 - surface reflectance
var landsatCollection_1990 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1990-06-01', '1990-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1990 = landsatCollection_1990.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1990 = median_1990.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1990.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1990
var classified_1990 = clipped_1990.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1990 = clipped_1990.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1990 = validation_1990.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1990 = validated_1990.errorMatrix('class', 'classification');
var exportconfusionMatrix_1990 = ee.Feature(null, {matrix: testError_1990.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1990),
  description: 'exportconfusionMatrix_1990',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1990 = testError_1990.accuracy(); 
var exporttestAccuracy_1990 = ee.Feature(null, {matrix: testAccuracy_1990});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1990),
  description: 'testAccuracy_1990',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1990,
  description: 'landusemap1990',
  scale: 100
});

// Make image into feature collection
var fcclassified_1990 = ee.FeatureCollection([classified_1990]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1990.select(['classification']),
  description: 'classified_1990',
  assetId: 'classified_1990',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1990,
  description:'landusemap1990',
  fileFormat: 'KML'
});


///// 1991 ///////
// add satellite imagery for 1991 - surface reflectance
var landsatCollection_1991 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1991-06-01', '1991-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1991 = landsatCollection_1991.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1991 = median_1991.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1991.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1991
var classified_1991 = clipped_1991.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1991 = clipped_1991.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1991 = validation_1991.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1991 = validated_1991.errorMatrix('class', 'classification');
var exportconfusionMatrix_1991 = ee.Feature(null, {matrix: testError_1991.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1991),
  description: 'exportconfusionMatrix_1991',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1991 = testError_1991.accuracy(); 
var exporttestAccuracy_1991 = ee.Feature(null, {matrix: testAccuracy_1991});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1991),
  description: 'testAccuracy_1991',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1991,
  description: 'landusemap1991',
  scale: 100
});

// Make image into feature collection
var fcclassified_1991 = ee.FeatureCollection([classified_1991]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1991.select(['classification']),
  description: 'classified_1991',
  assetId: 'classified_1991',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1991,
  description:'landusemap1991',
  fileFormat: 'KML'
});

///// 1992 ///////
// add satellite imagery for 1992 - surface reflectance
var landsatCollection_1992 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1992-06-01', '1992-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1992 = landsatCollection_1992.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1992 = median_1992.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1992.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1992
var classified_1992 = clipped_1992.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1992 = clipped_1992.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1992 = validation_1992.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1992 = validated_1992.errorMatrix('class', 'classification');
var exportconfusionMatrix_1992 = ee.Feature(null, {matrix: testError_1992.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1992),
  description: 'exportconfusionMatrix_1992',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1992 = testError_1992.accuracy(); 
var exporttestAccuracy_1992 = ee.Feature(null, {matrix: testAccuracy_1992});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1992),
  description: 'testAccuracy_1992',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1992,
  description: 'landusemap1992',
  scale: 100
});

// Make image into feature collection
var fcclassified_1992 = ee.FeatureCollection([classified_1992]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1992.select(['classification']),
  description: 'classified_1992',
  assetId: 'classified_1992',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1992,
  description:'landusemap1992',
  fileFormat: 'KML'
});

///// 1993 ///////
// add satellite imagery for 1993 - surface reflectance
var landsatCollection_1993 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1993-06-01', '1993-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1993 = landsatCollection_1993.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1993 = median_1993.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1993.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1993
var classified_1993 = clipped_1993.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1993 = clipped_1993.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1993 = validation_1993.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1993 = validated_1993.errorMatrix('class', 'classification');
var exportconfusionMatrix_1993 = ee.Feature(null, {matrix: testError_1993.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1993),
  description: 'exportconfusionMatrix_1993',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1993 = testError_1993.accuracy(); 
var exporttestAccuracy_1993 = ee.Feature(null, {matrix: testAccuracy_1993});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1993),
  description: 'testAccuracy_1993',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1993,
  description: 'landusemap1993',
  scale: 100
});

// Make image into feature collection
var fcclassified_1993 = ee.FeatureCollection([classified_1993]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1993.select(['classification']),
  description: 'classified_1993',
  assetId: 'classified_1993',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1993,
  description:'landusemap1993',
  fileFormat: 'KML'
});

///// 1994 ///////
// add satellite imagery for 1994 - surface reflectance
var landsatCollection_1994 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1994-06-01', '1994-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1994 = landsatCollection_1994.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1994 = median_1994.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1994.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1994
var classified_1994 = clipped_1994.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1994 = clipped_1994.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1994 = validation_1994.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1994 = validated_1994.errorMatrix('class', 'classification');
var exportconfusionMatrix_1994 = ee.Feature(null, {matrix: testError_1994.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1994),
  description: 'exportconfusionMatrix_1994',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1994 = testError_1994.accuracy(); 
var exporttestAccuracy_1994 = ee.Feature(null, {matrix: testAccuracy_1994});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1994),
  description: 'testAccuracy_1994',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1994,
  description: 'landusemap1994',
  scale: 100
});

// Make image into feature collection
var fcclassified_1994 = ee.FeatureCollection([classified_1994]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1994.select(['classification']),
  description: 'classified_1994',
  assetId: 'classified_1994',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1994,
  description:'landusemap1994',
  fileFormat: 'KML'
});

///// 1995 ///////
// add satellite imagery for 1995 - surface reflectance
var landsatCollection_1995 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1995-06-01', '1995-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1995 = landsatCollection_1995.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1995 = median_1995.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1995.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1991
var classified_1995 = clipped_1995.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1995 = clipped_1995.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1995 = validation_1995.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1995 = validated_1995.errorMatrix('class', 'classification');
var exportconfusionMatrix_1995 = ee.Feature(null, {matrix: testError_1995.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1995),
  description: 'exportconfusionMatrix_1995',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1995 = testError_1995.accuracy(); 
var exporttestAccuracy_1995 = ee.Feature(null, {matrix: testAccuracy_1995});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1995),
  description: 'testAccuracy_1995',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1995,
  description: 'landusemap1995',
  scale: 100
});

// Make image into feature collection
var fcclassified_1995 = ee.FeatureCollection([classified_1995]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1995.select(['classification']),
  description: 'classified_1995',
  assetId: 'classified_1995',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1995,
  description:'landusemap1995',
  fileFormat: 'KML'
});

///// 1996 ///////
// add satellite imagery for 1996 - surface reflectance
var landsatCollection_1996 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1996-06-01', '1996-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1996 = landsatCollection_1996.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1996 = median_1996.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1996.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1996
var classified_1996 = clipped_1996.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1996 = clipped_1996.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1996 = validation_1996.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1996 = validated_1996.errorMatrix('class', 'classification');
var exportconfusionMatrix_1996 = ee.Feature(null, {matrix: testError_1996.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1996),
  description: 'exportconfusionMatrix_1996',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1996 = testError_1996.accuracy(); 
var exporttestAccuracy_1996 = ee.Feature(null, {matrix: testAccuracy_1996});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1996),
  description: 'testAccuracy_1996',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1996,
  description: 'landusemap1996',
  scale: 100
});

// Make image into feature collection
var fcclassified_1996 = ee.FeatureCollection([classified_1996]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1996.select(['classification']),
  description: 'classified_1996',
  assetId: 'classified_1996',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1996,
  description:'landusemap1996',
  fileFormat: 'KML'
});

///// 1997 ///////
// add satellite imagery for 1997 - surface reflectance
var landsatCollection_1997 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1997-06-01', '1997-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1997 = landsatCollection_1997.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1997 = median_1997.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1997.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1997
var classified_1997 = clipped_1997.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1997 = clipped_1997.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1997 = validation_1997.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1997 = validated_1997.errorMatrix('class', 'classification');
var exportconfusionMatrix_1997 = ee.Feature(null, {matrix: testError_1997.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1997),
  description: 'exportconfusionMatrix_1997',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1997 = testError_1997.accuracy(); 
var exporttestAccuracy_1997 = ee.Feature(null, {matrix: testAccuracy_1997});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1997),
  description: 'testAccuracy_1997',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1997,
  description: 'landusemap1997',
  scale: 100
});

// Make image into feature collection
var fcclassified_1997 = ee.FeatureCollection([classified_1997]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1997.select(['classification']),
  description: 'classified_1997',
  assetId: 'classified_1997',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1997,
  description:'landusemap1997',
  fileFormat: 'KML'
});

///// 1998 ///////
// add satellite imagery for 1998 - surface reflectance
var landsatCollection_1998 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1998-06-01', '1998-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1998 = landsatCollection_1998.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1998 = median_1998.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1998.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1998
var classified_1998 = clipped_1998.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1998 = clipped_1998.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1998 = validation_1998.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1998 = validated_1998.errorMatrix('class', 'classification');
var exportconfusionMatrix_1998 = ee.Feature(null, {matrix: testError_1998.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1998),
  description: 'exportconfusionMatrix_1998',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1998 = testError_1998.accuracy(); 
var exporttestAccuracy_1998 = ee.Feature(null, {matrix: testAccuracy_1998});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1998),
  description: 'testAccuracy_1998',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1998,
  description: 'landusemap1998',
  scale: 100
});

// Make image into feature collection
var fcclassified_1998 = ee.FeatureCollection([classified_1998]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1998.select(['classification']),
  description: 'classified_1998',
  assetId: 'classified_1998',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1998,
  description:'landusemap1998',
  fileFormat: 'KML'
});

///// 1999 ///////
// add satellite imagery for 1999 - surface reflectance
var landsatCollection_1999 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('1999-06-01', '1999-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_1999 = landsatCollection_1999.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_1999 = median_1999.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_1999.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 1999
var classified_1999 = clipped_1999.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_1999 = clipped_1999.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_1999 = validation_1999.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_1999 = validated_1999.errorMatrix('class', 'classification');
var exportconfusionMatrix_1999 = ee.Feature(null, {matrix: testError_1999.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_1999),
  description: 'exportconfusionMatrix_1999',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_1999 = testError_1999.accuracy(); 
var exporttestAccuracy_1999 = ee.Feature(null, {matrix: testAccuracy_1999});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_1999),
  description: 'testAccuracy_1999',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_1999,
  description: 'landusemap1999',
  scale: 100
});

// Make image into feature collection
var fcclassified_1999 = ee.FeatureCollection([classified_1999]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_1999.select(['classification']),
  description: 'classified_1999',
  assetId: 'classified_1999',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_1999,
  description:'landusemap1999',
  fileFormat: 'KML'
});

///// 2000 ///////
// add satellite imagery for 2000 - surface reflectance
var landsatCollection_2000 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2000-06-01', '2000-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2000 = landsatCollection_2000.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2000 = median_2000.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2000.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2000
var classified_2000 = clipped_2000.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2000 = clipped_2000.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2000 = validation_2000.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2000 = validated_2000.errorMatrix('class', 'classification');
var exportconfusionMatrix_2000 = ee.Feature(null, {matrix: testError_2000.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2000),
  description: 'exportconfusionMatrix_2000',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2000 = testError_2000.accuracy(); 
var exporttestAccuracy_2000 = ee.Feature(null, {matrix: testAccuracy_2000});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2000),
  description: 'testAccuracy_2000',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2000,
  description: 'landusemap2000',
  scale: 100
});

// Make image into feature collection
var fcclassified_2000 = ee.FeatureCollection([classified_2000]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2000.select(['classification']),
  description: 'classified_2000',
  assetId: 'classified_2000',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2000,
  description:'landusemap2000',
  fileFormat: 'KML'
});


///// 2001 ///////
// add satellite imagery for 2001 - surface reflectance
var landsatCollection_2001 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2001-06-01', '2001-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2001 = landsatCollection_2001.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2001 = median_2001.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2001.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2001
var classified_2001 = clipped_2001.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2001 = clipped_2001.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2001 = validation_2001.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2001 = validated_2001.errorMatrix('class', 'classification');
var exportconfusionMatrix_2001 = ee.Feature(null, {matrix: testError_2001.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2001),
  description: 'exportconfusionMatrix_2001',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2001 = testError_2001.accuracy(); 
var exporttestAccuracy_2001 = ee.Feature(null, {matrix: testAccuracy_2001});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2001),
  description: 'testAccuracy_2001',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2001,
  description: 'landusemap2001',
  scale: 100
});

// Make image into feature collection
var fcclassified_2001 = ee.FeatureCollection([classified_2001]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2001.select(['classification']),
  description: 'classified_2001',
  assetId: 'classified_2001',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2001,
  description:'landusemap2001',
  fileFormat: 'KML'
});

///// 2002 ///////
// add satellite imagery for 2002 - surface reflectance
var landsatCollection_2002 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2002-06-01', '2002-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2002 = landsatCollection_2002.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2002 = median_2002.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2002.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2002
var classified_2002 = clipped_2002.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2002 = clipped_2002.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2002 = validation_2002.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2002 = validated_2002.errorMatrix('class', 'classification');
var exportconfusionMatrix_2002 = ee.Feature(null, {matrix: testError_2002.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2002),
  description: 'exportconfusionMatrix_2002',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2002 = testError_2002.accuracy(); 
var exporttestAccuracy_2002 = ee.Feature(null, {matrix: testAccuracy_2002});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2002),
  description: 'testAccuracy_2002',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2002,
  description: 'landusemap2002',
  scale: 100
});

// Make image into feature collection
var fcclassified_2002 = ee.FeatureCollection([classified_2002]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2002.select(['classification']),
  description: 'classified_2002',
  assetId: 'classified_2002',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2002,
  description:'landusemap2002',
  fileFormat: 'KML'
});

///// 2003 ///////
// add satellite imagery for 2003 - surface reflectance
var landsatCollection_2003 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2003-06-01', '2003-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2003 = landsatCollection_2003.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2003 = median_2003.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2003.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2003
var classified_2003 = clipped_2003.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2003 = clipped_2003.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2003 = validation_2003.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2003 = validated_2003.errorMatrix('class', 'classification');
var exportconfusionMatrix_2003 = ee.Feature(null, {matrix: testError_2003.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2003),
  description: 'exportconfusionMatrix_2003',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2003 = testError_2003.accuracy(); 
var exporttestAccuracy_2003 = ee.Feature(null, {matrix: testAccuracy_2003});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2003),
  description: 'testAccuracy_2003',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2003,
  description: 'landusemap2003',
  scale: 100
});

// Make image into feature collection
var fcclassified_2003 = ee.FeatureCollection([classified_2003]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2003.select(['classification']),
  description: 'classified_2003',
  assetId: 'classified_2003',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2003,
  description:'landusemap2003',
  fileFormat: 'KML'
});

///// 2004 ///////
// add satellite imagery for 2004 - surface reflectance
var landsatCollection_2004 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2004-06-01', '2004-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2004 = landsatCollection_2004.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2004 = median_2004.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2004.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2004
var classified_2004 = clipped_2004.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2004 = clipped_2004.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2004 = validation_2004.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2004 = validated_2004.errorMatrix('class', 'classification');
var exportconfusionMatrix_2004 = ee.Feature(null, {matrix: testError_2004.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2004),
  description: 'exportconfusionMatrix_2004',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2004 = testError_2004.accuracy(); 
var exporttestAccuracy_2004 = ee.Feature(null, {matrix: testAccuracy_2004});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2004),
  description: 'testAccuracy_2004',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2004,
  description: 'landusemap2004',
  scale: 100
});

// Make image into feature collection
var fcclassified_2004 = ee.FeatureCollection([classified_2004]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2004.select(['classification']),
  description: 'classified_2004',
  assetId: 'classified_2004',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2004,
  description:'landusemap2004',
  fileFormat: 'KML'
});

///// 2005 ///////
// add satellite imagery for 2005 - surface reflectance
var landsatCollection_2005 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2005-06-01', '2005-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2005 = landsatCollection_2005.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2005 = median_2005.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2005.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2005
var classified_2005 = clipped_2005.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2005 = clipped_2005.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2005 = validation_2005.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2005 = validated_2005.errorMatrix('class', 'classification');
var exportconfusionMatrix_2005 = ee.Feature(null, {matrix: testError_2005.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2005),
  description: 'exportconfusionMatrix_2005',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2005 = testError_2005.accuracy(); 
var exporttestAccuracy_2005 = ee.Feature(null, {matrix: testAccuracy_2005});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2005),
  description: 'testAccuracy_2005',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2005,
  description: 'landusemap2005',
  scale: 100
});

// Make image into feature collection
var fcclassified_2005 = ee.FeatureCollection([classified_2005]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2005.select(['classification']),
  description: 'classified_2005',
  assetId: 'classified_2005',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2005,
  description:'landusemap2005',
  fileFormat: 'KML'
});

///// 2006 ///////
// add satellite imagery for 2006 - surface reflectance
var landsatCollection_2006 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2006-06-01', '2006-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2006 = landsatCollection_2006.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2006 = median_2006.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2006.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2006
var classified_2006 = clipped_2006.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2006 = clipped_2006.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2006 = validation_2006.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2006 = validated_2006.errorMatrix('class', 'classification');
var exportconfusionMatrix_2006 = ee.Feature(null, {matrix: testError_2006.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2006),
  description: 'exportconfusionMatrix_2006',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2006 = testError_2001.accuracy(); 
var exporttestAccuracy_2006 = ee.Feature(null, {matrix: testAccuracy_2006});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2006),
  description: 'testAccuracy_2006',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2006,
  description: 'landusemap2006',
  scale: 100
});

// Make image into feature collection
var fcclassified_2006 = ee.FeatureCollection([classified_2006]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2006.select(['classification']),
  description: 'classified_2006',
  assetId: 'classified_2006',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2006,
  description:'landusemap2006',
  fileFormat: 'KML'
});

///// 2007 ///////
// add satellite imagery for 2007 - surface reflectance
var landsatCollection_2007 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2007-06-01', '2007-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2007 = landsatCollection_2007.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2007 = median_2007.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2007.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2007
var classified_2007 = clipped_2007.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2007 = clipped_2007.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2007 = validation_2007.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2007 = validated_2007.errorMatrix('class', 'classification');
var exportconfusionMatrix_2007 = ee.Feature(null, {matrix: testError_2007.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2007),
  description: 'exportconfusionMatrix_2007',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2007 = testError_2007.accuracy(); 
var exporttestAccuracy_2007 = ee.Feature(null, {matrix: testAccuracy_2007});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2007),
  description: 'testAccuracy_2007',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2007,
  description: 'landusemap2007',
  scale: 100
});

// Make image into feature collection
var fcclassified_2007 = ee.FeatureCollection([classified_2007]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2007.select(['classification']),
  description: 'classified_2007',
  assetId: 'classified_2007',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2007,
  description:'landusemap2007',
  fileFormat: 'KML'
});

///// 2008 ///////
// add satellite imagery for 2008 - surface reflectance
var landsatCollection_2008 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2008-06-01', '2008-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2008 = landsatCollection_2008.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2008 = median_2008.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2008.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2008
var classified_2008 = clipped_2008.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2008 = clipped_2008.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2008 = validation_2008.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2008 = validated_2008.errorMatrix('class', 'classification');
var exportconfusionMatrix_2008 = ee.Feature(null, {matrix: testError_2008.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2008),
  description: 'exportconfusionMatrix_2008',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2008 = testError_2008.accuracy(); 
var exporttestAccuracy_2008 = ee.Feature(null, {matrix: testAccuracy_2008});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2008),
  description: 'testAccuracy_2008',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2008,
  description: 'landusemap2008',
  scale: 100
});

// Make image into feature collection
var fcclassified_2008 = ee.FeatureCollection([classified_2008]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2008.select(['classification']),
  description: 'classified_2008',
  assetId: 'classified_2008',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2008,
  description:'landusemap2008',
  fileFormat: 'KML'
});

///// 2009 ///////
// add satellite imagery for 2009 - surface reflectance
var landsatCollection_2009 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2009-06-01', '2009-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2009 = landsatCollection_2009.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2009 = median_2009.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2009.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2009
var classified_2009 = clipped_2009.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2009 = clipped_2009.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2009 = validation_2009.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2009 = validated_2009.errorMatrix('class', 'classification');
var exportconfusionMatrix_2009 = ee.Feature(null, {matrix: testError_2009.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2009),
  description: 'exportconfusionMatrix_2009',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2009 = testError_2009.accuracy(); 
var exporttestAccuracy_2009 = ee.Feature(null, {matrix: testAccuracy_2009});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2009),
  description: 'testAccuracy_2009',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2009.select[('classification')],
  description: 'landusemap2009',
  scale: 100,
  region: poly
});

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2009.select(['classification']),
  description: 'classified_2009',
  assetId: 'classified_2009',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Make image into feature collection
var fcclassified_2009 = ee.FeatureCollection([classified_2009]);

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2009,
  description:'landusemap2009',
  fileFormat: 'KML'
});

///// 2010 ///////
// add satellite imagery for 2010 - surface reflectance
var landsatCollection_2010 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
     .filterDate('2010-06-01', '2010-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median_2010 = landsatCollection_2010.map(cloudMaskL457).median();

// clip on size of latvia 
var clipped_2010 = median_2010.clip(latvia_poly);

// Sample the input imagery to get a FeatureCollection of training data.
var training = clipped_2010.select(bands).sampleRegions({
  collection: trainingSample,
  properties: ['class'],
  scale: 30,
});

// classify FeatureCollection 2010
var classified_2010 = clipped_2010.classify(trainedClassifier, 'classification');

// Sample input to get validation data
var validation_2010 = clipped_2010.sampleRegions({
  collection: testingSample,
  properties: ['class'],
  scale: 30,
})

// Classify validation data
var validated_2010 = validation_2010.classify(trainedClassifier)

// Get error of testing data + export to table to save time
var testError_2010 = validated_2010.errorMatrix('class', 'classification');
var exportconfusionMatrix_2010 = ee.Feature(null, {matrix: testError_2010.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix_2010),
  description: 'exportconfusionMatrix_2010',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy_2010 = testError_2010.accuracy(); 
var exporttestAccuracy_2010 = ee.Feature(null, {matrix: testAccuracy_2010});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy_2010),
  description: 'testAccuracy_2010',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified_2010,
  description: 'landusemap2010',
  scale: 100
});

// Make image into feature collection
var fcclassified_2010 = ee.FeatureCollection([classified_2010]);

// Export the image to an Earth Engine asset.
Export.image.toAsset({
  image: classified_2010.select(['classification']),
  description: 'classified_2010',
  assetId: 'classified_2010',
  scale: 30,
  region: poly,
  maxPixels: 1e13
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified_2010,
  description:'landusemap2010',
  fileFormat: 'KML'
});

