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

// add buffer of 100 metres to create polygons
var fc_total = fc_total.map(function(f) {
  return f.buffer(100);
});

var forestry = forestry.map(function(f) {
  return f.buffer(100);
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: forestry,
  description:'forestry_buffer',
  fileFormat: 'KML'
});

var abandoned = abandoned.map(function(f) {
  return f.buffer(100);
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: abandoned,
  description:'abandoned_buffer',
  fileFormat: 'KML'
});

var extensive = extensive.map(function(f) {
  return f.buffer(100);
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: extensive,
  description:'extensive_buffer',
  fileFormat: 'KML'
});

var intensive = intensive.map(function(f) {
  return f.buffer(100);
});

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: intensive,
  description:'intensive_buffer',
  fileFormat: 'KML'
});

var water = water.map(function(f) {
  return f.buffer(100);
});


var wetlands = wetlands.map(function(f) {
  return f.buffer(100);
});

var artificial = artificial.map(function(f) {
  return f.buffer(100);
});

// add layer to map
Map.addLayer(fc_total, {}, 'total');

// import shape of Latvia polygon to define ROI
var latvia_poly = ee.FeatureCollection('users/izzyrich/latvia_poly');

// add satellite imagery for 2012 - surface reflectance
var landsatCollection = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
     .filterDate('2012-06-01', '2012-08-30');
    
// get median of imagery to remove high and low reflectance (cloud and shadow)
var median = landsatCollection.median();

// clip on size of latvia 
var clipped = median.clip(latvia_poly);

// visualise Landsat imagery
Map.addLayer(clipped, {bands: ['B3', 'B2', 'B1']}, 'L7 Image', false);
Map.setCenter(24.11, 56.948);

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

// split up data for testing and training - 70% for training and 30% for testing
var split = 0.7;

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

// trained with 70% of our data
var trainedClassifier = ee.Classifier.randomForest({
  numberOfTrees: 30
})
  .train(training, 'class');

// classify FeatureCollection
var classified = clipped.classify(trainedClassifier, 'classification');

// Define a palette for the Land Use classification.
var palette = ['B0A8B9', // 0, forestry, grey
'936C00', // 1, abandoned, orange/brown
'00896F', // 2, extensive, green
'4B4453', // 3, intensive, dark purple
'008CCA', // 4, water, blue
'B0A8B9', // 5, wetlands, grey
'B0A8B9', // 6, artificial, grey
];

// Display the classification result and the input image.
Map.setCenter(24.11,56.948);
Map.addLayer(classified, {min: 0, max: 6, palette: palette}, 'Classification');

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
  description: 'exportconfusionMatrix',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy = testError.accuracy(); 
var exporttestAccuracy = ee.Feature(null, {matrix: testAccuracy});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy),
  description: 'testAccuracy',
  fileFormat: 'CSV'
});

// calculate area by class
var count = classified.eq([0, 1, 2, 3, 4, 5, 6]);
var total = count.multiply(ee.Image.pixelArea());
var area = total.reduceRegion(ee.Reducer.sum(), latvia_poly, 100, null, null, false, 1e13);
// print('Area by class (m2)',area);

// create feature class
var areaSize = ee.Feature(null, area);

// export data 
Export.table.toDrive({
  collection: ee.FeatureCollection([areaSize]),
  description: 'area',
  fileFormat: 'CSV'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified,
  description: 'landusemap2012',
  scale: 100
});

// Make image into feature collection
var fcclassified = ee.FeatureCollection([classified]);

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: fcclassified,
  description:'landusemap2012',
  fileFormat: 'KML'
});

//Export as map
Export.map.toCloudStorage(classified);


// CORINE 

// add CORINE dataset 
var dataset = ee.Image('COPERNICUS/CORINE/V18_5_1/100m/2012');
var landCover = dataset.select('landcover');

// clip on size of latvia 
var clippedCORINE = landCover.clip(latvia_poly);

Map.addLayer(clippedCORINE, {}, 'CORINE');

// Make image into feature collection
var CORINEKML = ee.FeatureCollection([clippedCORINE]);

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: CORINEKML,
  description:'CORINE2012',
  fileFormat: 'KML'
});

// Export to Drive
Export.image.toDrive({
  image: clippedCORINE,
  description: 'CORINE',
  crs:'EPSG:3857',
  scale: 100, // adjust for desired resolution
});
