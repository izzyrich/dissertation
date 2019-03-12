// classify land use in Latvia with LUCAS (2012) data

// add abandoned points 
var fc = ee.FeatureCollection('users/izzyrich/2012_lucas_abandoned');
Map.addLayer(fc, {}, 'abandoned points');

// import forestry points (other to help with classification)
var fc_forestry  = ee.FeatureCollection('users/izzyrich/2012_lucas_forestry');
Map.addLayer(fc_forestry, {}, 'forestry points');

// import extensive points
var fc_extensive  = ee.FeatureCollection('users/izzyrich/2012_lucas_extensive');
Map.addLayer(fc_extensive, {}, 'extensive points');

// import intensive points 
var fc_intensive  = ee.FeatureCollection('users/izzyrich/2012_lucas_intensive');
Map.addLayer(fc_intensive, {}, 'intensive points');

// import border to define ROI
var latvia_border  = ee.FeatureCollection('users/izzyrich/latvia_border');
Map.addLayer(latvia_border, {}, 'Border');

// add satellite imagery for 2012
var landsatCollection = ee.ImageCollection('LANDSAT/LE07/C01/T1_RT')
    .filterDate('2012-01-01', '2012-12-31')
    .filterBounds(latvia_border);

// Make a cloud-free composite of 2012 data 
var composite = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection,
  asFloat: true
});

// add 2012 image 
Map.addLayer(composite, {bands: ['B3', 'B2', 'B1'], max: 0.5, gamma: 2}, 'L7 Image', false);

// create polygons around points of each land use type as training data 

// create landclass variable, merging all classes (need to add extensive and intensive)
var newfc = abandoned_poly.merge(forestry);
print(newfc, 'newfc')

// choose bands for training -- chosen based off of a tutorial 
var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7'];

// Sample the input imagery to get a FeatureCollection of training data.
var training = composite.select(bands).sampleRegions({
  collection: newfc,
  properties: ['landuse'],
  scale: 30
});

// see if it worked
print('trainingSAMPLE', training.limit(10));

// instigate a classifier and train it on the training data
var classifier = ee.Classifier.randomForest().train({
  features: training,
  classProperty: 'landuse',
  inputProperties: bands
});

// Train the classifier
var trained = classifier.train(training, 'landuse', bands);

// Classify the input imagery.
var classified = composite.classify(trained);

// Define a palette for the Land Use classification.
var palette = [
  '008000', //  forest (0) // green
  '0000FF', // abandoned (1)  // blue
];

// Display the classification result and the input image.
Map.setCenter(24.11,56.948);
Map.addLayer(classified, {min: 0, max: 1, palette: palette}, 'Classification');

// Get a confusion matrix representing resubstitution accuracy.
print('RF error matrix: ', classifier.confusionMatrix());
print('RF accuracy: ', classifier.confusionMatrix().accuracy()); 

// how well the classifier was able to correctly label 
// resubstituted training data - so this is data the classifier had already seen
// for true accuracy - need to show new testing data

// accuracy assessment 

// add column to training dataset 
var withRandom = training.randomColumn('random');

// split up data for testing and training - 70% for training and 30% for testing

var split = 0.7;
var trainingPartition = withRandom.filter(ee.Filter.lt('random', split));
var testingPartition = withRandom.filter(ee.Filter.gte('random', split));

// trained with 70% of our data
var trainedClassifier = ee.Classifier.randomForest(500).train({
  features: trainingPartition,
  classProperty: 'landuse',
  inputProperties: bands
});

// classify test FeatureCollection
var test = testingPartition.classify(trainedClassifier);

// Get error of testing data + export to table to save time
var confusionMatrix = test.errorMatrix('landuse', 'classification');
var exportconfusionMatrix = ee.Feature(null, {matrix: confusionMatrix.array()}); 
Export.table.toDrive({
  collection: ee.FeatureCollection(exportconfusionMatrix),
  description: 'exportconfusionMatrix',
  fileFormat: 'CSV'
});

// Get accuracy of testing data + export to table to save time
var testAccuracy = confusionMatrix.accuracy(); 
var exporttestAccuracy = ee.Feature(null, {matrix: testAccuracy});
Export.table.toDrive({
  collection: ee.FeatureCollection(exporttestAccuracy),
  description: 'testAccuracy',
  fileFormat: 'CSV'
});

// can get pixel area this way (returns it in square meters)
// click the arrow in the corner to be able to export as csv 
// works but may be incorrect? total area doesn't add to total of latvia
var areaChart = ui.Chart.image.byClass({
  image: ee.Image.pixelArea().addBands(classified),
  classBand: 'classification', 
  region: latvia_border,
  scale: 30,
  reducer: ee.Reducer.sum()
});
print(areaChart);


// The following export/saving may be helpful 
// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified,
  description: 'landusemap2012',
  scale: 30
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


//// NEED TO FIGURE OUT HOW TO ITERATE OVER TIMESCALE (to make code more efficient)

// add satellite imagery
var landsatCollection2013 = ee.ImageCollection('LANDSAT/LE07/C01/T1_RT')
    .filterDate('2013-01-01', '2013-12-31')
    .filterBounds(latvia_border);
    
// something I saw to consider
// over the 6 month period in our date range there will be approx 12 images (more in overlap regions) default is to select most recent pixel. 
// change this using EE reducers. include median pixel value in the stack. median value will remove high value cloud pixels and shadows (low value)
//Map.addLayer(ee.ImageCollection('LANDSAT/LC8_L1T')
// .filterDate('2013-06-01','2013-12-31')
// .median()

// Make a cloud-free composite.
var composite2013 = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection2013,
  asFloat: true
});

// Classify the input imagery.
var classified2013 = composite2013.classify(trained);

// Map.addLayer(classified2013, {min: 0, max: 1, palette: palette}, 'Abandoned Classification 2013');

// can get pixel area this way (returns it in square meters)
// click the arrow in the corner to be able to export as csv 
// works but may be incorrect? total area doesn't add to total of latvia
var areaChart2013 = ui.Chart.image.byClass({
  image: ee.Image.pixelArea().addBands(classified2013),
  classBand: 'classification', 
  region: latvia_border,
  scale: 30,
  reducer: ee.Reducer.sum()
});
print(areaChart2013);

// FAILED ATTEMPTS -----
// try to get area of each class 
//var abandonedImage2012 = classified.select(['landuse']);
//var areaImage = abandonedImage2012.multiply(ee.Image.pixelArea())

//var areas = areaImage.reduceRegion({
//  reducer:ee.Reducer.sum(),
//  geometry: latvia_border,
//  scale: 30,
//  maxPixels:1e13
//})
//print(areas)

// var pixelAreas = classified.multiply(ee.Image.pixelArea());
// var totalAreas = pixelAreas.reduceRegion('sum', latvia_border, 30);
// print(totalAreas);

// add CORINE dataset 
var dataset = ee.Image('COPERNICUS/CORINE/V18_5_1/100m/2012');
var landCover = dataset.select('landcover');
Map.addLayer(landCover, {}, 'CORINE');
