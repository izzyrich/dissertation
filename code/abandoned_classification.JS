// classify abandoned land in Latvia with LUCAS data for 2012

// add abandoned points 
var fc = ee.FeatureCollection('users/izzyrich/lucas_2012');
Map.addLayer(fc, {}, 'abandoned points');

// import forestry points (other to help with classification)
var fc_forestry  = ee.FeatureCollection('users/izzyrich/2012_lucas_forestry');
Map.addLayer(fc_forestry, {}, 'forestry points');

// import border to define ROI
var latvia_border  = ee.FeatureCollection('users/izzyrich/latvia_border');
Map.addLayer(latvia_border, {}, 'Border');

// add satellite imagery
var landsatCollection = ee.ImageCollection('LANDSAT/LE07/C01/T1_RT')
    .filterDate('2012-01-01', '2012-12-31')
    .filterBounds(latvia_border);

// Make a cloud-free composite.
var composite = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection,
  asFloat: true
});

// add image
Map.addLayer(composite, {bands: ['B3', 'B2', 'B1'], max: 0.5, gamma: 2}, 'L7 Image', false);

// create landclass variable
var newfc = abandoned_poly.merge(forestry);
print(newfc, 'newfc')

// choose bands for trainings
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
Map.addLayer(classified, {min: 0, max: 1, palette: palette}, 'Abandoned Classification');

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

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: classified,
  description: 'landusemap2012',
  scale: 500
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


//// NEED TO FIGURE OUT HOW TO ITERATE OVER TIMESCALE

// add satellite imagery
var landsatCollection2 = ee.ImageCollection('LANDSAT/LE07/C01/T1_RT')
    .filterDate('2013-01-01', '2013-12-31')
    .filterBounds(latvia_border);

// Make a cloud-free composite.
var composite2013 = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection2,
  asFloat: true
});


// Classify the input imagery.
var classified2013 = composite2013.classify(trained);

Map.addLayer(classified2013, {min: 0, max: 1, palette: palette}, 'Abandoned Classification 2013');
