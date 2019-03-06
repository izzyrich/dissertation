// classify abandoned land in Latvia with LUCAS data for 2012

var fc = ee.FeatureCollection('users/izzyrich/lucas_2012');
Map.setCenter(24.704, 56.854, 7);
Map.addLayer(fc, {}, 'abandoned points'); // drawn in 

// add satellite imagery
var landsatCollection = ee.ImageCollection('LANDSAT/LE07/C01/T1_RT')
    .filterDate('2012-01-01', '2012-12-31');

// Make a cloud-free composite.
var composite = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection,
  asFloat: true
});

// add image
Map.addLayer(composite, {bands: ['B3', 'B2', 'B1'], max: 0.5, gamma: 2}, 'L7 Image', false);

// create landclass variable
var newfc = abandoned_poly;
print(newfc, 'newfc')

// choose bands for trainings
var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7'];

// Sample the input imagery to get a FeatureCollection of training data.
var training = composite.select(bands).sampleRegions({
  collection: newfc,
  properties: ['abandoned'],
  scale: 30
});

// see if it worked
print('trainingSAMPLE', training.limit(10));

// instigate a classifier and train it on the training data
var classifier = ee.Classifier.randomForest().train({
  features: training,
  classProperty: 'abandoned',
  inputProperties: bands
});

// Classify the input imagery.
var classified = composite.select(bands).classify(classifier);

// Define a palette for the Land Use classification.
var palette = [
  '0000FF', // abandoned (1)  // blue
];

// Display the classification result and the input image.
Map.setCenter(24.11,56.948);
Map.addLayer(classified, {min: 0, max: 1, palette: palette}, 'Abandoned Classification');
