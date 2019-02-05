// Classification tutorial
// https://geohackweek.github.io/GoogleEarthEngine/05-classify-imagery/

// give it a bunch of features for a class
// and then it learns
// can't have sparse data -- need information
// for each variable 

// Build, train, apply, assess

// define a region of interest as a point

var roi = geometry;

// THE FOLLOWING IS FOR A SINGLE IMAGE
// load image selection
var landsatCollection = ee.ImageCollection('LANDSAT/LC08/C01/T1')
    .filterDate('2017-01-01', '2017-12-31');

// Make a cloud-free composite.
var composite = ee.Algorithms.Landsat.simpleComposite({
  collection: landsatCollection,
  asFloat: true
});

// Visualize the Composite
Map.addLayer(composite, {bands: ['B4', 'B3', 'B2'], max: 0.5, gamma: 2}, 'L8 Image', false);
// here the bands represent the spectral bands like the colours 
// max value is assigned to white

// collect training data
// 1) go to geometry imports 
// 2) new layer - each new layer will be one class
// so in my case, abandoned etc.
// here, going to do urban, water and forest

// convert classes into merged layer with one land cover value
// with the classes of 0, 1 and 2
// Merge points together
var newfc = water.merge(urban).merge(forest);
print(newfc, 'newfc')

// now need to sample the Landsat imagery 
// will extract the reflectance in the designated bands
// for each of the points

// choose bands for trainings
var bands = ['B2', 'B3', 'B4', 'B5', 'B6', 'B7'];

// Sample the input imagery to get a FeatureCollection of training data.
var training = composite.select(bands).sampleRegions({
  collection: newfc,
  properties: ['landcover'],
  scale: 30
});

// see if it worked
print('trainingSAMPLE', training.limit(10));

// instigate a classifier and train it on the training data
var classifier = ee.Classifier.randomForest().train({
  features: training,
  classProperty: 'landcover',
  inputProperties: bands
});

// Classify the input imagery.
var classified = composite.select(bands).classify(classifier);

// Define a palette for the Land Use classification.
var palette = [
  'D3D3D3', // urban (0)  // grey
  '0000FF', // water (1)  // blue
  '008000' //  forest (2) // green
];

// Display the classification result and the input image.
Map.setCenter(24.11,56.948);
Map.addLayer(classified, {min: 0, max: 2, palette: palette}, 'Land Use Classification');

// Get a confusion matrix representing resubstitution accuracy.
print('RF error matrix: ', classifier.confusionMatrix());
print('RF accuracy: ', classifier.confusionMatrix().accuracy()); // 0.999

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
var trainedClassifier = ee.Classifier.randomForest().train({
  features: trainingPartition,
  classProperty: 'landcover',
  inputProperties: bands
});

// classify test FeatureCollection
var test = testingPartition.classify(trainedClassifier);

// print the confusion matrix
var confusionMatrix = test.errorMatrix('landcover', 'classification');
print('RF testing error matrix', confusionMatrix);
print('RF testing accuracy', confusionMatrix.accuracy());


var region = smalllatvia; 

var areaChart = ui.Chart.image.byClass({
  image: ee.Image.pixelArea().addBands(classified),
  classBand: 'classification', 
  region: region,
  scale: 30,
  reducer: ee.Reducer.sum()
});

print(areaChart)
