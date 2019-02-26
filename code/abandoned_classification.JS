// classify abandoned land in Latvia with LUCAS data for 2012

var fc = ee.FeatureCollection('users/izzyrich/lucas_2012');
Map.setCenter(24.704, 56.854, 7);
Map.addLayer(fc, {}, 'abandoned points');
