// Izzy tries to load in all the datasets and zoom-in to Latvia 

// add CORINE dataset 
var CORINE = ee.Image('COPERNICUS/CORINE/V18_5_1/100m/2012');
var Corine_landCover = CORINE.select('landcover'); // select land cover
Map.addLayer(Corine_landCover, {}, 'Land Cover');

// add MODIS land-cover dataset
var MODIS = ee.ImageCollection('MODIS/006/MCD12Q1');
var igbpLandCover = MODIS.select('LC_Type1');
// below sets colours for the bands 
var igbpLandCoverVis = {
  min: 1.0,
  max: 17.0,
  palette: [
    '05450a', '086a10', '54a708', '78d203', '009900', 'c6b044', 'dcd159',
    'dade48', 'fbff13', 'b6ff05', '27ff87', 'c24f44', 'a5a5a5', 'ff6d4c',
    '69fff8', 'f9ffa4', '1c0dff'
  ],
};
Map.addLayer(igbpLandCover, igbpLandCoverVis, 'IGBP Land Cover'); // what you're adding, colours, what it will be called


