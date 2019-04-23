// CALCULATIONS 


// import all classification images
var classified_1989  = ee.Image('users/izzyrich/classified_1989');
var classified_1990  = ee.Image('users/izzyrich/classified_1990');
var classified_1991  = ee.Image('users/izzyrich/classified_1991');
var classified_1992  = ee.Image('users/izzyrich/classified_1992');
var classified_1993  = ee.Image('users/izzyrich/classified_1993');
var classified_1994  = ee.Image('users/izzyrich/classified_1994');
var classified_1995  = ee.Image('users/izzyrich/classified_1995');
var classified_1996  = ee.Image('users/izzyrich/classified_1996');
var classified_1997  = ee.Image('users/izzyrich/classified_1997');
var classified_1998  = ee.Image('users/izzyrich/classified_1998');
var classified_1999  = ee.Image('users/izzyrich/classified_1999');
var classified_2000  = ee.Image('users/izzyrich/classified_2000');
var classified_2001  = ee.Image('users/izzyrich/classified_2001');
var classified_2002  = ee.Image('users/izzyrich/classified_2002');
var classified_2003  = ee.Image('users/izzyrich/classified_2003');
var classified_2004  = ee.Image('users/izzyrich/classified_2004');
var classified_2005  = ee.Image('users/izzyrich/classified_2005');
var classified_2006  = ee.Image('users/izzyrich/classified_2006');
var classified_2007  = ee.Image('users/izzyrich/classified_2007');
var classified_2008  = ee.Image('users/izzyrich/classified_2008');
var classified_2009  = ee.Image('users/izzyrich/classified_2009');
var classified_2010  = ee.Image('users/izzyrich/classified_2010');
var classified_2011  = ee.Image('users/izzyrich/classified'); 

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

// calculate transition area  -- this needs work!
var transitionarea = function(image1, image2, class1, class2, name){
  var first = image1.select('classification').eq(class1);
  var second = image2.select('classification').eq(class2);
  var change = first.and(second);
  var reduce = change.addBands(change).reduceRegions({
    collection: filtered, 
    reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName: 'classification',
  }), 
    scale:30});
  var feature = ee.Feature(reduce);
  Export.table.toDrive({
    collection: feature,
    description: name,
    fileFormat: 'CSV'
 });
};
transitionarea(classified_1989, classified_1990, 1, 3, '89_90_1to3');
transitionarea(classified_1989, classified_1990, 1, 2, '89_90_1to2');
transitionarea(classified_1989, classified_1990, 2, 1, '89_90_2to1');
transitionarea(classified_1989, classified_1990, 2, 3, '89_90_2to3');
transitionarea(classified_1989, classified_1990, 3, 1, '89_90_3to1');
transitionarea(classified_1989, classified_1990, 3, 2, '89_90_3to2');
transitionarea(classified_1990, classified_1991, 1, 3, '90_91_1to3');
transitionarea(classified_1990, classified_1991, 1, 2, '90_91_1to2');
transitionarea(classified_1990, classified_1991, 2, 1, '90_91_2to1');
transitionarea(classified_1990, classified_1991, 2, 3, '90_91_2to3');
transitionarea(classified_1990, classified_1991, 3, 1, '90_91_3to1');
transitionarea(classified_1990, classified_1991, 3, 2, '90_91_3to2');
transitionarea(classified_1991, classified_1992, 1, 3, '91_92_1to3');
transitionarea(classified_1991, classified_1992, 1, 2, '91_92_1to2');
transitionarea(classified_1991, classified_1992, 2, 1, '91_92_2to1');
transitionarea(classified_1991, classified_1992, 2, 3, '91_92_2to3');
transitionarea(classified_1991, classified_1992, 3, 1, '91_92_3to1');
transitionarea(classified_1991, classified_1992, 3, 2, '91_92_3to2'); 
transitionarea(classified_1992, classified_1993, 1, 3, '92_93_1to3');
transitionarea(classified_1992, classified_1993, 1, 2, '92_93_1to2');
transitionarea(classified_1992, classified_1993, 2, 1, '92_93_2to1');
transitionarea(classified_1992, classified_1993, 2, 3, '92_93_2to3');
transitionarea(classified_1992, classified_1993, 3, 1, '92_93_3to1');
transitionarea(classified_1992, classified_1993, 3, 2, '92_93_3to2');
transitionarea(classified_1993, classified_1994, 1, 3, '93_94_1to3');
transitionarea(classified_1993, classified_1994, 1, 2, '93_94_1to2');
transitionarea(classified_1993, classified_1994, 2, 1, '93_94_2to1');
transitionarea(classified_1993, classified_1994, 2, 3, '93_94_2to3');
transitionarea(classified_1993, classified_1994, 3, 1, '93_94_3to1');
transitionarea(classified_1993, classified_1994, 3, 2, '93_94_3to2');
transitionarea(classified_1994, classified_1995, 1, 3, '94_95_1to3');
transitionarea(classified_1994, classified_1995, 1, 2, '94_95_1to2');
transitionarea(classified_1994, classified_1995, 2, 1, '94_95_2to1');
transitionarea(classified_1994, classified_1995, 2, 3, '94_95_2to3');
transitionarea(classified_1994, classified_1995, 3, 1, '94_95_3to1');
transitionarea(classified_1994, classified_1995, 3, 2, '94_95_3to2');
transitionarea(classified_1995, classified_1996, 1, 3, '95_96_1to3');
transitionarea(classified_1995, classified_1996, 1, 2, '95_96_1to2');
transitionarea(classified_1995, classified_1996, 2, 1, '95_96_2to1');
transitionarea(classified_1995, classified_1996, 2, 3, '95_96_2to3');
transitionarea(classified_1995, classified_1996, 3, 1, '95_96_3to1');
transitionarea(classified_1995, classified_1996, 3, 2, '95_96_3to2');
transitionarea(classified_1996, classified_1997, 1, 3, '96_97_1to3');
transitionarea(classified_1996, classified_1997, 1, 2, '96_97_1to2');
transitionarea(classified_1996, classified_1997, 2, 1, '96_97_2to1');
transitionarea(classified_1996, classified_1997, 2, 3, '96_97_2to3');
transitionarea(classified_1996, classified_1997, 3, 1, '96_97_3to1');
transitionarea(classified_1996, classified_1997, 3, 2, '96_97_3to2');
transitionarea(classified_1997, classified_1998, 1, 3, '97_98_1to3');
transitionarea(classified_1997, classified_1998, 1, 2, '97_98_1to2');
transitionarea(classified_1997, classified_1998, 2, 1, '97_98_2to1');
transitionarea(classified_1997, classified_1998, 2, 3, '97_98_2to3');
transitionarea(classified_1997, classified_1998, 3, 1, '97_98_3to1');
transitionarea(classified_1997, classified_1998, 3, 2, '97_98_3to2');
transitionarea(classified_1998, classified_1999, 1, 3, '98_99_1to3');
transitionarea(classified_1998, classified_1999, 1, 2, '98_99_1to2');
transitionarea(classified_1998, classified_1999, 2, 1, '98_99_2to1');
transitionarea(classified_1998, classified_1999, 2, 3, '98_99_2to3');
transitionarea(classified_1998, classified_1999, 3, 1, '98_99_3to1');
transitionarea(classified_1998, classified_1999, 3, 2, '98_99_3to2');
transitionarea(classified_1999, classified_2000, 1, 3, '99_00_1to3');
transitionarea(classified_1999, classified_2000, 1, 2, '99_00_1to2');
transitionarea(classified_1999, classified_2000, 2, 1, '99_00_2to1');
transitionarea(classified_1999, classified_2000, 2, 3, '99_00_2to3');
transitionarea(classified_1999, classified_2000, 3, 1, '99_00_3to1');
transitionarea(classified_1999, classified_2000, 3, 2, '99_00_3to2');
transitionarea(classified_2000, classified_2001, 1, 3, '00_01_1to3');
transitionarea(classified_2000, classified_2001, 1, 2, '00_01_1to2');
transitionarea(classified_2000, classified_2001, 2, 1, '00_01_2to1');
transitionarea(classified_2000, classified_2001, 2, 3, '00_01_2to3');
transitionarea(classified_2000, classified_2001, 3, 1, '00_01_3to1');
transitionarea(classified_2000, classified_2001, 3, 2, '00_01_3to2');
transitionarea(classified_2001, classified_2002, 1, 3, '01_02_1to3');
transitionarea(classified_2001, classified_2002, 1, 2, '01_02_1to2');
transitionarea(classified_2001, classified_2002, 2, 1, '01_02_2to1');
transitionarea(classified_2001, classified_2002, 2, 3, '01_02_2to3');
transitionarea(classified_2001, classified_2002, 3, 1, '01_02_3to1');
transitionarea(classified_2001, classified_2002, 3, 2, '01_02_3to2');
transitionarea(classified_2002, classified_2003, 1, 3, '02_03_1to3');
transitionarea(classified_2002, classified_2003, 1, 2, '02_03_1to2');
transitionarea(classified_2002, classified_2003, 2, 1, '02_03_2to1');
transitionarea(classified_2002, classified_2003, 2, 3, '02_03_2to3');
transitionarea(classified_2002, classified_2003, 3, 1, '02_03_3to1');
transitionarea(classified_2002, classified_2003, 3, 2, '02_03_3to2');
transitionarea(classified_2003, classified_2004, 1, 3, '03_04_1to3');
transitionarea(classified_2003, classified_2004, 1, 2, '03_04_1to2');
transitionarea(classified_2003, classified_2004, 2, 1, '03_04_2to1');
transitionarea(classified_2003, classified_2004, 2, 3, '03_04_2to3');
transitionarea(classified_2003, classified_2004, 3, 1, '03_04_3to1');
transitionarea(classified_2003, classified_2004, 3, 2, '03_04_3to2');
transitionarea(classified_2004, classified_2005, 1, 3, '04_05_1to3');
transitionarea(classified_2004, classified_2005, 1, 2, '04_05_1to2');
transitionarea(classified_2004, classified_2005, 2, 1, '04_05_2to1');
transitionarea(classified_2004, classified_2005, 2, 3, '04_05_2to3');
transitionarea(classified_2004, classified_2005, 3, 1, '04_05_3to1');
transitionarea(classified_2004, classified_2005, 3, 2, '04_05_3to2');
transitionarea(classified_2005, classified_2006, 1, 3, '05_06_1to3');
transitionarea(classified_2005, classified_2006, 1, 2, '05_06_1to2');
transitionarea(classified_2005, classified_2006, 2, 1, '05_06_2to1');
transitionarea(classified_2005, classified_2006, 2, 3, '05_06_2to3');
transitionarea(classified_2005, classified_2006, 3, 1, '05_06_3to1');
transitionarea(classified_2005, classified_2006, 3, 2, '05_06_3to2');
transitionarea(classified_2006, classified_2007, 1, 3, '06_07_1to3');
transitionarea(classified_2006, classified_2007, 1, 2, '06_07_1to2');
transitionarea(classified_2006, classified_2007, 2, 1, '06_07_2to1');
transitionarea(classified_2006, classified_2007, 2, 3, '06_07_2to3');
transitionarea(classified_2006, classified_2007, 3, 1, '06_07_3to1');
transitionarea(classified_2006, classified_2007, 3, 2, '06_07_3to2');
transitionarea(classified_2007, classified_2008, 1, 3, '07_08_1to3');
transitionarea(classified_2007, classified_2008, 1, 2, '07_08_1to2');
transitionarea(classified_2007, classified_2008, 2, 1, '07_08_2to1');
transitionarea(classified_2007, classified_2008, 2, 3, '07_08_2to3');
transitionarea(classified_2007, classified_2008, 3, 1, '07_08_3to1');
transitionarea(classified_2007, classified_2008, 3, 2, '07_08_3to2');
transitionarea(classified_2008, classified_2009, 1, 3, '08_09_1to3');
transitionarea(classified_2008, classified_2009, 1, 2, '08_09_1to2');
transitionarea(classified_2008, classified_2009, 2, 1, '08_09_2to1');
transitionarea(classified_2008, classified_2009, 2, 3, '08_09_2to3');
transitionarea(classified_2008, classified_2009, 3, 1, '08_09_3to1');
transitionarea(classified_2008, classified_2009, 3, 2, '08_09_3to2');
transitionarea(classified_2009, classified_2010, 1, 3, '09_10_1to3');
transitionarea(classified_2009, classified_2010, 1, 2, '09_10_1to2');
transitionarea(classified_2009, classified_2010, 2, 1, '09_10_2to1');
transitionarea(classified_2009, classified_2010, 2, 3, '09_10_2to3');
transitionarea(classified_2009, classified_2010, 3, 1, '09_10_3to1');
transitionarea(classified_2009, classified_2010, 3, 2, '09_10_3to2'); 
transitionarea(classified_2010, classified_2011, 1, 3, '10_11_1to3');
transitionarea(classified_2010, classified_2011, 1, 2, '10_11_1to2');
transitionarea(classified_2010, classified_2011, 2, 1, '10_11_2to1');
transitionarea(classified_2010, classified_2011, 2, 3, '10_11_2to3');
transitionarea(classified_2010, classified_2011, 3, 1, '10_11_3to1');
transitionarea(classified_2010, classified_2011, 3, 2, '10_11_3to2'); 

// calculate area of each class
var area = function(image, name){
  var areacount = image.addBands(image).reduceRegions({
    collection: filtered, 
    reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName: 'classification',
  }), 
    scale:30});
var reduce = ee.FeatureCollection(ee.Feature(areacount));
  Export.table.toDrive({
    collection: areacount,
    description: name,
    fileFormat: 'CSV'
 });
};

area(classified_1989, 'classified_1989');
area(classified_1990, 'classified_1990');
area(classified_1991, 'classified_1991');
area(classified_1992, 'classified_1992');
area(classified_1993, 'classified_1993');
area(classified_1994, 'classified_1994');
area(classified_1995, 'classified_1995');
area(classified_1996, 'classified_1996');
area(classified_1997, 'classified_1997');
area(classified_1998, 'classified_1998');
area(classified_1999, 'classified_1999');
area(classified_2000, 'classified_2000');
area(classified_2001, 'classified_2001');
area(classified_2002, 'classified_2002');
area(classified_2003, 'classified_2003');
area(classified_2004, 'classified_2004');
area(classified_2005, 'classified_2005');
area(classified_2006, 'classified_2006');
area(classified_2007, 'classified_2007');
area(classified_2008, 'classified_2008');
area(classified_2009, 'classified_2009');
area(classified_2010, 'classified_2010');
area(classified_2011, 'classified_2011'); 

