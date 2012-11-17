Namespace("org.wattdepot.gdata.makahiki");

// http://code.google.com/apis/visualization/documentation/gallery/genericimagechart.html

// Creates an Energy Data visualization for the last 24 hours.
org.wattdepot.gdata.makahiki.EnergyLastTwentyFourHours = function() {

  // Generates a datatable for this specific source from the multi-source datatable.
  // The datatable is used to generate a stacked bar chart where the lower bar is colored 
  // green and represents energy consumed up to the goal level, and the upper bar is colored
  // red and represents energy consumed in excess of the goal level. 
  // Column 0 contains "green" energy consumed (up to the goal level) for each hour of the past 24.
  // Column 1 contains "red" energy consumed (in excess of the goal level, or 0 if under goal).
  // Column 2 contains the goal energy consumption value.
  // Column 3 contains the X axis label to be used for each hour.
  // Column 4 contains a single value, the last update timestamp.
  function makeDataTable(energyLast24Datatable, source, goal) {
    // Create the data table to return.
    var datatable = new google.visualization.DataTable();
    datatable.addColumn('number'); // the "green" kWh.
    datatable.addColumn('number'); // the "red" kWh.
    datatable.addColumn('number'); // the goal kWh.
    datatable.addColumn('string'); // the X axis label.
    datatable.addColumn('date'); // the last update timestamp.
    datatable.addRows(24);
    
    var millisPerHour = 1000 * 60 * 60;
    // Determine the row in the rollingHours table that contains this source's data.
    var row = findRow(energyLast24Datatable, source);
    // Get the last update time for this row in milliseconds.
    var lastUpdateMillis = energyLast24Datatable.getValue(row, 1).getTime();
    // Set the last update timestamp.
    datatable.setCell(0, 4, energyLast24Datatable.getValue(row, 1));
    // Initialize columns 0 to 3 of each of the 24 rows.
    for ( var hour = 0; hour < 24; hour++) {
      // Get the baseline, then determine the actual, green, read, and goal kWh.
      var baselineKwH = energyLast24Datatable.getValue(row, hour + 26);
      var goalKwH = baselineKwH * ((100 - goal) / 100);
      var actualKwH = energyLast24Datatable.getValue(row, hour + 2);
      var greenKwH = (actualKwH > goalKwH) ? goalKwH : actualKwH;
      var redKwH = (actualKwH > goalKwH) ? (actualKwH - goalKwH) : 0;
      datatable.setCell(hour, 0, greenKwH);
      datatable.setCell(hour, 1, redKwH);
      datatable.setCell(hour, 2, goalKwH);
      // Set the X axis label.
      var hoursAgo = hour - 24;
      var currDate = new Date(lastUpdateMillis + (hoursAgo * millisPerHour));
      var axisHour = (currDate.getHours() + 1);
      if (axisHour % 3 == 0) {
        var axisAMorPM = (axisHour >= 12) ? "pm" : "am";
        var axisLabel = ((axisHour > 12) ? (axisHour - 12) : axisHour) + axisAMorPM;
        // Special case noon and midnight.
        if (axisHour == 12) {
          axisLabel = 'noon';
        }
        else if (axisHour == 24) {
          axisLabel = 'mid';
        }
        datatable.setCell(hour, 3, axisLabel);
      }
    }
    return datatable;
  }
  
  //Returns the row in which source's data is located, or -1 if not found.
  function findRow(datatable, source) {
    for (var i = 0; i < datatable.getNumberOfRows(); i++) {
      if (datatable.getValue(i, 0) == source) {
        return i;
      }
    }
    alert("Unable to find row in datatable for source: " + source);
    return -1;
  }


  // Draws the three components (title, chart, caption) of the energy
  // visualization.
  function draw(id, source, goal, spreadsheetDatatable, options) {
    var width = options.width || 300;
    var height = options.height || 200;
    var backgroundColor = options.backgroundColor || "F5F3E5";
    var globalStyle = options.globalStyle || {};
    var titleStyle = options.titleStyle || {};
    var captionStyle = options.captionStyle || {};
    var title = options.title || 'Actual Consumption vs. Goal<br>(Last 24 Hours)';
    
    // Create a datatable with this source's data. 
    var datatable = makeDataTable(spreadsheetDatatable, source, goal);

    // Get the top-level div where this visualization should go.
    element = document.getElementById(id);
    // Now add the elements.
    addGlobalStyle(element, backgroundColor, width, globalStyle);
    addTitleDiv(element, id, title, titleStyle, width);
    addChartDiv(element, id, source, goal, datatable, width, height, backgroundColor);
    addCaptionDiv(element, id, goal, captionStyle, datatable, width);
  }

  // Adds 'global' CSS styling to the top-level div passed into this instance.
  function addGlobalStyle(element, backgroundColor, width, globalStyle) {
    element.style.backgroundColor = backgroundColor;
    element.style.margin = '0 auto';
    element.style.width = width + 'px';
    addStyleProperties(element, globalStyle);
  }

  function addTitleDiv(element, id, title, titleStyle, width) {
    var divId = id + '__Title';
    var div = getElementByIdOrCreate(divId, 'div');
    element.appendChild(div);
    div.style.textAlign = 'center';
    addStyleProperties(div, titleStyle);
    div.style.width = width + 'px';
    div.innerHTML = title;
  }

  function addChartDiv(element, id, source, goal, datatable, width, height, backgroundColor) {
    var divId = id + '__Chart';
    var div = getElementByIdOrCreate(divId, 'div');
    element.appendChild(div);

    var greenColor = '459E00';
    var redColor = 'FF0000';
    // Now set up the chart options.
    var options = {};
    // Chart type is a stacked vertical bar chart.
    options.cht = 'bvs';
    // Specify scale of Y axis: from 0 to the max value of the stacked bars.
    var maxValue = getMaxValue(datatable);
    options.chds = '0,' + maxValue;
    // Set the width and height.
    options.chs = width + 'x' + height;
    // Specify the bar colors.
    options.chco = greenColor + ',' + redColor;
    // Specify the axes.
    options.chxt = 'x,y,y';
    // Specify axis ranges.
    options.chxr = '0,0,' + maxValue + '|' + '1,0,' + maxValue; 
    // Specify background color.
    options.chf = 'bg,s,' + backgroundColor;
    // Specify axis label positions.
    options.chxp = '2,' + Math.floor(maxValue / 2);
    // Specify where the bar chart data ends.
    options.firstHiddenColumn = 2;
    // Specify the axis labels
    options.chxl = '0:|' + getDateAxisLabels(datatable) + '|2:|kWh';

    // Create and draw the visualization.
    var chart = new google.visualization.ImageChart(div);
    chart.draw(datatable, options);
  }

  function addCaptionDiv(element, id, goal, captionStyle, datatable, width) {
    var divId = id + '__Caption';
    var div = getElementByIdOrCreate(divId, 'div');
    var goalKwH = Math.round(sumColumn(datatable, 2));
    var actualKwH = Math.round(sumColumn(datatable, 0) + sumColumn(datatable, 1));
    var didMeet = (goalKwH > actualKwH) ? " Meeting " : " Not meeting ";
    var lastUpdate = toShortDate(datatable.getValue(0,4));
    element.appendChild(div);
    addStyleProperties(div, captionStyle);

    div.innerHTML = 
      '<p style="margin-left: 10px">' +
      'Goal: ' + goalKwH + ' total kWh. (' + goal + '% below baseline.)<br>' +
      'Actual: ' + actualKwH + ' total kWh. ' + didMeet + 'the goal.<br> ' +
      'Red tips are hours where use exceeded goal. <br>' +
      'Last update: ' + lastUpdate + 
      '</p>';
  }

  // Returns the pre-existing element with id 'id', or else creates and returns
  // a new element with type elementType and that id.
  function getElementByIdOrCreate(id, elementType) {
    var element = document.getElementById(id);
    if (element) {
      return element;
    }
    else {
      element = document.createElement(elementType);
      element.setAttribute("id", id);
      return element;
    }
  }
  
  // Returns the passed date string in yyyy/mm/dd hh:mm:ss format.
  function toShortDate(date) {
    var tmp =
    (date.getMonth() + 1) + '/' +
    date.getDate() + '/' +
    date.getFullYear() + ' ' +
    date.getHours() + ':' +
    (date.getMinutes() < 10 ? '0' : '') +
    date.getMinutes() + ':' +
    (date.getSeconds() < 10 ? '0' : '') +
    date.getSeconds();
    return tmp;
  }


  // DEPRECATED: Returns a string of pipe-separated colors, one for each bar in the histogram.
  // Red if over the baseline, and green if equal to or under the baseline.
  function getColors(datatable) {
    var colors = [];
    for ( var i = 0; i < datatable.getNumberOfRows(); i++) {
      if ((datatable.getValue(i, 0) > datatable.getValue(i, 1))) {
        colors.push(redColor);
      }
      else {
        colors.push(greenColor);
      }
    }
    return colors.join('|');
  }

  // Return the maximum stacked bar value in the datatable.
  function getMaxValue(datatable) {
    // Find the maximum stacked kWh value (sum of column 0 and 1).
    var maxValue = 0;
    for ( var i = 0; i < datatable.getNumberOfRows(); i++) {
      var stackedValue = datatable.getValue(i, 0) + datatable.getValue(i, 1); 
      if (stackedValue > maxValue) {
        maxValue = stackedValue;
      }
    }
    return maxValue;
  }
  
  // Returns the sum of all the values in the passed column of the datatable. 
  function sumColumn(datatable, column) {
    var total = 0;
    for (var i = 0; i < datatable.getNumberOfRows(); i++) {
      total += datatable.getValue(i, column);
    }
    return total;
  }

  // Returns a string containing the last seven days for use in the X axis labels.
  // Uses the last update value, assumes that this day is one day after the last
  // seven days.
  function getDateAxisLabels(datatable) {
    var dates = [];
    for (var i = 0; i < datatable.getNumberOfRows(); i++) {
      dates.push(datatable.getValue(i, 3));
    }
    return dates.join('|');
  }
  
  //Updates the divElement style attribute with all properties in styleObject.
  function addStyleProperties(divElement, styleObject) {
    for (key in styleObject) {
      if (styleObject.hasOwnProperty(key)) {
        divElement.style[key] = styleObject[key]; 
      }
    }
  }

  

  return {
    // Public interface to this function.
    draw : draw
  };
}
