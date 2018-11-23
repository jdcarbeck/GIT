const WIDTH = 960;
const HEIGHT = 430;

var margin = {top: 20, right: 20, bottom: 100, left: 50},
    width = WIDTH - margin.left - margin.right,
    height = HEIGHT - margin.top - margin.bottom;
    gridSize = Math.floor(width / 24),
    legendElementWidth = gridSize*2,
    buckets = 8,
    colors = ["#081d58","#253494","#225ea8","#1d91c0","#41b6c4","#7fcdbb","#c7e9b4","#edf8b1"], // alternatively colorbrewer.YlGnBu[9]
    days = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    times = [ "01", "02", "03", "04", "05", "06"
            , "07", "08", "09", "10", "11", "12"
            , "13", "14", "15", "16", "17", "18"
            , "19", "20", "21", "22", "23", "24"];

var title =  d3.select("body").append("h1")
      .style("font-size", "2em")
      .style("font-family", "sans-serif");

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

var dayLabels = svg.selectAll(".dayLabel")
    .data(days)
    .enter().append("text")
      .text(function(d){ return d; })
      .attr("x", 0)
      .attr("y", function(d, i){ return i * gridSize; })
      .style("text-anchor", "end")
      .attr("transform", "translate(-6," + gridSize / 1.5 + ")")
      .attr("class", function (d, i) {
        return ((i >= 1 && i <= 5) ? "dayLabel mono axis axis-workweek" : "dayLabel mono axis");
      });

var timeLabels = svg.selectAll(".timeLabel")
    .data(times)
    .enter().append("text")
      .text(function(d) { return d; })
      .attr("x", function(d, i) { return i * gridSize; })
      .attr("y", 0)
      .style("text-anchor", "middle")
      .attr("transform", "translate(" + gridSize / 2 + ", -6)")
      .attr("class", function(d, i) {
        return ((i >= 7 && i <= 16) ? "timeLabel mono axis axis-worktime" : "timeLabel mono axis");
      });

var gridData = function gridData(){
  var data = new Array();
  for(var row = 0; row < 7; row++){
    data.push( new Array() );
    for(var col = 0; col < 24; col++){
      data[row].push({
        commits: 0,
        x: col,
        y: row
      });
    }
  }
  return data;
}

var totalCommits = 0;
var maxCommits = 0;

d3.json("/data/data.json").then(function(data){

  title.text(function(d){ return data.user });

  var grid = gridData();

  data.commits.forEach(function(d){
    var dt = new Date(Date.parse(d.timeOfCommit));
    var day = dt.getDay();
    var hr = dt.getHours();
    totalCommits++;
    grid[day][hr].commits++;
    if(grid[day][hr].commits > maxCommits)
      maxCommits = grid[day][hr].commits;
  });

  var colorScale = d3.scaleQuantile()
      .domain([0, buckets-1, maxCommits])
      .range(colors);

  var commitData = svg.selectAll(".hour")
      .data(grid)
      .enter()
      .selectAll(".hour")
      .data(function(d){ return d; })
      .enter().append("rect")
      .attr("x", function(d){ return (d.x * gridSize) + 1 })
      .attr("y", function(d){ return (d.y * gridSize) + 1})
      .attr("rx", 3)
      .attr("ry", 3)
      .attr("width", gridSize - 2)
      .attr("height", gridSize - 2)
      .style("opacity", 1)
      .style("fill", function(d){
        if(d.commits == 0) return "#dddddd";
        else return colorScale(d.commits); });

  var legend = svg.selectAll(".legend")
      .data([1].concat(colorScale.quantiles()), function(d){ return d; })
      .enter().append("g")
      .attr("class", "legend")

  legend.append("rect")
    .attr("x", function(d, i) { return legendElementWidth * i; })
    .attr("y", height)
    .attr("width", legendElementWidth)
    .attr("height", gridSize / 2)
    .style("fill", function(d, i) { return colors[i]; })

  legend.append("text")
    .attr("class", "mono")
    .text(function(d) { return "≥ " + Math.ceil(d); })
    .attr("x", function(d, i) { return legendElementWidth * i; })
    .attr("y", height + gridSize);

  console.log(totalCommits);
});
