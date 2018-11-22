const WIDTH = 960;
const HEIGHT = 430;

var margin = {top: 20, right: 20, bottom: 100, left: 50},
    width = WIDTH - margin.left - margin.right,
    height = HEIGHT - margin.top - margin.bottom;
    gridSize = Math.floor(width / 24),
    legendElementWidth = gridSize*2,
    buckets = 9,
    colors = [ "#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"], // alternatively colorbrewer.YlGnBu[9]
    days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    times = [ "01", "02", "03", "04", "05", "06"
            , "07", "08", "09", "10", "11", "12"
            , "13", "14", "15", "16", "17", "18"
            , "19", "20", "21", "22", "23", "24"];

var parseTime = d3.utcParse("%Y-%m-%dT%H:%M:%SZ");

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
        return ((i >= 0 && i <= 4) ? "dayLabel mono axis axis-workweek" : "dayLabel mono axis");
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

var avgChurn = 0;
var avgCommit = 0;
var totalCommits = 0;

d3.json("/data/jdcarbeck_git_data.json").then(function(data){
  
});
