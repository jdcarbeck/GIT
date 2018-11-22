const WIDTH = 960;
const HEIGHT = 430;

var margin = {top: 20, right: 20, bottom: 100, left: 50},
    width = WIDTH - margin.left - margin.right,
    height = HEIGHT - margin.top - margin.bottom;
    gridSize = Math.floor(width / 24),
    legendElementWidth = gridSize*2,
    buckets = 9,
    colors = [ "#18A993","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58"], // alternatively colorbrewer.YlGnBu[9]
    days = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    times = [ "01", "02", "03", "04", "05", "06"
            , "07", "08", "09", "10", "11", "12"
            , "13", "14", "15", "16", "17", "18"
            , "19", "20", "21", "22", "23", "24"];

var t = d3.transition()
    .duration(750)

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

var avgChurn = 0;
var avgCommit = 0;
var totalCommits = 0;

d3.json("/data/GitSquared_edex-ui_data.json").then(function(data){

  title.text(function(d){return data.user + "/" + data.repo;});

  var cards = svg.selectAll(".hour")
      .data(data.commits, function(d){
        var dt = new Date(Date.parse(d.timeOfCommit));
        var day = dt.getDay();
        var hr = dt.getHours();
        return day+':'+hr;
      });

  cards.append("title");

  cards.enter().append("rect")
        .attr("y", function(d){
          var dt = new Date(Date.parse(d.timeOfCommit));
          var day = dt.getDay();
          return (((day) * gridSize));
        })
        .attr("x", function(d){
          var dt = new Date(Date.parse(d.timeOfCommit));
          var hr = dt.getHours();
          return (((hr) * gridSize));
        })
        .attr("class", "hour")
        .attr("width", gridSize)
        .attr("height", gridSize)
        .style("opacity", 0.1)
        .style("fill", colors[0]);
});
