var margin = {top: 20, right: 20, bottom: 100, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var parseTime = d3.utcParse("%Y-%m-%dT%H:%M:%SZ");

var x = d3.scaleTime().range([0,width]);
var y = d3.scaleLinear().range([height, 0]);

var line = d3.line()
    .x(function(d) { return x(d.timeOfCommit); })
    .y(function(d) { return y(d.churn); });

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

d3.json("/data/jdcarbeck_git_data.json").then(function(data){
  data.forEach(function(d){
    d.timeOfCommit = parseTime(d.timeOfCommit);
    if(d.totalLines > 0){
      d.churn = (Math.round((d.delLines/d.totalLines)*100));
    }
    else {
      console.log(d);
    }
  });


  x.domain(d3.extent(data,function(d){ return d.timeOfCommit }));
  y.domain([0,100]);

  svg.append("path")
      .data([data])
      .attr("class", "line")
      .attr("d", line);

  svg.append("g")
      .attr("class", "axis")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
              .tickFormat(d3.timeFormat("%d/%m/%Y")))
      .selectAll("text")
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", "rotate(-45)");

      // Add the Y Axis
  svg.append("g")
      .attr("class", "axis")
      .call(d3.axisLeft(y));
});
