var data = d3.json("/data/jdcarbeck_git_data.json", function(error, data) {
  console.log(data[0]);
});

console.log(data);
