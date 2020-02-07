console.log('starting');
require("./exports.js");
var app = require("./src/demo.bs.js").main(document.getElementById("app"));
console.log('app',app);
