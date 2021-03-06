const { EventEmitter } = require('events');
const ev = new EventEmitter()
ev.on("event", function (line) {
    console.log(line);
})

var Elm = require("./Main.elm.js").Elm;

var fs = require('fs');
var data = fs.readFileSync(process.stdin.fd, 'utf-8');

const app = Elm.Main.init({
    flags: data
});

app.ports.output.subscribe(line =>
    ev.emit("event", line)
);
