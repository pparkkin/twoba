// State from server
var state = null;
// Player state
var player = {
  sprite: null,
  location: { x: 0, y: 0 }
};
// List of inputs
var input = [];
// The current view
var view = null;

var gridDimensions = null;

//Aliases
let Application = PIXI.Application,
    loader = PIXI.loader,
    resources = PIXI.loader.resources,
    Sprite = PIXI.Sprite;


function printGrid(grid) {
  var lines = grid.map(function(row) {
    return row.map(function(cell) {
      if (cell == "Live") {
        return "x";
      } else {
        return "o";
      }
    }).join('');
  });
  console.log(lines.join('\n'));
}

function render(app) {
  if (state == null) { return; }
  let grid = state,
      cellHeight = window.innerHeight / gridDimensions.y,
      cellWidth = window.innerWidth / gridDimensions.x;
  // printGrid(grid);

  grid.forEach(function(row, i) {
    row.forEach(function(cell, j) {
      if (cell == "Live") {
        view[i][j].visible = true;
      } else {
        view[i][j].visible = false;
      }
    });
  });

  if (player.sprite == null) {
    player.sprite = new Sprite(
      resources["sprites/player-sprite.png"].texture
    );
    player.sprite.width = cellWidth;
    player.sprite.height = cellHeight;
  }
  player.sprite.x = player.location.x * cellWidth;
  player.sprite.y = player.location.y * cellHeight;
  app.stage.addChild(player.sprite);
}

function processInput(app) {
  input.forEach(function(e) {
    // console.log(e);
    if (e.type == "initializegame") {
      gridDimensions = {
        x: e.data[0], y: e.data[1]
      };
      initView(app);
    } else if (e.type == "playermove") {
      player.location.x = e.x;
      player.location.y = e.y;
    } else if (e.type == "serverstate") {
      state = e.data["grid"];
    }
  });
  input = [];
}

function gameLoop(app) {
  window.requestAnimationFrame(gameLoop.bind(null, app));
  processInput(app);
  render(app);
}

function initView(app) {
  let cellHeight = window.innerHeight / gridDimensions.y,
      cellWidth = window.innerWidth / gridDimensions.x;
  view = [];
  for (var i = 0; i < gridDimensions.y; i++) {
    var row = [];
    for (var j = 0; j < gridDimensions.x; j++) {
      let sprite = new Sprite(
        resources["sprites/sprite.png"].texture
      );
      sprite.width = cellWidth;
      sprite.height = cellHeight;
      sprite.x = j * cellWidth;
      sprite.y = i * cellHeight;
      sprite.visible = false;
      app.stage.addChild(sprite);
      row.push(sprite);
    }
    view.push(row);
  }
}

function onMouseUp(ev) {
  let cellWidth = window.innerWidth / 20,
      cellHeight = window.innerHeight / 20;
  input.push({
    type: "playermove",
    x: Math.floor(ev.clientX/cellWidth),
    y: Math.floor(ev.clientY/cellHeight)
  });
}

function onMessage(ev) {
  var msg = JSON.parse(ev.data);
  if (msg["tag"] == "ServerHello") {
    input.push({
      type: "initializegame",
      data: msg["contents"]
    });
  } else if (msg["tag"] == "ServerState") {
    input.push({
      type: "serverstate",
      data: msg["contents"]
    });
  } else {
    console.log("Got unknown message tagged '"+msg["tag"]+"'");
  }
}

function setup() {
  let app = new Application({
    backgroundColor: 0xffffff
  });
  app.renderer.view.style.position = "absolute";
  app.renderer.view.style.display = "block";
  app.renderer.autoResize = true;
  app.renderer.resize(window.innerWidth, window.innerHeight);
  document.body.appendChild(app.view);

  window.addEventListener('mouseup', onMouseUp);

  let ws = new WebSocket('ws://localhost:3000');
  ws.onmessage = onMessage;
  ws.onopen = function (event) {
    ws.send("Gruffalo Crumble!\n");
  };

  gameLoop(app);
}

function load() {
  loader
    .add("sprites/sprite.png")
    .add("sprites/player-sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  load();
});
