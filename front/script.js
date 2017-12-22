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
var view = {
  grid: null, // [[<sprite>]]
  gridSize: null, // { w: ?, h: ? }
  viewSize: null // { w: ?, h: ? }
};

function cellWidth() {
  return view.viewSize.x / view.gridSize.x;
}

function cellHeight() {
  return view.viewSize.y / view.gridSize.y;
}

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
  let grid = state;
  // printGrid(grid);

  grid.forEach(function(row, i) {
    row.forEach(function(cell, j) {
      if (cell == "Wall") {
        view.grid[i][j].visible = true;
      } else {
        view.grid[i][j].visible = false;
      }
    });
  });

  if (player.sprite == null) {
    player.sprite = new Sprite(
      resources["sprites/player-sprite.png"].texture
    );
    player.sprite.width = cellWidth();
    player.sprite.height = cellHeight();
  }
  player.sprite.x = player.location.x * cellWidth();
  player.sprite.y = player.location.y * cellHeight();
  app.stage.addChild(player.sprite);
}

function processInputEvent(app, ws, e) {
  // console.log(e);
  if (e.type == "initializegame") {
    view.gridSize = {
      x: e.data[0], y: e.data[1]
    };
    // Make the view size the largest multiple of grid size that fits in the window
    view.viewSize = {
      x: window.innerWidth - (window.innerWidth % view.gridSize.x),
      y: window.innerHeight - (window.innerHeight % view.gridSize.x)
    };
    initView(app);
  } else if (e.type == "playermove") {
    ws.send(JSON.stringify({
      tag: "PlayerMove",
      contents: {
        x: e.x, y: e.y
      }
    }));
  } else if (e.type == "serverstate") {
    state = e.data["grid"];
    var p = e.data["player"];
    player.location.x = p["pos"]["x"];
    player.location.y = p["pos"]["y"];
  }
}

function processInput(app, ws) {
  input.forEach(function(e) {
    try {
      processInputEvent(app, ws, e);
    } catch (err) {
      console.error("Unable to process input event '"+e+"'");
      console.error(err);
    }
  });
  input = [];
}

function gameLoop(app, ws) {
  window.requestAnimationFrame(gameLoop.bind(null, app, ws));
  processInput(app, ws);
  render(app);
}

function initView(app) {
  app.renderer.resize(view.viewSize.x, view.viewSize.y);
  view.grid = [];
  for (var i = 0; i < view.gridSize.y; i++) {
    var row = [];
    for (var j = 0; j < view.gridSize.x; j++) {
      let sprite = new Sprite(
        resources["sprites/wall-sprite.png"].texture
      );
      sprite.width = cellWidth();
      sprite.height = cellHeight();
      sprite.x = j * cellWidth();
      sprite.y = i * cellHeight();
      sprite.visible = false;
      app.stage.addChild(sprite);
      row.push(sprite);
    }
    view.grid.push(row);
  }
}

function onMouseUp(ev) {
  input.push({
    type: "playermove",
    x: Math.floor(ev.clientX/cellWidth()),
    y: Math.floor(ev.clientY/cellHeight())
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

  gameLoop(app, ws);
}

function load() {
  loader
    .add("sprites/wall-sprite.png")
    .add("sprites/player-sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  load();
});
