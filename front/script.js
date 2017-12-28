// State from server
var state = {
  grid: null,
  dirty: false // only process if dirty
};
// Player state
var player = {
  location: null,
  destination: null,
  cooldown: 0,
  dirty: false
};
// Enemy state
var enemy = {
  location: null,
  dirty: false
};
// List of inputs
var input = [];
// The current view
var view = {
  grid: null, // [[<sprite>]]
  playerSprite: null,
  playerProgressBar: null,
  targetSprite: null,
  enemySprite: null,
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
    Sprite = PIXI.Sprite,
    Graphics = PIXI.Graphics;


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

function renderGrid(app) {
  if (!state.dirty) { return; }
  let grid = state.grid;
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
  state.dirty = false;
}

function renderProgressBar(app, gridX, gridY, progress) {
  if (view.playerProgressBar != null) {
    app.stage.removeChild(view.playerProgressBar);
    view.playerProgressBar = null;
  }
  if (player.cooldown <= 0) {
    return;
  }
  let topPad = 3,
      leftPad = 3,
      rightPad = leftPad,
      barHeight = 7,
      fullBarWidth = cellWidth() - (leftPad + rightPad),
      barWidth = fullBarWidth * progress,
      barX = (cellWidth() * gridX) + leftPad,
      barY = (cellHeight() * gridY) + topPad;
  view.playerProgressBar = new Graphics();
  view.playerProgressBar.beginFill(0x66CCFF);
  view.playerProgressBar.lineStyle(1, 0x000000, 1);
  view.playerProgressBar.drawRect(0, 0, barWidth, barHeight);
  view.playerProgressBar.endFill();
  view.playerProgressBar.x = barX;
  view.playerProgressBar.y = barY;
  app.stage.addChild(view.playerProgressBar);
}

function renderPlayer(app) {
  if (!player.dirty) { return; }
  view.playerSprite.x = player.location.x * cellWidth();
  view.playerSprite.y = player.location.y * cellHeight();
  if (player.destination != null) {
    view.targetSprite.x = player.destination.x * cellWidth();
    view.targetSprite.y = player.destination.y * cellHeight();
    view.targetSprite.visible = true;
    app.stage.addChild(view.targetSprite);
  } else {
    view.targetSprite.visible = false;
  }
  app.stage.addChild(view.playerSprite);
  renderProgressBar(app, player.location.x, player.location.y, player.cooldown / 12);
  player.dirty = false;
}

function render(app) {
  renderGrid(app);
  renderPlayer(app);
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
    state.grid = e.data["grid"];
    state.dirty = true;
    var p = e.data["player"][1];
    player.location = {
      x: p["pos"]["x"],
      y: p["pos"]["y"]
    };
    if (p["pos"] != p["dst"]) {
      player.destination = {
        x: p["dst"]["x"],
        y: p["dst"]["y"],
      };
    } else {
      player.destination = null;
    }
    player.cooldown = p["cooldown"];
    player.dirty = true;
    var n = e.data["enemy"];
    enemy.location = {
      x: n["pos"]["x"],
      y: n["pos"]["y"]
    };
    enemy.dirty = true;
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

function initGrid(app) {
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

function initPlayer(app) {
  if (view.playerSprite == null) {
    view.playerSprite = new Sprite(
      resources["sprites/player-sprite.png"].texture
    );
    view.playerSprite.width = cellWidth();
    view.playerSprite.height = cellHeight();
  }
  if (view.targetSprite == null) {
    view.targetSprite = new Sprite(
      resources["sprites/target-sprite.png"].texture
    );
    view.targetSprite.width = cellWidth();
    view.targetSprite.height = cellHeight();
    view.targetSprite.visible = false;
    app.stage.addChild(view.targetSprite);
  }
  player.location = { x: 0, y: 0 };
  view.playerSprite.x = player.location.x * cellWidth();
  view.playerSprite.y = player.location.y * cellHeight();
  app.stage.addChild(view.playerSprite);
}

function initEnemy(app) {
  if (view.enemySprite == null) {
    view.enemySprite = new Sprite(
      resources["sprites/enemy-sprite.png"].texture
    );
    view.enemySprite.width = cellWidth();
    view.enemySprite.height = cellHeight();
  }
  enemy.location = { x: view.gridSize.x - 1, y: view.gridSize.y - 1 };
  view.enemySprite.x = enemy.location.x * cellWidth();
  view.enemySprite.y = enemy.location.y * cellHeight();
  app.stage.addChild(view.enemySprite);
}

function initView(app) {
  app.renderer.resize(view.viewSize.x, view.viewSize.y);
  initGrid(app);
  initPlayer(app);
  initEnemy(app);
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

// https://stackoverflow.com/a/1349426/7339694
function random_name() {
  var text = "";
  var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

  for (var i = 0; i < 5; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }

  return text;
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
    ws.send(random_name() + "\n");
  };
  ws.onclose = function (event) {
    alert("Connection to server closed!");
  };

  gameLoop(app, ws);
}

function load() {
  loader
    .add("sprites/wall-sprite.png")
    .add("sprites/player-sprite.png")
    .add("sprites/target-sprite.png")
    .add("sprites/enemy-sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  load();
});
