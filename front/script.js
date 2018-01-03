// State from server
var state = {
  grid: null
  // dirty: false // only process if dirty
};
// Player state
var player = {
  name: null,
  location: null,
  destination: null,
  cooldown: 0,
  hp: 0,
  dirty: false
};
// Enemy state
var enemy = {
  location: null,
  dead: false,
  dirty: false
};
// List of inputs
var input = [];
// The current view
var view = {
  grid: null, // [[<sprite>]]
  playerSprite: null,
  playerHealthBar: null,
  playerProgressBar: null,
  targetSprite: null,
  enemySprite: null,
  gridSize: null, // { w: ?, h: ? }
  viewSize: null // { w: ?, h: ? }
};

function cellWidth() {
  let width = view.viewSize.x / view.gridSize.x,
      height = view.viewSize.y / view.gridSize.y;
  return Math.min(width, height);
}

function cellHeight() {
  return cellWidth();
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

function renderHealthBar(app, gridX, gridY, progress) {
  if (view.playerHealthBar != null) {
    app.stage.removeChild(view.playerHealthBar);
    view.playerHealthBar = null;
  }
  if (progress <= 0) {
    return;
  }
  let bottomPad = 5,
      leftPad = 5,
      rightPad = leftPad,
      barHeight = 7,
      fullBarWidth = cellWidth() - (leftPad + rightPad),
      barWidth = fullBarWidth * progress,
      barX = (cellWidth() * gridX) + leftPad,
      barY = (cellHeight() * gridY) + (cellHeight() - (bottomPad + barHeight));
  view.playerHealthBar = new Graphics();
  view.playerHealthBar.beginFill(0xFF3333);
  view.playerHealthBar.lineStyle(1, 0x000000, 1);
  view.playerHealthBar.drawRect(0, 0, barWidth, barHeight);
  view.playerHealthBar.endFill();
  view.playerHealthBar.x = barX;
  view.playerHealthBar.y = barY;
  app.stage.addChild(view.playerHealthBar);
}

function renderProgressBar(app, gridX, gridY, progress) {
  if (view.playerProgressBar != null) {
    app.stage.removeChild(view.playerProgressBar);
    view.playerProgressBar = null;
  }
  if (progress <= 0) {
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

function spriteTurnLeft(sprite) {
  sprite.anchor.x = 1;
  sprite.scale.x = -1;
  sprite.width = cellWidth();
  sprite.height = cellHeight();
}

function spriteTurnRight(sprite) {
  sprite.anchor.x = 0;
  sprite.scale.x = 1;
  sprite.width = cellWidth();
  sprite.height = cellHeight();
}

function playerTurnLeft() {
  spriteTurnLeft(view.playerSprite);
}

function playerTurnRight() {
  spriteTurnRight(view.playerSprite);
}

function enemyTurnLeft() {
  spriteTurnLeft(view.enemySprite);
}

function enemyTurnRight() {
  spriteTurnRight(view.enemySprite);
}

function renderPlayer(app) {
  if (!player.dirty) { return; }
  if (player.hp == 0) {
    app.stage.removeChild(view.playerSprite);
    view.playerSprite = new Sprite(
      resources["sprites/dead-sprite.png"].texture
    );
    view.playerSprite.width = cellWidth();
    view.playerSprite.height = cellHeight();
    app.stage.addChild(view.playerSprite);
  }
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
  view.playerSprite.visible = true;
  if (player.location.x > enemy.location.x) {
    playerTurnLeft();
  } else {
    playerTurnRight();
  }
  app.stage.addChild(view.playerSprite);
  renderHealthBar(app, player.location.x, player.location.y, player.hp / 20);
  renderProgressBar(app, player.location.x, player.location.y, player.cooldown / 12);
  player.dirty = false;
}

function renderEnemy(app) {
  if (!enemy.dirty) { return; }
  if (enemy.dead) {
    app.stage.removeChild(view.enemySprite);
    view.enemySprite = new Sprite(
      resources["sprites/dead-sprite.png"].texture
    );
    view.enemySprite.width = cellWidth();
    view.enemySprite.height = cellHeight();
    app.stage.addChild(view.enemySprite);
  }
  view.enemySprite.x = enemy.location.x * cellWidth();
  view.enemySprite.y = enemy.location.y * cellHeight();
  view.enemySprite.visible = true;
  if (enemy.location.x > player.location.x) {
    enemyTurnLeft();
  } else {
    enemyTurnRight();
  }
  app.stage.addChild(view.enemySprite);
  enemy.dirty = false;
}

function render(app) {
  // renderGrid(app);
  renderEnemy(app);
  renderPlayer(app);
}

function findPlayerData(ps, n) {
  let needles = ps.filter(p => p[0] == n);
  if (needles.length == 1) {
    return needles[0][1];
  } else {
    return null;
  }
}

function findEnemyData(ps, n) {
  let needles = ps.filter(p => p[0] != n);
  if (needles.length == 1) {
    return needles[0][1];
  } else {
    return null;
  }
}

function processInitializeGame(app, data) {
  view.gridSize = {
    x: data["params"][0], y: data["params"][1]
  };
  // Make the view size the largest multiple of grid size that fits in the window
  view.viewSize = {
    x: window.innerWidth - (window.innerWidth % view.gridSize.x),
    y: window.innerHeight - (window.innerHeight % view.gridSize.x)
  };
  initView(app, data["grid"]);
  state.grid = data["grid"];
}

function processPlayerMove(ws, data) {
  ws.send(JSON.stringify({
    tag: "PlayerMove",
    contents: {
      x: data.x, y: data.y
    }
  }));
}

function processPlayerState(data) {
  player.location = {
    x: data["pos"]["x"],
    y: data["pos"]["y"]
  };
  if (data["pos"] != data["dst"]) {
    player.destination = {
      x: data["dst"]["x"],
      y: data["dst"]["y"],
    };
  } else {
    player.destination = null;
  }
  player.cooldown = data["cooldown"];
  player.hp = data["hp"];
  player.dirty = true;
}

function processEnemyState(data) {
  enemy.location = {
    x: data["pos"]["x"],
    y: data["pos"]["y"]
  };
  enemy.dead = data["dead"];
  enemy.dirty = true;
}

function processServerState(data) {
  let p = data["player"];
  if (p != null) { processPlayerState(p); }
  let n = data["enemy"];
  if (n != null) { processEnemyState(n); }
}

function processInputEvent(app, ws, e) {
  // console.log(e);
  if (e.type == "initializegame") {
    processInitializeGame(app, e.data);
  } else if (e.type == "playermove") {
    processPlayerMove(ws, e.data);
  } else if (e.type == "serverstate") {
    processServerState(e.data);
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

function initGrid(app, grid) {
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
      if (grid[i][j] == "Wall") {
        sprite.visible = true;
      } else {
        sprite.visible = false;
      }
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
  view.playerSprite.visible = false;
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
  view.enemySprite.visible = false;
  app.stage.addChild(view.enemySprite);
}

function initView(app, grid) {
  app.renderer.resize(view.viewSize.x, view.viewSize.y);
  initGrid(app, grid);
  initPlayer(app);
  initEnemy(app);
}

function onMouseUp(ev) {
  input.push({
    type: "playermove",
    data: {
      x: Math.floor(ev.clientX/cellWidth()),
      y: Math.floor(ev.clientY/cellHeight())
    }
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
  player.name = random_name();
  ws.onopen = function (event) {
    ws.send(player.name);
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
    .add("sprites/dead-sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  load();
});
