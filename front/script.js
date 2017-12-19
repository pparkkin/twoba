var state = null;

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

function render(app, data) {
  let grid = data["grid"],
      height = grid.length,
      cellHeight = innerHeight / height,
      width = grid[0].length,
      cellWidth = innerWidth / width;
  if (state == null) {
    initState(width, height);
  }
  // printGrid(grid);

  grid.forEach(function(row, i) {
    row.forEach(function(cell, j) {
      if (cell == "Live") {
        if (state[i][j] == null) {
          let sprite = new Sprite(
            resources["sprites/sprite.png"].texture
          );
          sprite.width = cellWidth;
          sprite.height = cellHeight;
          sprite.x = j * cellWidth;
          sprite.y = i * cellHeight;
          state[i][j] = sprite;
          app.stage.addChild(sprite);
        }
      } else {
        if (state[i][j] != null) {
          let sprite = state[i][j];
          app.stage.removeChild(sprite);
          state[i][j] = null;
        }
      }
    });
  });
}

function initState(width, height) {
  state = [];
  for (var i = 0; i < height; i++) {
    state.push(new Array(width));
  }
}

function setup() {
  let app = new Application({
    backgroundColor: 0xffffff
  });
  app.renderer.view.style.position = "absolute";
  app.renderer.view.style.display = "block";
  app.renderer.view.style.backgroundColor = "white";
  app.renderer.autoResize = true;
  app.renderer.resize(window.innerWidth, window.innerHeight);
  document.body.appendChild(app.view);

  let ws = new WebSocket('ws://localhost:3000');
  ws.onmessage = function (event) {
    render(app, JSON.parse(event.data));
  };
  ws.onopen = function (event) {
    console.log("Socket open!");
  };
}

function load() {
  loader
    .add("sprites/sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  load();
});
