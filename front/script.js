var data = {"grid":
  [["Dead","Dead","Dead","Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Live","Dead","Live","Dead","Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Live","Dead","Dead","Dead","Live","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Live","Dead","Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Live","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Live","Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Live","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Live","Dead","Live","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Live","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"],
   ["Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead","Dead"]]}

//Aliases
let Application = PIXI.Application,
    loader = PIXI.loader,
    resources = PIXI.loader.resources,
    Sprite = PIXI.Sprite;

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


  let grid = data["grid"],
      height = grid.length,
      cellHeight = innerHeight / height,
      width = grid[0].length,
      cellWidth = innerWidth / width;
  for (var i = 0; i < grid.length; i++) {
    for (var j = 0; j < grid[i].length; j++) {
      if (grid[i][j] == "Live") {
        let sprite = new Sprite(
          resources["sprites/sprite.png"].texture
        );
        app.stage.addChild(sprite);
        sprite.width = cellWidth;
        sprite.height = cellHeight;
        sprite.x = j * cellWidth;
        sprite.y = i * cellHeight;
      }
    }
  }
}

function load() {
  loader
    .add("sprites/sprite.png")
    .load(setup);
}

document.addEventListener("DOMContentLoaded", function(event) {
  // ws = new WebSocket('ws://localhost:3000');
  // ws.onmessage = function (event) {
  //   console.log(event.data);
  // };
  // ws.onopen = function (event) {
  //   console.log("Socket open!");
  //   ws.send("Hello, Server!")
  // };

  load();
});
