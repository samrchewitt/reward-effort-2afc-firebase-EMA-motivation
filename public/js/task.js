// import js modules that hold the game/experiment scenes
import InstructionsScene from "./scenes/instructionsScene.js";
import practiceTask from "./scenes/practiceTask.js";
import questInstructionsScene from "./scenes/questInstructionsScene.js";
import Questions from "./scenes/Questions.js";
import StartTaskScene from "./scenes/startTaskScene.js";
import MainTask from "./scenes/mainTask.js";
import TaskEndScene from "./scenes/taskEndScene.js";
import { debug_mode, randomiseOrder, sceneOrder } from "./versionInfo.js";
import { saveStartData } from "./saveData.js";
// log the scene order for checking
if (debug_mode) { console.log('scene order: ' + sceneOrder) };

// create the phaser game, based on the following config
const config = {
    type: Phaser.Scale.AUTO,           // rendering: webGL if available, otherwise canvas
    width: 400,
    height: 780,
    physics: {
        default: 'arcade',       // add light-weight physics to our world
        arcade: {
            gravity: { y: 600 }, // need some gravity for a side-scrolling platformer
            debug: false         // TRUE for debugging game physics, FALSE for deployment
        }
    },
    parent: 'game-container',    // ID of the DOM element to add the canvas to
    dom: {
        createContainer: true    // to allow text input DOM element
    },
    backgroundColor: "#d0f4f7",  // pale blue sky color [black="#222222"],
    scene: sceneOrder.map(sceneName => eval(sceneName)),         // construct the experiment from componenent scenes
    plugins: {
        scene: [{
            key: 'rexUI',
            plugin: rexuiplugin,  // load the rexUI plugins here for all scenes
            mapping: 'rexUI'
        }]
    },
    scale: {
        parent: 'game-container',
        mode: Phaser.Scale.FIT,
        // Center vertically and horizontally
        autoCenter: Phaser.Scale.CENTER_BOTH
    }
};

// if desired, allow game window to resize to fit available space 
function resizeApp() {
    var canvas = document.querySelector("canvas");
    var windowWidth = window.innerWidth;
    var windowHeight = window.innerHeight;
    var windowRatio = windowWidth / windowHeight;
    var gameRatio = config.width / config.height;

    if (windowRatio < gameRatio) {
        canvas.style.width = windowWidth + "px";
        canvas.style.height = (windowWidth / gameRatio) + "px";
    }
    else {
        canvas.style.width = (windowHeight * gameRatio) + "px";
        canvas.style.height = windowHeight + "px";
    };
}

// wrap game creation in a function so that it isn't created until consent completed
export function runTask() {
    // create new phaser game configured as above
    new Phaser.Game(config);
    resizeApp()
    window.addEventListener("resize", resizeApp) //resize if necessary
    var currentTime = new Date().toLocaleTimeString();
    saveStartData(currentTime, randomiseOrder, sceneOrder);           // [for firebase] update !!
};

export { resizeApp, sceneOrder}