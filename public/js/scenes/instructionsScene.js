// Scene to hold multi-page instructions text

// import js game element modules (sprites, ui, outcome animations)
import InstructionsPanel from "../elements/instructionsPanel.js";

// import our custom events centre for passsing info between scenes and data saving function
import eventsCenter from "../eventsCenter.js";
import { saveStartTask } from "../saveData.js";
import { sceneOrder } from "../task.js";


// initialize global start time var
var taskStartTime;

// this function extends Phaser.Scene and includes the core logic for the scene
export default class InstructionsScene extends Phaser.Scene {
    constructor() {
        super({
            key: 'InstructionsScene',
            autoStart: true
        });

    }

    preload() {
        // load cloud sprites to add texture to background
        this.load.image('cloud1', './assets/imgs/cloud1.png');
        // load button and coin sprites
        this.load.image('button', './assets/imgs/button.png');
        this.load.spritesheet('coin', './assets/spritesheets/coin.png', { 
            frameWidth: 15.8, 
            frameHeight: 16 
        });
    }
    
    create() {
        // add listener to resize 
        // resizeApp();
        //window.addEventListener('resize', resizeApp);
        // load a few cloud sprites dotted around
        const cloud1 = this.add.sprite(180, 100, 'cloud1');
        const cloud2 = this.add.sprite(320, 540, 'cloud1');
        const cloud3 = this.add.sprite(630, 80, 'cloud1');
        
        var gameHeight = this.sys.game.config.height;
        var gameWidth = this.sys.game.config.width;
        
        var titleText = 'Welcome!'
        var WarningText='Caution!'
        taskStartTime = Math.round(this.time.now);
        
        // let's do this the long-winded way for now...[should make this a function]
        ///////////////////PAGE ONE////////////////////
        var mainTxt = (" You are travelling\n" +
                        "in a strange\n"+
                        "land of rivers\n"+
                        "and streams...\n\n");
        var buttonTxt = "next";
        var pageNo = 1;
        this.instructionsPanel = new InstructionsPanel(this, 
                                                       gameWidth/2, gameHeight/2,
            pageNo, titleText, mainTxt, buttonTxt);

        ///////////////////PAGE TWO////////////////////
        eventsCenter.once('page1complete', function () {
            mainTxt = ("Use your\n" +
                "[b]magic umbrella[/b]\n" +
                "to fly across\n" +
                "the ravine!\n\n");
            pageNo = 2;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, titleText, mainTxt, buttonTxt);
        }, this);

        ///////////////////PAGE THREE////////////////////
        eventsCenter.once('page2complete', function () {
            mainTxt = ("At each crossing,\n" +
                "you can [color=#d0f4f7]choose[/color] between\n" +
                "[color=#d0f4f7]two[/color] routes.\n\n" +
                "There is no correct\n" +
                "correct answer.\n\n" +
                "It's up to you!\n\n")
            pageNo = 3;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, titleText, mainTxt, buttonTxt);
        }, this);

        
        ///////////////////PAGE FOUR////////////////////
        eventsCenter.once('page3complete', function () {
            mainTxt = ("Routes have different\n\n"+
                        "[img=coin] [color=#FFD700]coins[/color] [img=coin]\n\n" +
                        "At the end, coins\n" +
                        "are added to\n" +
                "your potential\n" +
                "bonus pot\n\n")
            pageNo = 4;
            this.instructionsPanel = new InstructionsPanel(this, 
                                                           gameWidth/2, gameHeight/2,
                                                           pageNo, titleText, mainTxt, buttonTxt);
        }, this);

        ///////////////////PAGE FIVE////////////////////
        eventsCenter.once('page4complete', function () {
            mainTxt = ("[b]However[/b],\n" +
                "the routes\n" +
                "require different\n\n" +
                "[img=button] [color=#e45404]POWER[/color] [img=button]\n\n")
            pageNo = 5;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, titleText, mainTxt, buttonTxt);
        }, this);

        ///////////////////PAGE SIX////////////////////
        eventsCenter.once('page5complete', function () {
            mainTxt = ("Power your umbrella \n\n" +
                "by pressing \n\n" +
                "[img=button] [color=#e45404]POWER[/color] [img=button]\n\n" +
                "as fast as you can\n\n");
            pageNo = 6;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, titleText, mainTxt, buttonTxt);
        }, this);


        ///////////////////PAGE SEVEN////////////////////
        eventsCenter.once('page6complete', function () {
            mainTxt = ("Some routes\n\n" +
                "require much less\n\n" +
                "power.\n\n\n" +
                "Read the route\n\n" +
                "info carefully and\n\n" +
                "[b]adjust your power\n\n" +
                "to the route\n\n" +
                "of your choice[/b]!\n\n");
            pageNo = 7;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, WarningText, mainTxt, buttonTxt);
        }, this);

        ///////////////////PAGE EIGHT////////////////////
        eventsCenter.once('page7complete', function () {
            mainTxt = ("Hold your\n\n"+
                "phone in\n\n"+
                "portrait mode,\n\n" +
                "and use your\n\n" +
                "dominant thumb or\n\n" +
                "finger to power!\n\n");
            pageNo = 8;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, WarningText, mainTxt, buttonTxt);
        }, this);

        ///////////////////PAGE NINE////////////////////
        eventsCenter.once('page8complete', function () {
            mainTxt = ("Do not\n\n" +
                "leave the\n\n" +
                "game window during\n\n" +
                "your journey!\n\n")
            pageNo = 9;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, WarningText, mainTxt, buttonTxt);
        }, this);
        
        ///////////////////PAGE TEN////////////////////
        eventsCenter.once('page9complete', function () {
            mainTxt = ("Each route is\n" +
                        "[b]your choice![/b]\n\n" +
                        "You should\n" +
                        "reduce your speed\n" +
                        "on easier routes\n" +
                        "for a smoother ride!\n\n"+
                        "Before we start,\n" +
                        "let's try\n"+
                        "powering the umbrella\n\n");
            buttonTxt = "Start!"
            pageNo = 10;
            this.instructionsPanel = new InstructionsPanel(this, 
                                                           gameWidth/2, gameHeight/2,
                                                           pageNo, "Remember!", mainTxt, buttonTxt);
            }, this);
        
        // end scene
        eventsCenter.once('page10complete', function () {
            this.nextScene();
            }, this);
    }
        
    update(time, delta) {
    }
    
    nextScene() {
        saveStartTask(taskStartTime);           // [for firebase]
        this.scene.start('PracticeTask');
    } 
}
