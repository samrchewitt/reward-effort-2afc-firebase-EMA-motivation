// End scene to inform participants they have finished the task, and route them back to Brain Explorer

// import js game element modules (sprites, ui, outcome animations)
import InstructionsPanel from "../elements/instructionsPanel.js";
// import nCoins running total  
// import { bonusRate, nTrials} from "../versionInfo.js";
import { saveEndData } from "../saveData.js";
import {
    bonusRate, complete_link,
    buttonText, maxCoins, completionBonus80, completionMin, demo_mode,
    runPractice
} from "../versionInfo.js";

// import our custom events centre for passsing info between scenes and data saving function
import eventsCenter from "../eventsCenter.js";

// this function extends Phaser.Scene and includes the core logic for the scene
export default class TaskEndScene extends Phaser.Scene {
    constructor() {
        super({
            key: 'TaskEndScene'
        });
    }

    preload() {
        // load cloud sprites to add texture to background
        this.load.image('cloud1', './assets/imgs/cloud1.png');
        // load medal 
        this.load.image('medal', './assets/imgs/icons8-medal-40.png');
        // load coin 
        this.load.spritesheet('coin', './assets/spritesheets/coin.png', {
            frameWidth: 10,
            frameHeight: 100
        });
        // // load player sprite
        this.load.spritesheet('player', './assets/spritesheets/player1.png', {
            frameWidth: 90,
            frameHeight: 96
        });
    }
    
    create() {
         // load a few cloud sprites dotted around
        var gameHeight = this.sys.game.config.height;
        var gameWidth = this.sys.game.config.width;
        // get the nCoins won this game 
        var nCoins = this.registry.get('CoinsRunningTotal');
        var score = (nCoins / maxCoins) * 100
        // set the endText message with a catch for undefined to prevent strange user experience 
        if (typeof nCoins === 'undefined') {
            // variable is undefined
          var endText = ('Thanks\n for\n playing!')
        } else {
        // variable is defined
            var endText = ('[img=medal]\n\nYou added\n\n' + nCoins + ' coins\n\n' +
                'to your\n\ntotal!\n');
        }

        // save task data backup
        saveEndData(this.registry.getAll());

        var titleText = 'Game Over!'
        ///////////////////PAGE ONE////////////////////
        var mainTxt = endText;
        var buttonTxt = 'next';
        var pageNo = 1;
        this.endPanel = new InstructionsPanel(this, gameWidth/2, gameHeight/2,
            pageNo, titleText, mainTxt, buttonTxt);

        ///////////////////PAGE TWO////////////////////
        eventsCenter.once('page1complete', function () {
            if (demo_mode == false & runPractice == false) {
                var titleText = 'Remember!'
                mainTxt = ('You will earn\n' +
                    '[b]\u00A3' + completionBonus80 +
                    ' + coins (' + bonusRate + 'p each)\n\n' + 'if ' +
                    'you complete \nat least ' + completionMin + '%[/b]\n' +
                    'of the notifications.\n\nSee you\n\nnext time\nThank you!\n')
            }
            else if (demo_mode == false & runPractice == true) {
                var titleText = 'Part 2 begins tomorrow!';
                mainTxt = ('You will earn\nat least\n' +
                    '\u00A3' + completionBonus80 +
                    ' + coins (' + bonusRate + 'p each)\n\n' + '[b]if ' +
                    'you complete \nat least ' + completionMin + '%[/b]\n' +
                    'of the notifications\n\nPlease click\n[b]Return to Prolific[/b]\n' +
                    'on the next page\nto receive payment\nfor Part 1!\n\n')
            } else {
                var titleText = 'Game over!'
                mainTxt = ('Thanks for\n' +
                'playing\n');
            };

            buttonTxt = buttonText;
            pageNo = 2;
            this.instructionsPanel = new InstructionsPanel(this,
                gameWidth / 2, gameHeight / 2,
                pageNo, titleText, mainTxt, buttonTxt);
        }, this);

        // end scene
        eventsCenter.once('page2complete', function () {
            if (runPractice == false) {
                // for FU games without a practice, don't display prolific complete link
                BeApp.postMessage(JSON.stringify({
                    "type": "back",
                    "backPayload": {
                        "success": true,
                        "score": 123, //test if score var was the problem 
                        "showShareButton": false,
                        "showPlayAgainButton": false,
                        "goodbyeMessage": {
                            "localizations": {
                                "en": "All done! You can close this app now"
                            }
                        }
                    }
                }))
            }
            else {
                // for baseline games, display the complete link in postGame resources
                BeApp.postMessage(JSON.stringify({
                    "type": "back",
                    "backPayload": {
                        "success": true,
                        "score": 123, //test if score var was the problem
                        "showShareButton": false,
                        "showPlayAgainButton": false,
                        "postGameResources": [
                            {
                                "buttonText": {
                                    "localizations": {
                                        "en": "Return to Prolific!"
                                    }
                                },
                                "urlDestination": complete_link
                            }
                        ],
                        "goodbyeMessage": {
                            "localizations": {
                                "en": "Please confirm your Prolific login on the next page, then simply close this app. Thanks!"
                            }
                        }
                    }
                }))
            }
        }, this);
    }
    
    update(time, delta) {
    }
    
    nextScene() {
    }
}
