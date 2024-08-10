//make popup dialog box with instructions and number bar for participant to enter ratings
export default class QuestionPanel {
    constructor(scene, x, y, gamePhase, questionNo, mainTxt, responses) {
    this.scene = scene;
    
    var buttonTxt = 'select';    

    var mainPanel = createMainPanel(scene, gamePhase, questionNo, mainTxt, buttonTxt, responses)
        .setPosition(x,y)
        .layout()
        //.drawBounds(scene.add.graphics(), 0xff0000) //for debugging only
        .popUp(500); 
    }
    
}

////////////////////functions for making in-scene graphics//////////////////////////
///////////main panel////////////
var createMainPanel = function (scene, gamePhase, questionNo, mainTxt, buttonTxt, responses) {
    // create global registry var to pass ratings data between scenes
    scene.registry.set(gamePhase+'question'+questionNo, []);
    // create panel components
    var dialog = createDialog(scene, gamePhase, questionNo, mainTxt, buttonTxt);
    var slider = createNumberBar(scene, responses);
    var mainPanel = scene.rexUI.add.fixWidthSizer({
        orientation: 'x' //vertical stacking
        }).add(
            dialog, // child
            0, // proportion
            'center', // align
            0, // paddingConfig
            false, // expand
        )
        .add(
            slider, // child
            0, // proportion
            'center', // align
            0, // paddingConfig
            true, // expand
        )
    .layout();
    
    slider
    .once('valuechange', function () {
        //only allow answer to be entered once ppt has interacted with slider
        dialog
            .once('button.click', function (button, groupName, index) {
                let answer = Math.round(slider.getValue(0, 6));   // get final slider value
                scene.registry.set(gamePhase+'question'+questionNo, {questionStage: gamePhase,
                                                                     questionNo: questionNo,
                                                                     question: mainTxt,
                                                                     answer: answer});
                dialog.scaleDownDestroy();            // destroy ratings panel components
                slider.scaleDownDestroy();            // destroy ratings panel components
                scene.events.emit(gamePhase+'question'+questionNo+'complete');   // emit completion event
            }, this)
            .on('button.over', function (button, groupName, index) {
                button.getElement('background').setStrokeStyle(2, 0xffffff); // when hover
            })
            .on('button.out', function (button, groupName, index) {
                button.getElement('background').setStrokeStyle();
            });
    });
    
    return mainPanel;
};

///////////popup dialog box//////
var createDialog = function (scene, gamePhase, questionNo, mainTxt, buttonTxt) {
    var textbox = scene.rexUI.add.dialog({
    background: scene.rexUI.add.roundRectangle(0, 0, 300, 600, 20, 0x815532),
    
    title: scene.rexUI.add.label({
        background: scene.rexUI.add.roundRectangle(0, 0, 75, 60, 20, 0xf57f17),
        text: scene.add.text(0, 0, 'Slide the bar below!', {
            fontSize: '20px'
            }),
        align: 'center',
        space: {
            left: 15,
            right: 15,
            top: 10,
            bottom: 10
        }
    }),

    content: scene.rexUI.add.BBCodeText(0, 0, mainTxt, {
        fontSize: '20px',
        font: '20px monospace',
        align: 'center'
    }),

    actions: [
        createLabel(scene, buttonTxt)
    ],

    space: {
        title: 25,
        content: 20,
        action: 30,
        left: 10,
        right: 10,
        top: 10,
        bottom: 30,
    },
        
    align: {
        actions: 'center',
    },

    expand: {
        content: false, 
    }
    })
    .layout();
    
    return textbox;
};

/////////button labels////////////////////////////
var createLabel = function (scene, text) {
    return scene.rexUI.add.label({
        // made buttons larger for mobile 
        background: scene.rexUI.add.roundRectangle(0, 0, 0, 100, 20, 0xf57f17),
        text: scene.add.text(0, 0, text, {
            fontSize: '24px'
        }),
        align: 'center',
        width: 100,
        space: {
            left: 10,
            right: 10,
            top: 40,
            bottom: 40
        }
    });
};

////////number bar//////////////////////////////////
var createNumberBar = function (scene, responses) {
    var numberBar = scene.rexUI.add.numberBar({ 
        width: 280,                 // fixed width slider
        orientation: 'horizontal',

        background: scene.rexUI.add.roundRectangle(0, 0, 0, 0, 10, 0x815532),
        // remove icon as I think this is confusing for questions
        //icon: scene.rexUI.add.roundRectangle(0, 0, 0, 50, 10, 0xf57f17),

        slider: {
            track: scene.rexUI.add.roundRectangle(0, 0, 0, 60, 0, 0x7b8185),
            indicator: scene.rexUI.add.roundRectangle(0, 0, 0, 55, 0,  0xf57f17),
            input: 'click',
        },

        text: scene.rexUI.add.BBCodeText(0, 0, '', {
            fontSize: '20px', fixedWidth: 110, fixedHeight: 70,
            valign: 'top', halign: 'left'
        }),

        space: {
            left: 10,
            right: 10,
            top: 10,
            bottom: 10,
            //icon: 10,
            slider: 10,
        },

        // valuechangeCallback here
        valuechangeCallback: function (value, oldValue, numberBar) {

        // convert the number to text
            var responseIndex = Math.round(Phaser.Math.Linear(0, responses.length - 1, value));

            var response = responses[responseIndex];
            var responseValue = response.value;

            numberBar.text = response.text;
            return value;
        },
        // set a random starting value to avoid bias
    }).setValue((Math.floor(Math.random() * ((responses.length - 1) - (responses.length - responses.length + 1) + 1)) + (responses.length/2)), 0, responses.length) // starting point is random between 2-5
    .layout();
    
    return numberBar;
};