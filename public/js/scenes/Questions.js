import QuestionPanel from "../elements/questionPanel.js";
import { savePreTaskData } from "../saveData.js";
import { questionsFile, debug_mode, sceneOrder } from "../versionInfo.js";


export default class Questions extends Phaser.Scene {
    constructor() {
        super({
            key: 'Questions'
        });
    }

    preload() {
        // load trial type info from json array
        this.load.json('questions', './assets/' + questionsFile);
    }

    create() {
        // get questions from cache
        let questions_list = this.cache.json.get("questions");

        var gameHeight = this.sys.game.config.height;
        var gameWidth = this.sys.game.config.width;

        var gamePhase = "EMA";
        var questionNumber = 1;

        function generateQuestion() {
            let question = questions_list.questions[questionNumber - 1];
            // debugging
            if (debug_mode == true) {
                console.log('questions_list = ' + questions_list);
                console.log('question = ' + question.text);
                console.log('number of questions ' + questions_list.questions.length);
                console.log('response idx 1 text = ' + question.responses[1].text);
                console.log('response idx 1 value = ' + question.responses[1].value);
            }
            let mainTxt = question.text;
            // updated attention check items 29 Aug 2023 (see Zorowitz et al., 2023 recommendations)
            // if the question is an attention check question; randomise the required response
            //if (mainTxt.includes('Attention')) {
                // get idx of the random response
               // let randomResponse = question.responses[Math.floor(Math.random() * question.responses.length)];
                // replace the question text with that answer
               // mainTxt = mainTxt.replace('X', randomResponse.text);
            //};
            this.QuestionPanel = new QuestionPanel(
                this,
                gameWidth / 2,
                gameHeight / 2,
                gamePhase,
                questionNumber,
                mainTxt,
                question.responses
            );
            this.events.once(gamePhase + 'question' + questionNumber + 'complete', function () {
                // save the data first 
                let questionData = this.registry.get(`${gamePhase}question${questionNumber}`);
                if (questionData) {
                    savePreTaskData(questionNumber, questionData);
                }
                // move to the next question
                questionNumber++;
                if (questionNumber <= questions_list.questions.length) {
                    generateQuestion.call(this);
                } else {
                    // move onto the next scene 
                    this.nextScene();
                };
            }, this);
        };

        generateQuestion.call(this);
    }

    update(time, delta) {

    }

    // load the next scene based on the scene order 
    nextScene() {
        const currentIndex = sceneOrder.indexOf(this.scene.key);
        if (currentIndex < sceneOrder.length - 1) {
            const nextSceneKey = sceneOrder[currentIndex + 1];
            this.scene.start(nextSceneKey);
        }
    }
}