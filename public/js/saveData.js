// Helper functions for saving trial data [using firebase's firetore]

// import task version info
import { taskName, gameType, version, debug_mode, sceneOrder, thresholdAutoSet} from "./versionInfo.js";
import { db, uid} from "./firebaseInit.js";
import { collection, doc, setDoc, getDoc} from 'https://www.gstatic.com/firebasejs/9.4.0/firebase-firestore.js';
import { userHash, attemptHash} from "./getUserBEX.js";

// database collection path  
const dbRef = collection(db, taskName);

// function to save initial data at study start 
var saveInitial = function () {
    setDoc(doc(dbRef, version, userHash, attemptHash), {
        userHash: userHash,     // BEX user hash
        firebaseUID: uid,             // firebase UID 
        attemptHash: attemptHash,     // BEX attempt hash 
        sessionStartDate: new Date().toISOString().split('T')[0],
        sessionStartTime: new Date().toLocaleTimeString(),
        participantOS: navigator.userAgent,
        // also save the game info:
        taskName: taskName,
        gameType: gameType,
        version: version,
        taskOrder: sceneOrder,
        debugMode: debug_mode,
    })
};

// function to save start time of task 
var saveStartData = function (startTime, randomiseOrder, sceneOrder) {
    setDoc(doc(dbRef, version, userHash, attemptHash), {
        // add date here for ease of reference
        StartTimePhaser: startTime,
        // save the order config
        randomOrder: randomiseOrder,
        // and which task was first 
        task1: sceneOrder[0],
        // initialise the experiment
        expCompleted: 0
    }, { merge: true });
};

// function to save initial data
var saveStartTask = function (startTime) {
    setDoc(doc(dbRef, version, userHash, attemptHash), {
        // add date here for ease of reference
        StartTimeTask: startTime,
    }, { merge: true });
}

// function to save the pre-task questions data
var savePreTaskData = function (questN, dataToSave) {
    setDoc(doc(dbRef, version, userHash, attemptHash, 'quest', 'data'), { [questN]: dataToSave }, { merge: true });
};

// function to save the practice task data
var savePracTaskData = function (trialN, dataToSave) {
    setDoc(doc(dbRef, version, userHash, attemptHash, 'practice-data', 'data'), { [trialN]: dataToSave }, { merge:true });
};

// function to save the main task data
var saveTaskData = function (trialN, dataToSave) {
    setDoc(doc(dbRef, version, userHash, attemptHash, 'task-data', 'data'), { [trialN]: dataToSave }, { merge:true });
};

// function to save final data 
var saveEndData = function (dataBackup) {
    // save end time info and mark exp as complete 
    setDoc(doc(dbRef, version, userHash, attemptHash), {
        endTimeDB: new Date().toLocaleTimeString(),
        expCompleted: 1
    }, { merge: true });
    // data-dump in case of any issues
    setDoc(doc(dbRef, version, userHash, attemptHash, 'task-data', 'data-backup'), dataBackup)
    // update the expCompleted 
    setDoc(doc(dbRef, version, userHash, attemptHash), {
        expCompleted: 1
    }, { merge: true });
};

// function to shuffle trials
var shuffleTrials= function (nTrials, catchIdx, nCalibrates) {
    // Create a shuffled array of trial indices
    const trialIndices = Array.from(Array(nTrials).keys()).sort(() => Math.random() - 0.5);

    // condition: 
    // the catch trials (catchIdx) cannot be a recalibration trial (<nCalibrates)

    // Find the index of catchIdx
    const catchIdxIndex = trialIndices.indexOf(catchIdx);

    // If catchIdx is below nCalibrates, swap it with an alternative random value >= nCalibrates
    if (catchIdxIndex < nCalibrates) {
        // Find a random index from nCalibrates to nTrials
        const randomIndex = Math.floor(Math.random() * (nTrials - nCalibrates)) + nCalibrates;

        // Swap catchIdx with the random index
        [trialIndices[catchIdxIndex], trialIndices[randomIndex]] = [trialIndices[randomIndex], trialIndices[catchIdxIndex]];
    }

    return trialIndices;
}

// function to save the maxThreshold seperately within the user as a document
var saveThresholdMax = function (maxThreshold) {
    setDoc(doc(dbRef, version, userHash, "maxThreshold"), {
        // add date here for ease of reference
        maxThreshold,
    }, { merge: true });
}


// function to retrieve maxThreshold when the user played the game already
var thresholdMax;
var fetchThresholdMax = async function () {
    try {
        const docSnap = await getDoc(doc(dbRef, version, userHash, "maxThreshold"));
        thresholdMax = docSnap.get("maxThreshold")["thresholdMax"];
    } catch (error) {
        console.error('Error fetching maxThreshold:', error);
        thresholdMax = thresholdAutoSet;
    }
    return thresholdMax;
};



// debugging
if (debug_mode == true) {
    console.log("db = " + db)
    console.log("uid = " + uid)
    console.log("db ref = " + dbRef)
    console.log("userHash = " + userHash)
    console.log("attemptHash = " + attemptHash)
    //console.log("threshold max " + thresholdMax)
    //console.log("threshold max type: "+typeof thresholdMax);
}

// export functions for other scripts 
export {
    dbRef, userHash, 
    saveInitial, savePreTaskData, saveStartData, saveStartTask, saveThresholdMax,
    savePracTaskData, saveTaskData, saveEndData, shuffleTrials, fetchThresholdMax
}

