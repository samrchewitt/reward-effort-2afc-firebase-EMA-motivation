// helper functions to enable firebase authorisation

// sign in
import { initializeApp } from 'https://www.gstatic.com/firebasejs/9.4.0/firebase-app.js';
import { getAuth, signInAnonymously, onAuthStateChanged } from 'https://www.gstatic.com/firebasejs/9.4.0/firebase-auth.js';
import { getFirestore, enableIndexedDbPersistence } from 'https://www.gstatic.com/firebasejs/9.4.0/firebase-firestore.js';

// web app config 
// public version of this code has below secret api keys removed (fill in yours to connect with your firebase project)
//const firebaseConfig = {
  //  apiKey: "",
   // authDomain: "",
  //  databaseURL: "",
   // projectId: "",
   // storageBucket: "",
   // messagingSenderId: "",
   // appId: "",
   // measurementId: ""
//};

// initialize
const app = initializeApp(firebaseConfig);


const auth = getAuth(app);
// sign in anonymously 
signInAnonymously(auth)
    .then(() => {
        // Signed in..
    })
    .catch((error) => {
        const errorCode = error.code;
        const errorMessage = error.message;
        // ...
    });

var uid;
// when signed in, get user ID
onAuthStateChanged(auth, (user) => {
  if (user) {
       uid = user.uid;
  } 
});


const db = getFirestore(app);
// enable persistence 
enableIndexedDbPersistence(db)
    .catch((err) => {
        if (err.code == 'failed-precondition') {
            // Multiple tabs open, persistence can only be enabled
            // in one tab at a a time.
            // ...
        } else if (err.code == 'unimplemented') {
            // The current browser does not support all of the
            // features required to enable persistence
            // ...
        }
    });
// Subsequent queries will use persistence, if it was enabled successfully

export { uid, db }
