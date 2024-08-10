// helper functions to get brain explorer app IDs from url
function getQueryVariable(variable) {
	var query = window.location.search.substring(1);
	var vars = query.split("&");
	for (var i=0;i<vars.length;i++) {
		var pair = vars[i].split("=");
		if(pair[0] == variable){return pair[1];}
	}
	return(false);
};

// get user_handle
if (window.location.search.indexOf('hash') > -1) {
	var userHash = getQueryVariable('hash');
}
else {
	var userHash = (Math.floor(Math.random() * (2000000 - 0 + 1)) + 0).toString();    // if no userHash enter a random number
};

// get attempt id
if (window.location.search.indexOf('attempthash') > -1) {
	var attemptHash = getQueryVariable('attempthash');
}
else {
	var attemptHash = (Math.floor(Math.random() * (2000000 - 0 + 1)) + 0).toString();    // if no attempthash enter a random number
};

// export ids: 
export { userHash, attemptHash }