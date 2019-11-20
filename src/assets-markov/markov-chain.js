function getStates (num_states) {
    var states = [];
    for (i = 65; i < 65 + num_states; i++) {
        states.push(String.fromCharCode(i))
    }
    return states;
}

function getRandomProb (num_states) {
    var tmpArray = [];
    
    function arrSum (total, num) {
        return total + num;
    }
    
    for (i = 0; i < num_states; i++) {
        if (i != num_states - 1) {
            var tmpProb = Math.ceil(Math.random(0.01) * 100/(num_states-1)) / 100;
        } else {
            var tmpProb = Math.ceil((1 - tmpArray.reduce(arrSum)) * 100) / 100;
        }
        tmpArray.push(tmpProb);
    }
    return tmpArray;
}

function getTransMatrix (num_states) {
    var transMatrix = [];
    var tmp = 0;
    document.getElementById("transition_matrix").innerHTML = "";
    while (tmp < num_states) {
        transMatrix.push(getRandomProb(num_states));
        tmp++;
    }
    for (i = 0; i < num_states; i++) {
        var row = document.createElement("p");
        row.innerHTML = transMatrix[i].toString().replace(/[,]/g, "    ");
        document.getElementById("transition_matrix").appendChild(row);
    }
    return transMatrix;
}

function shuffle(a) {
    for (let i = a.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
}

function getChainMatrix (num_states) {
    var states = getStates(num_states);
    var transition_matrix = getTransMatrix(num_states);
    var selection_array = [];
    var selection_matrix = [];
    
    for (i = 0; i < num_states; i++) {
        for (j = 0; j < num_states; j++) {
            var tmpArr = [];
            tmpArr = states[j].repeat(transition_matrix[i][j] * 100).split("");
            selection_array = selection_array.concat(tmpArr);
        }
        selection_matrix.push(shuffle(selection_array));
        selection_array = [];
    }
    return selection_matrix;
}

function runChain (num_states, chain_length) {
    var states = getStates(num_states);
    var selection_matrix = getChainMatrix(num_states);
    var count = 0;
    var running = true;
    // Start off the chain with 1st state
    var state_index = 1;

    while (running) {
        var current_state = selection_matrix[state_index][Math.floor(Math.random() * selection_matrix[state_index].length)]
        console.log(current_state);
        state_index = states.indexOf(current_state);
        if (count < chain_length) {
            count++;
        } else {
            running = false;
        }
    }
}

// Get user input and start the
function markovWrapper () {
    var input = document.getElementById("num-states").value;
    var chain_length = document.getElementById("chain-length").value;
    runChain(input, chain_length);
}

// Reset page if user wants to stop the chain
function stopMarkov () {
    location.reload();
}