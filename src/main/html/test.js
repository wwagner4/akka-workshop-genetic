$(function() {

// >50k
var code = "32301032310243113033224225502501232221321105101005421120400501101540434424043444444014444440044440544244444411440114403440440411"
// 48k
code = "35332105012310110103231331302111432341321013331025331320200343524444331423440442444444044434444134324444344434440442445534324304"

// 52k
code = "52332202011313220003031331002111121221321313322211311122200033224442444444405443444444442144444334442444444434444442424534223542"

// 35k
//code = "23352153202303510105211531332000353321235355531005320211030052110444341153441443444454044444424344331441344434444142442444423444"

// 42k
//code = "10332153312305353505531331042202350221212042351503525211550142411444222553042442444444444444444345032441344434444142442444425404"


var decisions = []
for (var i = 0; i < code.length; i++)
	decisions.push(parseInt(code[i]))


var game = new Game();
$('.field').html(game.render());

$('#next').click(function() {
    game.act(code)
    $('.field').html(game.render());
})

function Game()
{
    var mapping = [ 195,126,6,168,84,165,0,213,189,63,186,27,216,42,78,102,36,174,15,33,183,114,237,87,51,72,228,171,18,66,9,57,24,105,231,45,108,54,207,99,153,222,132,159,60,204,156,180,234,225,240,141,12,135,21,219,75,198,162,69,177,81,138,3,68,89,29,188,110,116,140,173,107,35,233,215,134,11,164,230,5,20,8,209,71,23,44,83,185,137,170,218,143,86,191,206,62,179,242,101,155,239,65,14,128,167,17,56,224,221,38,47,227,77,59,158,2,236,161,176,53,182,26,104,197,80,200,74 ]
    var reverseMapping = []
    $.map(mapping, function(s, i) { reverseMapping[s] = i })

	// create field
	this.fieldSize = 10
	this.itemCount = 50
	this.field = []
	for (var x = 0; x < this.fieldSize; x++)
		for (var y = 0; y < this.fieldSize; y++)
			this.field.push(false);
	
	// place items
	var placed = 0;
	while(placed < this.itemCount) {
		var loc = Math.floor(Math.random() * this.field.length);
		if (this.field[loc]) continue;
		this.field[loc] = true;
		placed++;
	}

	// robot location
	this.x = 0;
	this.y = 0;
	this.moves = 0;

	this.getIndex = function(x,y) { return y * this.fieldSize + x }

	// return cell info (0 = empty, 1= wall, 2=item)
	this.getCell = function(x,y) {
	    if (x < 0 || y < 0 || x >= this.fieldSize || y >= this.fieldSize)
	        return 1; // wall
	    else if(this.field[this.getIndex(x,y)])
	        return 2; // object
	    else
	        return 0; // empty
	}

	// act
	this.points = 0;
	this.act = function(code) {

	    if (this.moves >= 200) return;
        this.moves++;

	    // get situation
	    var sit = 0;
	    sit += this.getCell(this.x, this.y - 1) * 3 * 3 * 3 * 3
	    sit += this.getCell(this.x + 1, this.y) * 3 * 3 * 3
	    sit += this.getCell(this.x, this.y + 1) * 3 * 3
	    sit += this.getCell(this.x - 1, this.y) * 3
	    sit += this.getCell(this.x, this.y)

	    // get decision
	    var mapped = reverseMapping[sit]
		var decision = parseInt(code[mapped])

		// random move
		if (decision == 5) decision = Math.floor(Math.random() * 4);

		if (decision <= 3) {
			// move
			var delta;
			if (decision == 0) delta = [ 0, -1 ]
			else if (decision == 1) delta = [ 1, 0 ]
            else if (decision == 2) delta = [ 0, 1 ]
			else delta = [ -1, 0 ]
			var nextX = this.x + delta[0];
			var nextY = this.y + delta[1];

            // test move location
			if (this.getCell(nextX, nextY) == 1) {
			    this.points -= 5;  // move against wall
			} else {
			    // successful move
                this.x = nextX;
                this.y = nextY;
			}
		}
		else if(decision == 4) {
			// pick up
			var index = this.getIndex(this.x, this.y)
			if (this.field[index]) {
                this.points += 10;
                this.field[index] = false;
			}
			else
			    this.points -= 1;
		}
		else
			console.log("error")
	}

	// get html
	this.render = function() {
	    var robotIndex = this.getIndex(this.x, this.y)
	    var result = $.map(this.field, function(f, i) {
	        var fieldClass = f ? "item" : "empty"
	        var result = $('<span class="' + fieldClass + '">')
	        if (i == robotIndex) result.append($('<span class="robot">'))
	        return result;
	    })
	    return result;
	}

	console.log(this.field);
}



});
	

