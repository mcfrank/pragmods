// PRAGMODS_BASELINE
// Overview: 
//      (1) Helper
//      (2) Parameters and Stimulus Setup 
//      (3) Control Flow

// ---------------- 1. HELPER ------------------
// random function
function random(a,b) {
    if (typeof b == "undefined") {
	a = a || 2;
	return Math.floor(Math.random()*a);
    } else {
	return Math.floor(Math.random()*(b-a+1)) + a;
    }
}

// random function
function range(a,b) {
    var rangeArray = new Array();

    for (var i=a; i<=b; i++) {
	rangeArray.push(i);
    }

    return rangeArray;
}

// unique function
function unique(arrayName)
{
    var newArray=new Array();
    label:for(var i=0; i<arrayName.length;i++ )
    {  
	for(var j=0; j<newArray.length;j++ )
	{
	    if(newArray[j]==arrayName[i]) 
		continue label;
	}
	newArray[newArray.length] = arrayName[i];
    }
    return newArray;
}

// shuffle function
function shuffle (a) 
{ 
    var o = [];
    for ( var i=0; i < a.length; i++) { o[i] = a[i]; }
    for (var j, x, i = o.length; i; j = parseInt(Math.random() * i), 
	 x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

// show slide function
function showSlide(id) {
    $(".slide").hide(); //jquery - all elements with class of slide - hide
    $("#"+id).show(); //jquery - element with given id - show
}


// create HTML for property matrix and base image
function stimHTML(base,n,prop_mat,props,id) {
    var html = "";

    html += '<img src="images2/' + base + '-base' + String(n+1) +
	'.png" width=200px height=200px alt="' + base + '" id="' + id + 'Image"/>';

    var c = 0;
    for (var p = 0; p < prop_mat.length; p++) {
	if (prop_mat[p] == 1) {
	    html += '<img  src="images2/' + base + '-' + props[p] + 
		'.png" width=200px height=200px alt="' + props[p] + '" ' +
		'id="' + id + 'Property' + String(c+1) + '"/>';
	    c = c + 1; // keep count of how many properties we've stacked
	}
    }

    return html;
}


// ---------------- 2. STIMULUS SETUP ------------------
// Condition - call the maker getter to get the cond variable 
var filename = "MCF_pragmods_v5"
var condCounts = "1,75;2,75;3,75;4,75;5,75;6,75;7,75;8,75"
var xmlHttp = null;
xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", "http://langcog.stanford.edu/cgi-bin/subject_equalizer/maker_getter.php?conds=" + 
	      condCounts +"&filename=" + filename, false );
xmlHttp.send( null );
var cond = xmlHttp.responseText;

// CROSS CONDITIONS

// familiarization conditions
var fam_dists = [[0, 1, 2, 2, 2, 2, 2, 2, 2],
		 [0, 1, 1, 1, 2, 2, 2, 2, 2],
		 [0, 1, 1, 1, 1, 1, 2, 2, 2],
		 [0, 1, 1, 1, 1, 1, 1, 1, 2]];
var target_frequencies = [.125,.375,.625,.875];
var num_fam = 9; 
var num_fam_conds = 4;

// THIS EXPERIMENT VERSION SPECIFIC

if (cond < num_fam_conds + 1) {
    var word_cond = "listener";
    var fam_cond = cond - 1; // subtract 1 to convert from 1 to 0
} else if (cond >= num_fam_conds) {
    var word_cond = "salience";
    var fam_cond = cond - (num_fam_conds) - 1; // subtract 1 to convert from 1 to 0 
}

var fam_dist = fam_dists[fam_cond];

// bookkeeping variables
var choices = [0, 1, 2];
var target = -2;
var fam_clicked = new Array();
var fam_finished;
var positions = ["left","middle","right"];

// ADAPTED FROM PRAGMODS R CODE
var expt = [[1, 0, 0], [1, 1, 0], [0, 1, 1]];
var choice_names_unpermuted = ["foil","target","logical"];
var level = 2;
var target_unpermuted = 1;
var distractor_unpermuted = 2;
var other_unpermuted = 0;
var target_prop_unpermuted = 1;
var distractor_prop_unpermuted = 0;

var stims = ["bike","friend","pizza","snowman","sundae","Christmas tree"];
var stims_plural = ["bikes","friends","pizzas","snowmen","sundaes","Christmas trees"];
var stims_props = [["basket","waterbottle","saddlebag"],
		   ["hat","glasses","mustache"],
		   ["mushrooms","olives","peppers"],
                   ["hat","scarf","mittens"],		   
                   ["cherry","whipped cream","chocolate"],
		   ["lights","ornaments","star"]];
var stims_prop_words = [["a basket","a water bottle","saddlebags"],
			["a hat","glasses","a mustache"],
			["mushrooms","olives","peppers"],
                        ["a hat","a scarf","mittens"],
                        ["a cherry","whipped cream","chocolate sauce"],
			["lights","ornaments","a star"]];
var stims_actions = [["ride","rents","rode"],
		     ["visit","chooses to visit","visited"],
		     ["eat","orders","ate"],
		     ["decorate","makes","decorated"],
		     ["eat","makes","ate"],
		     ["trim","buys","trimmed"]];
var stims_times = [["weekend","Week"],
		   ["Sunday","Week"],
		   ["Wednesday","Week"],
		   ["winter","Year"],
		   ["Friday","Week"],
		   ["Christmas","Year"]];
var img_size = 200; // needs to be implemented, currently just a placeholder

    
var stim_index = random(0,stims.length-1);

// Permute the matrix randomly:
var prop_perm = shuffle(range(0,expt[0].length-1));
var target_perm = shuffle(range(0,expt.length-1));
var expt_perm = new Array();
var choice_names = new Array();

for (var i=0; i<expt.length; i++) {
    expt_perm[i] = new Array()
    for (var j=0; j<expt[0].length; j++) {
	expt_perm[i][j] = expt[target_perm[i]][prop_perm[j]];
    }
    choice_names[i] = choice_names_unpermuted[target_perm[i]];
}

var base = stims[stim_index];
var plural = stims_plural[stim_index];
var actions = stims_actions[stim_index];
var props = stims_props[stim_index];
var prop_words = stims_prop_words[stim_index];
var times = stims_times[stim_index];

var target = target_perm.indexOf(target_unpermuted);
var distractor = target_perm.indexOf(distractor_unpermuted);
var other = target_perm.indexOf(other_unpermuted);
var target_prop = prop_perm.indexOf(target_prop_unpermuted);
var distractor_prop = prop_perm.indexOf(distractor_prop_unpermuted);

// create shuffled familiarization
fam_mat = new Array();
fam_perm = shuffle(range(0,fam_dist.length-1))
for (var i = 0; i < num_fam; i++) {
    fam_mat[i] = new Array();
    for (var j = 0; j < expt[0].length; j++) {
	fam_mat[i][j] = expt[fam_dist[fam_perm[i]]][prop_perm[j]];
    }
}


// ---------------- 3. CONTROL FLOW ------------------
// PRE-LOAD IMAGES
var base_image_pl = new Array();
for (i=0; i<3; i++) {
    base_image_pl[i] = new Image()
    base_image_pl[i].src = "images2/" + base + "-base" + String(i+1) + ".png" 
}

var props_image_pl = new Array() // By creating image object and setting source, images preload
for (i=0;i<props.length;i++) {
    props_image_pl[i] = new Image()
    props_image_pl[i].src = "images2/" + base + "-" + props[i] + ".png" 
} 

showSlide("instructions");

// MAIN EXPERIMENT
var experiment = {
     // stimulus variables - bookkeeping, send these to turk
    item: base,
    target_property: props[target_prop],
    target_frequency: target_frequencies[fam_cond],
    target_position: positions[target],
    choice: "null",
    choice_correct: "FALSE",
    familiarization_cond: fam_cond, 
    word_condition: word_cond,
    
    // response variables
    manip_check_target: 0,
    manip_check_dist: 0,
    name_check_correct: "FALSE",
    about: "",
    comment: "",

    // FAMILIARIZATION DISPLAY FUNCTION
    next_familiarization: function() {
        // Allow experiment to start if it's a turk worker OR if it's a test run
	if (window.self == window.top | turk.workerId.length > 0) {
	    // FAMILIARIZATION INSTRUCTIONS
	    var familiarization_html = '<p class="block-text"">Bob really likes to ' + 
		actions[0] + ' ' + plural + '. ' +
		'Every ' + times[0] + ' he ' + actions[1] + ' a ' + base + '.<p>' +
		'<p class="block-text">Click on each ' + times[1].toLowerCase() + ' to see the ' + base + ' Bob ' + actions[2] + '.</p>'
    	    $("#familiarizationText").html(familiarization_html) 
	    
	    // TIME BY TIME POPUPS FOR FAMILIARIZATION
	    var fam_objects_html = '<table align="center"><tr>'
	    
	    for (i=0;i<=num_fam-1;i++){
		fam_objects_html += '<td width=200px height=230px align="center" ' +
		    'class="objTable"' + 
		    'id="famTable' + String(i) + 
		    '" onclick="experiment.reveal(' + String(i) + ')">'
		
		fam_objects_html += times[1] + ' ' + String(i+1) + ', Bob ' + actions[2] + ':<div id="day' + String(i) + '"> </div>'
		
		if ((i+1)%3 == 0) {
		    fam_objects_html += "</tr><tr>"
		}
	    }
	    
	    fam_objects_html += '</tr></table>'
	    $("#famObjects").html(fam_objects_html) 
    	    showSlide("prestage");	
	}
    },

    // MAIN DISPLAY FUNCTION
    next_test: function() {

    	showSlide("stage");	
	

	// CREATE SETUP
	var setup_html = '<p class="block-text">Take a look at these ' + plural + '!</p>'
	$("#setup").html(setup_html) 	

	// CREATE OBJECT TABLE
	// (tr=table row; td= table data)
	var objects_html = '<table align="center"><tr>'

	for (i=0;i<3;i++){
	    objects_html += '<td width=198px height=210px align="center"' + 
		' class="unchosen objTable" ' +
		'id="tdchoice' + String(i) + '" ' +
		'onclick=\"experiment.select(' + String(i) + ')\">'

	    objects_html += stimHTML(base,i,expt_perm[i],props,'obj')
	    objects_html += '</td>'
	}

	objects_html += '</tr><tr>'
	objects_html += '</tr></table>'
	
	$("#objects").html(objects_html) 


	// CREATE MANIPULATION CHECK COMMON GROUNDING
	var manipCheck_html = "";

	if (random(0,1) == 0) { // randomize order, this is annoyingly long but easy
	    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
		prop_words[target_prop] + '?' + '  <input type="text" id="manipCheckTarget" ' + 
		'name="manipCheckTarget" size="1"></p>';
	    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
		prop_words[distractor_prop] + '?' + 
		'  <input type="text" id="manipCheckDist" ' + 
		'name="manipCheckDist" size="1"></p>'	
	} else {
	    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
		prop_words[distractor_prop] + '?' + 
		'  <input type="text" id="manipCheckDist" ' + 
		'name="manipCheckDist" size="1"></p>'	
	    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
		prop_words[target_prop] + '?' + '  <input type="text" id="manipCheckTarget" ' + 
		'name="manipCheckTarget" size="1"></p>';
	} 

	$("#manipCheck").html(manipCheck_html) 	

	// CREATE CHOICE TEXT ETC
    	var label_html = '<br><br><p class="block-text">Bob says: '

	if (word_cond == "listener") {
	    label_html += '<p class="block-text style="font-size:x-large;">' + 
		'"My favorite ' + base + ' has <b>' + 
		prop_words[target_prop] + 
		'."</b></p>'
	} else if (word_cond == "salience") {
	    label_html += '<p class="block-text style="font-size:x-large;">' + 
		'"The friend I most like to ' + actions[0] + 
		' has <b>mumblemumble."</b></p>' + 
		'<p class="block-text style="font-size:small;">' + 
		'(You couldn\'t hear what he said.)</p>'
	}

	label_html += '<p class="block-text">Click on the ' + base + 
	    ' that you think Bob is referring to.</p>'
    	$("#labelInst").html(label_html) 
    	
		
    },

    // SELECT FUNCTION (called in stage slide)
    select: function (c) {
	experiment.choice = choice_names[c];

	// unchoose everything
	for (var i=0; i<choices.length; i++) {
	    $("#tdchoice" + String(i)).removeClass('chosen').addClass('unchosen')
	}
	// choose this one
	$("#tdchoice" + String(c)).removeClass('unchosen').addClass('chosen')

    },

    // REVEAL IMAGES IN FAMILIARIZATION
    reveal: function(n) {

	day_html = stimHTML(base,fam_dist[fam_perm[n]],fam_mat[n],props,'obj')
	
	$("#day" + String(n)).html(day_html) 
	fam_clicked = unique(fam_clicked.concat(n))
	
	if (fam_clicked.length == num_fam) {
	    fam_finished = 1
	}
    },

    // CHECK THAT FAMILIARIZARION IS DONE
    check_fam: function() {
	if (fam_finished == 1) 
	{
	    famNextButton.blur(); 
	    experiment.next_test();

	} else {
	    $("#famMessage").html('<font color="red">' + 
			       'Please make sure you have looked at all the days!' + 
			       '</font>');
	}
    },

    // CHECK THAT TEST IS DONE
    check_test: function() {
	if (experiment.choice != "null" &&
	    document.getElementById("manipCheckTarget").value != "" &&
	    document.getElementById("manipCheckDist").value != "") 
	{
	    testNextButton.blur(); 
	    experiment.manip_check_target = document.getElementById("manipCheckTarget").value;
	    experiment.manip_check_dist = document.getElementById("manipCheckDist").value;

    	    showSlide("check");

	} else {
	    $("#testMessage").html('<font color="red">' + 
			       'Please make sure you have answered all the questions!' + 
			       '</font>');
	}
    },

   // FINISHED BUTTON CHECKS EVERYTHING AND THEN ENDS
    check_finished: function() {
	if (($("input[type=radio]:checked").length == 0) ||
	    document.getElementById('about').value.length < 1) {
	    $("#checkMessage").html('<font color="red">' + 
			       'Please make sure you have answered all the questions!' + 
			       '</font>');
	} else {
	    if ($("input[type=radio]:checked")[0].value) {
		experiment.name_check_correct = "TRUE";
	    }
	    experiment.about = document.getElementById("about").value;
	    experiment.comment = document.getElementById("comments").value;

    	    showSlide("finished");

	    if (experiment.choice == "target") {
		experiment.choice_correct = "TRUE";
	    } 
	    experiment.end();
	}
    },

    // END FUNCTION 
    end: function () {
        showSlide("finished");
        setTimeout(function () {

            // Decrement only if this is an actual turk worker!		
	    if (turk.workerId.length > 0){
		var xmlHttp = null;
		xmlHttp = new XMLHttpRequest();
		xmlHttp.open('GET',			 
			     'http://langcog.stanford.edu/cgi-bin/' + 
			     'subject_equalizer/decrementer.php?filename=' + 
			     filename + "&to_decrement=" + cond, false);
		xmlHttp.send(null);
	    }

            turk.submit(experiment);
        }, 500); 
    }
}