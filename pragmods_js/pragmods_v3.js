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
function stimHTML(base,prop_mat,props,id) {
    var html = "";

    html += '<img src="images/' + base + 
	'.png" width=100px height=100px alt="' + base + '" id="' + id + 'Image"/>';

    var c = 0;
    for (var p = 0; p < prop_mat.length; p++) {
	if (prop_mat[p] == 1) {
	    html += '<img  src="images/' + 
		props[p] + '.png" width=100px height=100px alt="' + props[p] + '" ' +
		'id="' + id + 'Property' + String(c+1) + '"/>';
	    c = c + 1; // keep count of how many properties we've stacked
	}
    }

    return html;
}


// ---------------- 2. STIMULUS SETUP ------------------
// Condition - call the maker getter to get the cond variable 
var filename = "MCF_pragmods_v3"
var condCounts = "1,50;2,50;3,50;4,50;5,50;6,50;7,50;8,50;9,50;10,50;11,50;12,50;13,50;14,50"
var xmlHttp = null;
xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", "http://langcog.stanford.edu/cgi-bin/subject_equalizer/maker_getter.php?conds=" + 
	      condCounts +"&filename=" + filename, false );
xmlHttp.send( null );
var cond = xmlHttp.responseText;


// CROSS CONDITIONS

// familiarization conditions
var fam_dists = [[0, 0, 1, 2, 2, 2, 2, 2, 2, 2],
		 [0, 0, 1, 1, 2, 2, 2, 2, 2, 2],
		 [0, 0, 1, 1, 1, 2, 2, 2, 2, 2],
		 [0, 0, 1, 1, 1, 1, 2, 2, 2, 2],
		 [0, 0, 1, 1, 1, 1, 1, 2, 2, 2],
		 [0, 0, 1, 1, 1, 1, 1, 1, 2, 2],
		 [0, 0, 1, 1, 1, 1, 1, 1, 1, 2]];
var target_frequencies = [.125,.250,.375,.5,.625,.75,.875];
var num_fam = 10; 

// THIS EXPERIMENT VERSION SPECIFIC
if (cond < 8) {
    var word_cond = "listener";
    var fam_cond = cond - 1;
} else if (cond >= 8) {
    var word_cond = "salience";
    var fam_cond = cond-8;
}

var fam_dist = fam_dists[fam_cond];

// bookkeeping variables
var choices = [0, 1, 2];
var choice = -1;
var target = -2;
var fam_clicked = new Array();
var fam_finished;
var positions = ["left","middle","right"];

// ADAPTED FROM PRAGMODS R CODE
var expt = [[1, 0, 0], [1, 1, 0], [0, 1, 1]];
var level = 1;
var target_unpermuted = 1;
var distractor_unpermuted = 2;
var other_unpermuted = 0;
var target_prop_unpermuted = 1;

var stims = ["friend","snowman","sundae"];
var stims_plural = ["friends","snowmen","sundaes"];
var stims_props = [["hat","glasses","mustache"],
                      ["beanie","scarf","gloves"],
                      ["cherry","whipped cream","chocolate"]];
var stims_prop_words = [["a hat","glasses","a mustache"],
                           ["a beanie","a scarf","gloves"],
                           ["a cherry","whipped cream","chocolate sauce"]]
var stims_actions = [["visit","visits","visited"],
		     ["decorate","decorates","decorated"],
		     ["eat","eats","ate"]];
    
var stim_index = random(0,stims.length-1);

// Permute the matrix randomly:
var prop_perm = shuffle(range(0,expt[0].length-1));
var target_perm = shuffle(range(0,expt.length-1));
var expt_perm = new Array()

for (var i=0; i<expt.length; i++) {
    expt_perm[i] = new Array()
    for (var j=0; j<expt[0].length; j++) {
	expt_perm[i][j] = expt[target_perm[i]][prop_perm[j]];
    }
}

var base = stims[stim_index];
var plural = stims_plural[stim_index];
var actions = stims_actions[stim_index];
var props = stims_props[stim_index];
var prop_words = stims_prop_words[stim_index];

var target = target_perm.indexOf(target_unpermuted);
var distractor = target_perm.indexOf(distractor_unpermuted);
var other = target_perm.indexOf(other_unpermuted);
var target_prop = prop_perm.indexOf(target_prop_unpermuted);

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
var base_image_pl = new Image();
base_image_pl.src = "images/" + base + ".png" 

var props_image_pl = new Array() // By creating image object and setting source, images preload
for (i=0;i<props.length;i++) {
    props_image_pl[i] = new Image()
    props_image_pl[i].src = "images/" + props[i] + ".png" 
} 

showSlide("instructions");

// MAIN EXPERIMENT
var experiment = {
     // stimulus variables - bookkeeping, send these to turk
    item: base,
    target_property: props[target_prop],
    target_frequency: target_frequencies[fam_cond],
    target_position: positions[target],
    choice_correct: "FALSE",
    familiarization_cond: fam_cond, 
    word_condition: word_cond,
    
    // response variables
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
		'Every day he ' + actions[1] + ' a ' + base + '.<p>' +
		'<p class="block-text">Of course, Bob is more likely to ' + actions[0] + ' ' + plural + ' that he likes! Click on each day to see which ' + base + ' Bob ' + actions[2] + '.</p>'
    	    $("#familiarizationText").html(familiarization_html) 
	    
	    // DAY BY DAY POPUPS FOR FAMILIARIZATION
	    var fam_objects_html = '<table align="center"><tr>'
	    
	    for (i=0;i<=num_fam-1;i++){
		fam_objects_html += '<td width=100px height=130px align="center" ' +
		    'class="objTable"' + 
		    'id="famTable' + String(i) + 
		    '" onclick="experiment.reveal(' + String(i) + ')">'
		
		fam_objects_html += 'Day ' + String(i+1) + ', Bob ' + actions[2] + ':<div id="day' + String(i) + '"> </div>'
		
		if ((i+1)%5 == 0) {
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
	
    	var label_html = '<p class="block-text"">Bob says: '

	if (word_cond == "listener") {
	    label_html += '<p class="block-text style="font-size:x-large;">' + 
		'"My favorite ' + base + ' has <b>' + 
		prop_words[target_prop] + 
		'."</b></p>'
	} else if (word_cond == "salience") {
	    label_html += '<p class="block-text style="font-size:x-large;">' + 
		'"My favorite ' + base + ' has <b>mumblemumble."</b></p>' + 
		'<p class="block-text style="font-size:small;">' + 
		'(You couldn\'t hear what he said.)</p>'
	}

	label_html += '<p class="block-text">Which ' + base + 
	    ' do you think is Bob\'s favorite? Click on the ' + base + 
	    ' you think he is most likely to be referring to.</p>'
    	$("#labelInst").html(label_html) 
    	
	// CREATE OBJECT TABLE
	// (tr=table row; td= table data)
	var objects_html = '<table align="center"><tr>'

	for (i=0;i<3;i++){
	    objects_html += '<td width=98px height=110px align="center"' + 
		' class="unchosen objTable" ' +
		'id="tdchoice' + String(i) + '" ' +
		'onclick=\"experiment.select(' + String(i) + ')\">'

	    objects_html += stimHTML(base,expt_perm[i],props,'obj')
	    objects_html += '</td>'
	}

	objects_html += '</tr><tr>'
	objects_html += '</tr></table>'
	
	//jquery - $find the object in the DOM with the id of object, 
	//set the html of that element to this
	$("#objects").html(objects_html) 
		
    },

    // SELECT FUNCTION (called in stage slide)
    select: function (c) {
	choice = c;

	// unchoose everything
	for (var i=0; i<choices.length; i++) {
	    $("#tdchoice" + String(i)).removeClass('chosen').addClass('unchosen')
	}
	// choose this one
	$("#tdchoice" + String(c)).removeClass('unchosen').addClass('chosen')

    },

    // REVEAL IMAGES IN FAMILIARIZATION
    reveal: function(n) {

	day_html = stimHTML(base,fam_mat[n],props,'obj')
	
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
	if (experiment.choice != -1) 
	{
	    testNextButton.blur(); 
    	    showSlide("check");

	} else {
	    $("#testMessage").html('<font color="red">' + 
			       'Please make your choice by clicking on one picture!' + 
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

	    if (choice == target) {
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