//namespace scope
(function( DMI, $, undefined ){
		
var modctx = DMI.modctx;

//configuration
var mod_dir = 'mods/'


DMI.loaded_mod_files = [];

//local
var mods_to_load;
var custom_mods = [];
var custom_mod_data = {};


//load data
DMI.selectMods = function(cb_success) {
	mods_to_load = (new ParsedQueryString()).params("mod");
	
	if (location.search.indexOf('selectmods=1')==-1) {
		DMI.loadDom3Data( cb_success );
		return;
	}	
	$('#page-status').html('Select mods...');
	$('#mod-selection').show();
	
	
	var mod_re = new RegExp('<a\\s*href="([\\w\\d\\._-]+?\\.dm)">\\s*\\1\\s*</a>', 'igm')
	var modlist_url = 'mods';
	
	//load mod list from server on startup
	$($.ajax({
		url: modlist_url,
		dataType:  'text',
		success: function(data) {
			var modlist = [];
			while (match = mod_re.exec(data)) {
				modlist.push( match[1] );	
			}
			create_mod_buttons(modlist);
		},
		error: function(wtf, textStatus, errorThrown) {
			DMI.Utils.error(  "Error loading file: " + modlist_url);	
		}
	}));
	
	//create mod buttons on load
	function create_mod_buttons(modlist) {
		var html = '';
		for (var mod, i=0; mod=modlist[i]; i++) {
			//preticked if this mod is in url
			var checked = '';
			if (DMI.Utils.inArray(mod, mods_to_load))
				checked = ' checked="checked"';
			
			html += '<div class="selectable-mod" style="float:left; padding:0em 1em">'
			html += '<input name="mod" id="select-'+mod+'" value="'+mod+'" type="checkbox" '+checked+'>';
			html += '<label for="select-'+mod+'">'+mod+'</label>';
			html += '</div>'
		}
		$('#mod-selection-list').html(html)
		$('#mod-selection-list input').bind('change click', highlightSelectedMods);
		highlightSelectedMods();
	}
	//visual highlights & clear mod selection button [x]
	function highlightSelectedMods() {
		$('#clear-mods-btn').hide();
		$('#mod-selection-list input').each(function(){
			if ($(this).prop('checked')) {
				$(this).parent().css({ fontWeight:'bold', color:'#AA0000' })
				$('#clear-mods-btn').show();
			}
			else
				$(this).parent().css({ fontWeight:'normal', color:'black' });
		});
	}
	$('#clear-mods-btn').click(function(){
		$('#mod-selection-list input:checked').each(function(){
			$(this).prop('checked', false);
			highlightSelectedMods();
		});
	});
	
	//load local mod files
	function handleFileSelect(evt) {
	    var files = evt.target.files; // FileList object
	    $('ul#custom-mod-list').hide().html('');

	    // Loop through the FileList
	    for (var i = 0, f; f = files[i]; i++) {
		    var reader = new FileReader();
		    
		    $('ul#custom-mod-list').show().append('<li>'+f.name+'</li>');
		    $('#clear-custom-mods-btn').show();
		    
		    //callback in closure with file details
		    reader.onload = (function(f) {
			return function(e) {
				custom_mods.push(f.name);
				custom_mod_data[f.name] = e.target.result;
			};
		    })(f);
	      // Read in the file as a data URL.
	      reader.readAsText(f);
	    }
	}
	$('#load-custom-mod').bind('change', handleFileSelect);
	$('#clear-custom-mods-btn').click(function(){
		$('#load-custom-mod').val(null);
		$('ul#custom-mod-list').hide().html('');
		$(this).hide();
	}).hide();

	//on submit.. make list of modnames and load data
	$('#mod-selection').submit(function(evt){
		mods_to_load = [];
		$('#mod-selection-list input:checked').each(function(){
			mods_to_load.push( $(this).val() );
		});
		DMI.loadDom3Data(cb_success);
		
		evt.preventDefault();
		return false;
	});
}	
	
		
//load data
DMI.loadDom3Data = function(cb_success) {
	$('#page-status').html('Loading data...');
	$('#mod-selection').hide();

	var filestoload = [
		'gamedata/BaseI.csv'+versionCode,
		'gamedata/BaseA.csv'+versionCode,
		'gamedata/BaseU.csv'+versionCode,
		'gamedata/BaseW.csv'+versionCode,
		'gamedata/Spells.csv'+versionCode,
		'gamedata/MagicSites.csv'+versionCode,
		'gamedata/Nations.csv'+versionCode
	];
	
	//add directory to mod paths
	for (var i=0; i<mods_to_load.length; i++) 
		mods_to_load[i] = 'mods/'+mods_to_load[i];
	
	
	if (DMI.Options['Ignore mods']) {
		mods_to_load = [];
		custom_mods = [];
	}	
	var onsuccess = function(dataobj) {
			//all loaded.. parse tables			
			var data = dataobj['gamedata/BaseI.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/BaseI.csv'));
			modctx.itemdata = parseTextToTable(data);
			modctx.itemlookup = createLookup(modctx.itemdata, 'id', 'name');
			
			var data = dataobj['gamedata/BaseA.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/BaseA.csv'));
			modctx.armordata = parseTextToTable(data);
			modctx.armorlookup = createLookup(modctx.armordata, 'id', 'name');
	
			var data = dataobj['gamedata/BaseU.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/BaseU.csv'));
			modctx.unitdata = parseTextToTable(data);
			modctx.unitlookup = createLookup(modctx.unitdata, 'id', 'name');
	
			var data = dataobj['gamedata/BaseW.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/BaseW.csv'));
			modctx.wpndata = parseTextToTable(data);
			modctx.wpnlookup = createLookup(modctx.wpndata, 'id', 'name');

			var data = dataobj['gamedata/Spells.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/Spells.csv'));
			modctx.spelldata = parseTextToTable(data);
			modctx.spelllookup = createLookup(modctx.spelldata, 'id', 'name');
			
			var data = dataobj['gamedata/MagicSites.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/MagicSites.csv'));
			modctx.sitedata = parseTextToTable(data);
			modctx.sitelookup = createLookup(modctx.sitedata, 'id', 'name');
			
			var data = dataobj['gamedata/Nations.csv'+versionCode];
			if (!data) throw(DMI.Utils.error('ERROR LOADING: gamedata/Nations.csv'));
			modctx.nationdata = parseTextToTable(data);
			modctx.nationlookup = createLookup(modctx.nationdata, 'id', 'nationname');
			
			
			//before applying mod (order is important!)
			DMI.MWpn.prepareData_PreMod();
			DMI.MArmor.prepareData_PreMod();
			DMI.MItem.prepareData_PreMod();
			DMI.MUnit.prepareData_PreMod();
			DMI.MSpell.prepareData_PreMod();
			DMI.MSite.prepareData_PreMod();
			DMI.MNation.prepareData_PreMod();
	
			//parse the mods
			for (var i=0; i<mods_to_load.length; i++) {
				var data = dataobj[mods_to_load[i]];
				if (data) {
					modctx.parseMod( data, i+1, mods_to_load[i] );
					DMI.loaded_mod_files.push(mods_to_load[i].replace(mod_dir,''));
				}
				else
					DMI.Utils.error('ERROR LOADING: '+mods_to_load[i])
			}
			//parse custom mods
			for (var i=0; i<custom_mods.length; i++) {
				var data =  custom_mod_data[custom_mods[i]];
				if (data) {
					modctx.parseMod( data, i+1, custom_mods[i] );
				}
				else
					DMI.Utils.error('ERROR READING: '+custom_mods[i])
			}
			custom_mod_data = null;
			
			//after applying mod (order is important!)
			DMI.MWpn.prepareData_PostMod();
			DMI.MArmor.prepareData_PostMod();
			DMI.MItem.prepareData_PostMod();
			DMI.MUnit.prepareData_PostMod();
			DMI.MSpell.prepareData_PostMod();
			DMI.MSite.prepareData_PostMod();
			DMI.MNation.prepareData_PostMod();
			
			//run callback
			setTimeout(cb_success,1);
	}
	var onerror = function( emsg, details ) {
		console.log( emsg + "\n" + details );
		throw(DMI.Utils.error(emsg));
	}
	preloadData( filestoload.concat(mods_to_load), onsuccess, onerror );
}


function preloadData(urllist, onfinish, onerror) {
	if (urllist.length == 0) {
		onfinish({});
		return;
	}
	//upvalues
	var loadedData = {};
	var numFinished = 0;
	var onfinish = onfinish;
	var onerror = onerror;
	
	for (var i=0; i<urllist.length; i++) {
		//inner closure for each url
		var fn = function(){
			var url = urllist[i];
			$.ajax({
				url: url,
				dataType:  'text',
				success: function(data) {
					loadedData[url] = data;
					numFinished++;
					if (numFinished == urllist.length)
						onfinish(loadedData);
				},
				error: function(wtf, textStatus, errorThrown) {
					if (onerror) {
						onerror(  "Error loading file: " + url + ((wtf && wtf.responseText) || ' '), 
							textStatus 
							+ "\n" + JSON.stringify(wtf)
							+ "\n" + JSON.stringify(errorThrown)
						);
						onfinish = onerror = function(){};
						return;
					}
					loadedData[url] = undefined;
					numFinished++;
					if (numFinished == urllist.length)
						onfinish(loadedData);
				}
			});
		};fn();
	}
}



function parseTextToTable(str) {
	var t = [];
	
	var lines = str.split("\n");
	var keynames = lines[0].split("\t");

	for (var i=1; i<lines.length; i++) {
		var values = lines[i].split("\t");
		
		if (values[0]=="")
			continue;
		
		var o = new Object();
		for (var j=0; j<keynames.length; j++) {
			var key = keynames[j];
			var val = values[j]
			if (val != "" && val != "\r") {
				//if (key == 'id#') key = 'id';
				o[key] = values[j];
			}
		}
		t.push(o);	 
	}	
	return t;
}

function createLookup(t, k1, k2) {
	var lookup = {};
	for (var i=0; i<t.length; i++) {
		var line = t[i];
		
		var v1;
		if ((v1= line[k1]) && !lookup[v1])
			lookup[v1] = line;
		
		var v2;
		if (k2 && (v2= line[k2])) {
			v2 = v2.toLowerCase();
			if (!lookup[v2])
				lookup[v2] = line;
		}
	}
	return lookup;
}




//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
