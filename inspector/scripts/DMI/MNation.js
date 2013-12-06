//namespace scope
(function( DMI, $, undefined ){
		
var MSite = DMI.MSite = DMI.MSite || {};
var MNation = DMI.MNation = DMI.MNation || {};

var Format = DMI.Format;
var Utils = DMI.Utils;

var modctx = DMI.modctx;
var modconstants = DMI.modconstants;


MSite.initSite = function(o) {
	o.units = [];
	o.commanders = [];
	o.capunits = [];
	o.capcommanders = [];	
}
MSite.prepareData_PreMod = function() {
	for (var oi=0, o;  o= modctx.sitedata[oi];  oi++) {
		
		o.units = Utils.keyListToTable(o, 'mon');
		o.commanders = Utils.keyListToTable(o, 'com');
		o.capunits = Utils.keyListToTable(o, 'hmon');
		o.capcommanders = Utils.keyListToTable(o, 'hcom');
	}
}
MSite.prepareData_PostMod = function() {
	for (var oi=0, o;  o= modctx.sitedata[oi];  oi++) {
	}
}



MNation.initNation = function(o) {
	o.pretenders = [];
	o.commanders = [];
	o.units = [];
	o.heroes = [];	
	o.multiheroes = [];
	o.uwunits = [];
	o.uwcoms = [];
	o.specialunits = [];
	o.sites = [];
	o.spells = [];
}
MNation.prepareData_PreMod = function() {
	for (var oi=0, o;  o= modctx.nationdata[oi];  oi++) {
		
		o.pretenders = Utils.keyListToTable(o, 'pret');
		o.commanders = Utils.keyListToTable(o, 'com');
		o.units = Utils.keyListToTable(o, 'unit');
		o.heroes = Utils.keyListToTable(o, 'hero', 6);	
		o.multiheroes = Utils.keyListToTable(o, 'multi', 2);
		o.uwunits = Utils.keyListToTable(o, 'uwu', 5);
		o.uwcoms = Utils.keyListToTable(o, 'uwc', 5);
		o.specialunits = Utils.keyListToTable(o, 'spc');
		o.sites = Utils.keyListToTable(o, 'site', 4);
		
		o.spells = [];
	}
}

MNation.prepareData_PostMod = function() {
	for (var oi=0, o;  o= modctx.nationdata[oi];  oi++) {
		o.id = parseInt(o.id);
		
		o.renderOverlay = MNation.renderOverlay;
		
		o.eracode = modconstants.eracodes[o.era];
		o.shortname = o.eracode+'  '+o.name;
		o.fullname = o.eracode+'  '+o.name+'  -  '+o.epithet;
		
		//associate spells
		//national spells already listed themselves in o.spells
		//now we need to set nation details on the spells
		for (var si=0, s; s= o.spells[si]; si++) {			
			s.nations = s.nations || {};
			s.nations[o.id] = o;
			s.eracodes = s.eracodes || {}; 
			s.eracodes[o.eracode] = true;
			
			//nationname
			var ncount=0; for (var k in s.nations) ncount++;
			if (ncount == 1)
				s.nationname = o.shortname;
			else
				s.nationname = 'various ('+ncount+')';
			
			//set nation value on summoned units
			var arr = s.summonsunits || [];
			for (var i=0, u; u= arr[i]; i++) {
				if (u.type == 'combat summon' || u.type == 'combat summon (cmdr)')
					continue;
				
				u.nations = u.nations || {};
				u.nations[o.id] = o;
				u.eracodes = u.eracodes || {}; 
				u.eracodes[o.eracode] = true;
				
				//nationname
				var ncount=0; for (var k in u.nations) ncount++;
				if (ncount == 1)
					u.nationname = o.shortname;
				else
					u.nationname = 'various ('+ncount+')';
			}
		}
		
		//associate pretenders
 		var basekey = 'pretender';
		var arr = o.pretenders;
		for (var i=0; i<arr.length; i++) {
			if (!arr[i]) continue;
			var u = modctx.unitlookup[arr[i]];
			if (!u) {
				console.log(basekey+' '+arr[i]+' not found (nation '+o.id+')');
				continue;
			}
			if (u.type && u.type!=basekey) {
				//find pretender version of this unit
				u = modctx.getUnitOfType(u, basekey) || modctx.cloneUnit(u);
			}
			u.type = basekey;
			u.nations = u.nations || {};
			u.nations[o.id] = o;
			u.eracodes = u.eracodes || {}; 
			u.eracodes[o.eracode] = true;
			
			//nationname
			var ncount=0; for (var k in u.nations) ncount++;
			if (ncount == 1)
				u.nationname = o.shortname;
			else
				u.nationname = 'various ('+ncount+')';
		}
		
		//units from sites
 		o.capunits = [];
		o.capcommanders = [];
		
		var basekey = 'site';
		var arr = o.sites;
		var gemkeys = {F:0,A:0,W:0,E:0,S:0,D:0,N:0,B:0};
		for (var i=0; i<arr.length; i++) {
			if (!arr[i]) continue;
			var s = modctx.sitelookup[arr[i]];
			if (!s) {
				console.log(basekey+' '+arr[i]+' not found (nation '+o.id+')');
				continue;
			}
			o.capunits = o.capunits.concat(s.units, s.capunits);
			o.capcommanders = o.capcommanders.concat(s.commanders, s.capcommanders);
			for (k in gemkeys) {
				if (s[k])
				gemkeys[k] += parseInt(s[k]);  
			}
		}
		//remove capunits duplicated in units (etc)
		Utils.arrayDisect(o.capunits, o.units)
		Utils.arrayDisect(o.capcommanders, o.commanders)

		Utils.arrayUnique(o.units);
		Utils.arrayUnique(o.commanders);
		Utils.arrayUnique(o.capunits);
		Utils.arrayUnique(o.capcommanders);
		//should do it..?
		
		o.gems = '';
		for (k in gemkeys) {
			if (gemkeys[k])
				o.gems  +=  '+'+ String(gemkeys[k]) + k; 
		}
	}

	for (var oi=0, o;  o= modctx.nationdata[oi];  oi++) {
		//////////////////////////////////////////////////
		// associate units with this nation
		//  if unit is already associated with a nation it creates a duplicate (with incremented id: +.01)
		/////////////////////////////////////////////////
		var iterations = {
			'unit': o.units, 
			'commander': o.commanders,
			'hero (unique)': o.heroes,
			'hero (multi)': o.multiheroes,
			'unit (u-water)': o.uwunits,
			'cmdr (u-water)': o.uwcoms,
			'special': o.specialunits,
			'unit (cap only)': o.capunits,
			'cmdr (cap only)': o.capcommanders
		}
		for (var basekey in iterations) {
			var arr = iterations[basekey];
			for (var i=0; i<arr.length; i++) {
				if (!arr[i]) continue;
				var u = modctx.unitlookup[arr[i]];
				//~ if (!u) {
					//~ console.log(basekey+' '+arr[i]+' not found (nation '+o.id+')');
					//~ continue;
				//~ }
				//~ if ((u.nation && u.nation!=o) || (u.type && u.type!=basekey))
					//~ u = modctx.cloneUnit(u);
				//~ 
				//~ u.type = basekey;
				//~ u.nation = o;
				//~ u.nationname = o.shortname;
				//~ delete u.nations;

			///////////////////////////////
				if (!u) {
					console.log(basekey+' '+arr[i]+' not found (nation '+o.id+')');
					continue;
				}
				if (u.type && u.type!=basekey) {
					//find right version of this unit
					var newu = modctx.getUnitOfType(u, basekey);
					if (newu) u = newu;
					else {
						u = modctx.cloneUnit(u);
						u.nations = {};
						u.eracodes = {};
					}
				}
				u.type = basekey;
				u.nations = u.nations || {};
				if (u.nations[o.id]) continue;
				
				u.nations[o.id] = o;
				u.eracodes = u.eracodes || {}; 
				u.eracodes[o.eracode] = true;
				
				//nationname
				var ncount=0; for (var k in u.nations) ncount++;
				if (ncount == 1)
					u.nationname = o.shortname;
				else
					u.nationname = 'various ('+ncount+')';
			}
		}
	}
	//fill nation selection box
	var h='';
	for (var era=1; era<=3; era++) {
		h+='<option disabled=disabled>-- '+modconstants.eranames[era]+' --</option>\n';		
		for (var oi=0; oi<modctx.nationdata.length; oi++) {
			var o = modctx.nationdata[oi];
			if (o.era == era) {				
				h+='<option value="'+o.id+'" title="nation '+o.id+'">'+o.fullname+'</option>\n';
			}
		}
	}
	$('select.nation').append($(h));
	
	DMI.MUnit.prepareData_PostNationData();
}




MNation.renderOverlay = function(o) {
	//template
	var h=''
	h+='<div class="nation overlay-contents"> ';
	
	//header
	h+='	<div class="overlay-header" title="nation id: '+o.id+'"> ';
	h+=' 		<input class="overlay-pin" type="image" src="images/PinPageTrns.png" title="unpin" />';
	h+='		<h2>'+o.fullname+'</h2> ';
	h+='	</div>';
	
	//mid
	h+='	<div class="overlay-main">';
	h+=		Format.Gems(o.gems);
	
	//wikilink
	h+='		<div class="overlay-wiki-link non-content" style="clear:both;">';
	h+='			<a href="http://dom3.servegame.com/wiki/'+o.name.replace(/ /g, '_')+'">[wiki]</a>';
	h+='		</div>';

	if (o.modded) {
		h+='	<div class="modded hidden-block">Modded<span class="internal-inline"> [modded]</span>:<br />';
		h+=		o.modded.replace('ERROR:', '<span style="color:red;font-weight:bold;">ERROR:</span>');
		h+='	</div';
	}	
	h+='	</div>';
	
	h+='</div> ';
	
	return h;	
}

//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
