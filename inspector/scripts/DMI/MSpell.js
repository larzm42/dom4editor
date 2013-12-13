//namespace scope
(function( DMI, $, undefined ){
		
var MSpell = DMI.MSpell = DMI.MSpell || {};

var Format = DMI.Format;
var Utils = DMI.Utils;

var modctx = DMI.modctx;
var modconstants = DMI.modconstants;


//////////////////////////////////////////////////////////////////////////
// PREPARE DATA
//////////////////////////////////////////////////////////////////////////

MSpell.initSpell = function(o) {
	o.nations = [];
}

MSpell.nationList = function (o) {
	o.nations = [];
	if (o.national && o.national != '') {
		var parts = o.national.split(',');
		var era;
		var oldNation;
		for (var oi=0, nation;  nation = parts[oi];  oi++) {
			era = 0;
			if (nation.indexOf('EA') != -1) {
				era = 1;
				nation = nation.replace(/EA/g,'');
				nation = nation.replace(/^\s+|\s+$|"/g,'');
				if (nation == '') {
					nation = oldNation;
				}
			} else if (nation.indexOf('MA') != -1) {
				era = 2;
				nation = nation.replace(/MA/g,'');
				nation = nation.replace(/^\s+|\s+$|"/g,'');
				if (nation == '') {
					nation = oldNation;
				}
			} else if (nation.indexOf('LA') != -1) {
				era = 3;
				nation = nation.replace(/LA/g,'');
				nation = nation.replace(/^\s+|\s+$|"/g,'');
				if (nation == '') {
					nation = oldNation;
				}
			}
			nation = nation.replace(/^\s+|\s+$|"/g,'');
			if (nation == 'Pan') {
				nation = 'Pangaea';
			}
			if (nation == 'Arco') {
				nation = 'Arcoscephale';
			}
			oldNation = nation;
			for (var oj=0, natdata;  natdata = modctx.nationdata[oj];  oj++) {
				if (natdata.name == nation) {
					if (era == 0 || natdata.era == era) {
						o.nations.push(natdata.id);
					}
				}
			}
		}
	}
	return o.nations;
}

MSpell.prepareData_PreMod = function() {
	for (var oi=0, o;  o= modctx.spelldata[oi];  oi++) {
		
		o.path1  = modconstants[16][o.path1];
		o.path2  = modconstants[16][o.path2];

		//database uses 255. mods use -1
		if (o.school == '255')
			o.school = '-1';
		
		o.nations = MSpell.nationList(o);

		if (o.mrneg == 'normal') {
			o.mrneg_norm = 1;
		}
		if (o.mrneg == 'easy') {
			o.mrneg_easy = 1;
		}
		if (o.target_uw == 'y') {
			o.can_target_uw = 1;
		}
		if (o.target_land == 'y') {
			o.can_target_land = 1;
		}
		if (o.target_enemy == 'y') {
			o.can_target_enemey = 1;
		}
		if (o.target_friend == 'y') {
			o.can_target_friend = 1;
		}
		if (o.target_uw == 'n') {
			o.cannot_target_uw = 1;
		}
		if (o.target_land == 'n') {
			o.cannot_target_land = 1;
		}
		if (o.target_enemy == 'n') {
			o.cannot_target_enemey = 1;
		}
		if (o.target_friend == 'n') {
			o.cannot_target_friend = 1;
		}
			
	}
}


MSpell.prepareData_PostMod = function() {
	for (var oi=0, o;  o= modctx.spelldata[oi];  oi++) {
		//shift nation data to nations (who will shift it back in another form)
		for (var ni=0, nid, n; nid= o.nations[ni]; ni++) {
			if (!(n= modctx.nationlookup[nid])) {
				console.log('nation '+nid+ ' not found (spell '+o.id+')');
				continue;
			}
			n.spells.push(o);
		}
		delete o.nations;
		
		
		o.renderOverlay = MSpell.renderOverlay;
		o.matchProperty = MSpell.matchProperty;
		
		//convert to numbers (for column ordering)
		//doesn't seem to cause any further problems..
		o.id = parseInt(o.id);
		o.fatiguecost = parseInt(o.fatiguecost);
		
		
		//serachable string
		o.searchable = o.name.toLowerCase();
		
		//flip 'works underwater' bit and suchlike
		o.spec_original = o.spec;
		o.spec = MSpell.updateSpecialBitfield(o.spec);		
				
		//lookup effect 2
		if (o.nextspell == '0')
			delete o.nextspell;
		
		else if (o.nextspell) {
			var e2;
			if (e2 = modctx.spelllookup[o.nextspell])
				o.nextspell = e2;
			else {
				console.log('spell '+o.nextspell+' not found (nextspell on spell '+o.id+')');
				delete o.nextspell;
			}
		}

		//path: E1D1
		if (!o.path1 || o.pathlevel1=='0') {
			delete o.path1;
			delete o.pathlevel1;
		}
		if (!o.path2 || o.pathlevel2=='0') {
			delete o.path2;
			delete o.pathlevel2;
		}
		o.mpath = (o.path1 || "") + (o.pathlevel1 || "") + (o.path2 || "") + (o.pathlevel2 || "");

		//research: Alteration 10
		o.research = modconstants[15][o.school];
		o.sortschool = o.school
		if (o.school != -1 && o.school != 7) {
			o.research += ' ' + o.researchlevel;
			o.sortschool += '.' + o.researchlevel;
		}
		
		//gemcost from fatigue
		o.gemcost = 0;
		if (o.path1 && o.fatiguecost >= 100)
		 	o.gemcost = String(Math.floor(o.fatiguecost/100)) + o.path1;
		
		//combat fatiguecost
		if (o.type == 'Ritual'){
			delete o.fatiguecost;
		}
	

		//log cloud effects		
		  // var e = parseInt(o.effect);
		  // if (e > 999 && e<10000)
		  // console.log(o.id+', '+o.name+', damage: '+e);
		
		//associate summons with this spell (and vice  versa)
		var _o = o;
		while (o.school != -1 && _o) {
			//get summons data for this spell
			var arr = MSpell.summonsForSpell(_o);
			for (var i=0, uid, utype;  (uid= arr[i]) && (utype= arr[i+1]);  i+=2) {
				
				if (_o.damage == '-1' || _o.damage == '-2') //national undead
					break;
						
				var u = modctx.unitlookup[uid];
				if (!u) {
					console.log('Unit '+uid+' not found (Spell '+_o.id+')');
					break;
				}
				//find correct version of this unit or create a clone
				if (u.type && u.type != utype) {
					u = modctx.getUnitOfType(u, utype) || modctx.cloneUnit(u);
					u.summonedby = [];//clear
				}
				//add to list of summoned units (to be attached to nations later)
				o.summonsunits = o.summonsunits || [];
				o.summonsunits.push(u);
				
				//set unit type
				u.type = utype;
				
				//attach spell to unit
				u.summonedby = u.summonedby || [];
				u.summonedby.push( o );					
			}
			if (_o == _o.nextspell) break;
			_o = _o.nextspell;
		}
		
	}
}

	
//////////////////////////////////////////////////////////////////////////
// DEFINE GRID
//////////////////////////////////////////////////////////////////////////

function spellNameFormatter(row, cell, value, columnDef, dataContext) {
	if (dataContext.nat_x)
		return '<div class="national-spell">'+value+'</div>';	
	return value;
}

function fatigueFormatter(row, cell, value, columnDef, dataContext) {
	if (value) {
		if (value < 1000 && dataContext.type!='Ritual') {
	       		return String(value)+'-';
		}
	}
	return '';
}
function spellCostFormatter(row, cell, value, columnDef, dataContext) {
	return Format.Gems(dataContext.gemcost)
}
function spellTypeFormatter(row, cell, value, columnDef, dataContext) {
	return (value == 'combat spell') ? 'combat' : value
}

MSpell.CGrid = DMI.Utils.Class( DMI.CGrid, function() {		
	//grid columns
	var columns = [
		{ id: "name",     width: 140, name: "Spell Name", field: "name", sortable: true, formatter: spellNameFormatter },
		{ id: "type",      width: 40, name: "Type", field: "type", sortable: true, formatter: spellTypeFormatter },
		{ id: "research",      width: 60, name: "School", field: "sortschool", sortable: true, formatter: function(_,__,v,___,o){ return o.research; } },
		{ id: "mpath",    width: 40, name: "Path req", field: "mpath", sortable: true, formatter: DMI.GridFormat.Paths },
		{ id: "gemcost",    width: 30, name: "Cost", field: "fatiguecost", cssClass: "numeric", sortable: true, formatter: spellCostFormatter },
		{ id: "fatiguecost",     width: 30, name: "Fat", field: "fatiguecost", cssClass: "numeric", sortable: true, formatter: fatigueFormatter }
	];
	
	this.superClass.call(this, 'spell', modctx.spelldata, columns); //superconstructor

	//closure scope
	var that = this;
	
	
	//+ and - keys increment effect no
	$(that.domselp+" input.effect").keypress( function(e) {
			if (e.which == 43 || e.which == 61) {
				$(this).val( parseInt($(this).val()) +1); 
				e.preventDefault();	
			}
			if (e.which == 45) {
				$(this).val( parseInt($(this).val()) -1); 
				e.preventDefault();	
			}			
	});
	//+ and - keys double/half bitmask search values
	$(that.domselp+" input.effect-mask, "+ that.domselp+" input.special-mask").keypress( function(e) {
			if (e.which == 43 || e.which == 61) {
				$(this).val( parseInt($(this).val()) * 2); 
				e.preventDefault();	
			}
			if (e.which == 45) {
				$(this).val( parseInt($(this).val()) / 2); 
				e.preventDefault();	
			}			
	});

	//selecting national/generic deselects the other
	$(that.domselp+" input.national").bind('change click', function(e) {
		if ($(this).prop('checked')) 
			$(that.domselp+" input.generic").prop('checked', false).saveState();
	});
	$(that.domselp+" input.generic").bind('change click', function(e) {
		if ($(this).prop('checked')) 
			$(that.domselp+" input.national").prop('checked', false).saveState();
	});
	
	
	//reads search boxes
	this.getSearchArgs = function(domsel) {
		var args = Utils.merge(this.getPropertyMatchArgs(), {
			str: $(that.domselp+" input.search-box").val().toLowerCase(),
			nation: $(that.domselp+" select.nation").val(),
			
			type: $(that.domselp+" select.type").val(),
			schools: Utils.splitToLookup( $(that.domselp+" select.school").val(), ','),
			generic: $(that.domselp+" input.generic:checked").val(),
			national: $(that.domselp+" input.national:checked").val(),
			
			effect: parseInt($(that.domselp+" input.spell-effect-number").val()),
			effect_mask: parseInt($(that.domselp+" input.effect-mask").val()),
			special_mask: parseInt($(that.domselp+" input.special-mask").val()),
			
			aquatic: $(that.domselp+" select.aquatic").val(),

			mpaths: ''
		});
		if ($.isEmptyObject(args.schools)) delete args.schools;
		
		//whole era
		if (args.nation == 'EA' || args.nation == 'MA' || args.nation == 'LA') {
			args.eracode = args.nation;
			delete args.nation;
		}
		else args.nation = modctx.nationlookup[ args.nation ];
				
		//create string of mpaths from checkboxes
		$(that.domselp+' .toggle-path:checked').each(function() {
			args.mpaths += this.value;
		});
		return args;
	}
	
	//apply search
	this.searchFilter =  function(o, args) {
		//type in id to ignore filters
		if (args.str && args.str == String(o.id)) return true;
		
		//check effect no (and recurring attached effects)
		skip:
		if (args.effect) {
			var oo = o;
			//check recurring nextspell
			while(oo) {
				if (oo.effect == String(args.effect) || oo.effect == String(10000 + args.effect)) {
					//check masks on damage
					if (!args.effect_mask || parseInt(oo.damage) & args.effect_mask)
						//return true;
						break skip;
				}					
				oo = oo.nextspell;
				if (oo == o) break; //avoid infinite loop
			}
			return false;
		}
		
		//check special mask (including child effeccts)
		skip:
		if (args.special_mask) {
			var oo = o;
			//check recurring nextspell
			while(oo) {
				if (parseInt(oo.spec_original) & args.special_mask)
					//return true;
					break skip;
				oo = oo.nextspell;
				if (oo == o) break; //avoid infinite loop
			}
			return false;
		}
		
		//type		
		if (args.type && o.type != args.type)
			return false;
		
		//school		
		if (args.schools && !args.schools[o.school])
			return false;
		
		
		//national (national spells only)
		if (args.national && !o.nations)
			return false;
		//generic (generic spells only)
		if (args.generic && o.nations)
			return false;
		
		//era
		if (args.eracode && o.eracodes) {
			if (!o.eracodes[args.eracode])
				return false;			
		}
		//nation
		if (args.nation && o.nations) {
			if (!o.nations[args.nation.id])
				return false;
		}
		
		//aquatic
		if (args.aquatic) {
			if (args.aquatic == 'uw' && !DMI.MSpell.worksUnderwater(o))
				return false;
			if (args.aquatic == 'land' && !DMI.MSpell.worksOnDryLand(o))
				return false;
		}		
		
		//search string
		if (args.str && o.searchable.indexOf(args.str) == -1)
			return false;
		
		//magic paths
		if (args.mpaths) {
			if (args.mpaths.indexOf(o.path1) == -1)
				return false;
			if (o.path2 && args.mpaths.indexOf(o.path2) == -1)
				return false;
		}
		
		//key =~ val
		if (args.key) {
			var r = o.matchProperty(o, args.key, args.comp, args.val);
			if (args.not  ?  r  :  !r)
				return false;
		}
		return true;
	}
	
	//customise initial search
	this.initialSortTrigger = this.domsel+" div.slick-header-column[title=School]";
	
	this.defaultSortCmp = function(r1,r2) {
		if (r2.mpath < r1.mpath) return 1;
		if (r2.mpath > r1.mpath) return -1;
		return 0;
	}
	
	this.init();
});
//MSpell.matchProperty = DMI.matchProperty;
MSpell.matchProperty = function(o, key, comp, val) {
	if (DMI.matchProperty(o, key, comp, val))
		return true;

	//nextspell..
	if (o.nextspell)
		return DMI.MSpell.matchProperty(o.nextspell, key, comp, val);
}

MSpell.formatDmgType = function(v,o) {
	if (o.dt == 'n') {
		return 'normal';
	}
	if (o.dt == 'm') {
		return 'magic';
	}
	if (o.dt == 'f') {
		return 'fire';
	}
	if (o.dt == 'c') {
		return 'cold';
	}
	if (o.dt == 's') {
		return 'shock';
	}
	if (o.dt == 'p') {
		return 'poison';
	}
	if (o.dt == 'a') {
		return 'acid';
	}
}

MSpell.formatAoe = function(v,o) {
	if (v == 'bf') {
		return 'battlefield';
	}       
	return o.aoeplus ? v+'+' : v; 
}


//////////////////////////////////////////////////////////////////////////
// OVERLAY RENDERING
//////////////////////////////////////////////////////////////////////////

var aliases = {};
var formats = {};
var hiddenkeys = Utils.cutDisplayOrder(aliases, formats,
[
	'id', 		'spell id'
]);
var moddingkeys = Utils.cutDisplayOrder(aliases, formats,
[
	'effect',	'effect',	function(v,o){ return v + ' (damage:'+o.damage+')'; },
	'nextspell',	'nextspell',	function(v,o){ return v.id; },
	'spec_original',	'special',
	'extra_eff',	'effect',
	'explanation',	'explanation',
	'other',	'other'
]);
var displayorder = Utils.cutDisplayOrder(aliases, formats,
[
	'fatiguecost',	'fatigue cost',		function(v){ return v+'-'; },
	'gemcost',	'gems required',	Format.Gems,

	'rng_bat',	'range', 		function(v,o){ return o.rngplus ? v+'+' : v; },
	'rng_prov',	'range', 		function(v,o){ return o.rng_prov == 1 ? v+' province' : v+' provinces' },
	'aoe_s',	'area of effect', 	MSpell.formatAoe,
	'nreff', 	'number of effects',	function(v,o){ return o.effplus ? v+'+' : v; },

	'precision',	'precision',	{0: '0 '},
	'dmg',		'damage', 	function(v,o){ return o.dmgplus ? v+'+' : v; },
	'dt',		'damage type', 	MSpell.formatDmgType,
	'heal',		'heal',
	'comsummon',	'summons commander', 	Utils.unitRef,
	'summon',	'summons', 	Utils.unitRef
	
]);
var flagorder = Utils.cutDisplayOrder(aliases, formats,
[
//	dbase key	displayed key		function/dict to format value
	'can_target_uw',	'can target underwater',
	'can_target_land',	'can target land',
	'can_target_enemey',	'can target enemy',
	'can_target_friend',	'can target friendly',
	'cannot_target_uw',	'cannot target underwater',
	'cannot_target_land',	'cannot target land',
	'cannot_target_enemey',	'cannot target enemy',
	'cannot_target_friend',	'cannot target friend',
	'anonym',	'anonymous',
	'stun',		'stun damage',
	'ap',		'armor piercing',
	'an',		'armor negating',
	'mrneg_norm',	'magic resistence negates',
	'mrneg_easy',	'magic resistence negates easily'

]);
var ignorekeys = {
	modded:1,
	path1:1, pathlevel1:1, path2:1, pathlevel2:1, 
	school:1,
	researchlevel:1,research:1,sortschool:1,

	damage:1,
	type:1,		
	mpath:1,
	fatiguecost:1,gemcost:1,
	rngplus:1,
	aoeplus:1,
	dmgplus:1,
	effplus:1,
	subtype:1,
	uw:1,
	land:1,
	mrneg:1,
	aoe_p:1,
	national:1,
	nat_x:1,
	aoe_p:1,
	target_uw:1,
	target_land:1,
	target_enemy:1,
	target_friend:1,

	// aoe:1, nreff:1,
	summonsunits:1,	nations:1, eracodes:1, nationname:1,
	spec:1,
	
	//common fields
	name:1,descr:1,
	searchable:1, renderOverlay:1, matchProperty:1
};		

function spellCostLine(o) {
	if (o.type=="ritual")
		return 'ritual costs '+Format.Gems(o.gemcost);
	
	else {
		if (o.gemcost)
			return 'combat fatigue '+o.fatiguecost+'- ('+Format.Gems(o.gemcost)+')';
		else
			return 'combat fatigue '+o.fatiguecost+'-';
	}
}

MSpell.renderOverlay = function(o) {
	var descrpath = 'gamedata/spelldescr/';
	
	//template
	var h=''
	h+='<div class="spell overlay-contents"> ';
	
	//header
	h+='	<div class="overlay-header" title="spell id: '+o.id+'"> ';
	h+=' 		<input class="overlay-pin" type="image" src="images/PinPageTrns.png" title="unpin" />';
	h+='		<p style="float:right;">'+o.research+'</p>';
	h+='		<h2>'+o.name+'</h2> ';
	
	//nation info
	if (o.nations) {
		var nname = o.nationname,  ntitle='', num=0;
		for (var k in o.nations) {
			num++;
			ntitle += ntitle ? ', &nbsp;\n' : '';
			ntitle += Utils.nationRef( o.nations[k].id, o.nations[k].shortname );			
		}
		h+='	<p>'+ntitle+'</p> ';
		
		//vanilla spells have 3 nations max.3 shortnames will fit nicely
		// if (num <= 3) 
		// 	h+='	<p>'+ntitle+'</p> ';
		// else
		// 	h+='	<p title="'+ntitle+'">'+nname+'</p> ';
	}
	
	
	//body
	h+='	</div>';
	h+='	<div class="overlay-main" style="position:relative;">';
	
	//type & path requirements
	h+='		<div style="float:right; clear:right;">'+o.type+'</div>';
	h+='		<div style="float:right; clear:right;">'+Format.Paths(o.mpath)+'</div>';
	
	//spell details & secondary effects
	h+=		MSpell.renderSpellTable(o);
	
	//special flags; casting requirements (cannot be cast underwater etc..)
	var specflags = Utils.renderFlags( MSpell.landValues(o) );
	if (specflags)
		h+=	'<p>'+specflags+'</p>';	

	
	//wikilink
	h+='		<div class="overlay-wiki-link non-content">';
	// h+='			<a class="select-text-button hidden-inline" href="javascript:void(0);">[text]</a>';
	h+='			<a href="http://dom3.servegame.com/wiki/'+o.name.replace(/ /g, '_')+'">[wiki]</a>';
	h+='		</div>';
	
	//footer
	h+='	</div>';
	h+='	<div class="overlay-footer">';
	
	//descr
	var uid = 'c'+(Math.random());
	uid = uid.replace('.','');
	h+='		<div class="overlay-descr pane-extension '+uid+'">&nbsp;</div>';
	
	if (o.descr)
			Utils.insertContent( '<p>'+o.descr+'</p>', 'div.'+uid );
	else if (o.research != 'unresearchable') {
			 var url = descrpath + Utils.descrFilename(o.name);
			 Utils.loadContent( url, 'div.'+uid );
	}

	h+='	</div> ';
	h+='</div> ';
	return h;	
}

//spell table. +recursive secondary effects
MSpell.renderSpellTable = function(o, original_effect) {
	
	//so irrelevant rows can be hidden by css (eg: aoe for rituals)
	// if (o.type == 'ritual') cssclasses += ' ritual';
	// if (original_effect) cssclasses +=  ' nextspell';
	
	var cssclasses = original_effect ? ' hidden-block' : '';
	
	//template
	var h='';
	h+='		<table class="overlay-table spell-table' + cssclasses + '"> ';
	h+= 			Utils.renderDetailsRows(o, hiddenkeys, aliases, formats, 'hidden-row');
	h+= 			Utils.renderDetailsRows(o, moddingkeys, aliases, formats, 'modding-row');
	h+= 			Utils.renderDetailsRows(o, displayorder, aliases, formats);
	h+= 			Utils.renderDetailsFlags(o, flagorder, aliases, formats);
	h+= 			Utils.renderStrangeDetailsRows(o, ignorekeys, aliases, 'strange');
	h+='		</table> ';

	//mysterious?
	// if (o.spec & 536870912 || (original_effect && (original_effect.spec & 536870912)))
	// 	cssclasses += ' hidden-block';

	
	cssclasses = original_effect ? ' nextspell' : '';
	
	//hide the whole effect if its restore fatigue +0 (it does nothing)
	// (used as gfx effects placeholder in cbm)
	if (o.damage == '0' && MSpell.format.effect(o.effect) == 8)
		cssclasses += 	' hidden-block'
	
	//effect
	h+='		<table class="overlay-table spell-effect '+cssclasses+'"> ';
	//h+=			renderEffect(o, original_effect);
	//special
	var specflags = Utils.renderFlags( MSpell.bitfieldValues(o.spec, MSpell.masks_special) );
	if (specflags)
		h+=		'<tr><td class="widecell" colspan="2">&bull; '+specflags+'</td></tr></div>';
	
	if (o.modded) {
		h+='	<tr class="modded hidden-row"><td colspan="2">Modded<span class="internal-inline"> [modded]</span>:<br />';
		h+=		o.modded.replace('ERROR:', '<span style="color:red;font-weight:bold;">ERROR:</span>');
		h+='	</td></tr>';
	}
	h+=' 		</table> ';

	//h+= Utils.renderModded(o);
	
	//attached effect
	if (o.nextspell) {
		h+=' <h4 class="hidden-block nextspell">'+o.nextspell.name+'</h4>';
		//detect recursion
		if (o.nextspell == o)
			throw 'Error, spell 2nd effect as itself: '+o.id+': '+o.name; 
		else {
			h+= MSpell.renderSpellTable(o.nextspell, original_effect || o);
		}
	} 	
	return h;	
}

MSpell.bitfieldValues = function(bitfield, masks_dict) {
	var values=[],  bitfield = parseInt(bitfield);
	
	for (var k in masks_dict) {
		if (parseInt(k) & bitfield)
			values.push(masks_dict[k]);
	}
	return values;
}

MSpell.landValues = function(o) {
	var values=[];

	if (o.uw == 'n') {
		values.push('cannot be cast underwater');
	}
	if (o.land == 'n' && o.uw == 'y') {
		values.push('only works underwater');
	}
	return values;
}


function renderEffect(o, o_parent) {
	//format basic spell values
	var num = MSpell.format.num(o.nreff, o);
	var range = MSpell.format.range(o.range, o);
	var aoe = MSpell.format.aoe(o.aoe, (o_parent || o));
	var cloud = MSpell.format.cloud(o.effect, o);
	
	//shave spare thousands of effect value
	var neffect = MSpell.format.effect(o.effect)

	
	var _hidekeys = {};
		
	//hide some meaningless stuff
 	if (num == '0' || num == '1') 		_hidekeys.num = true;
 	if ((o_parent || o).type == 'ritual')	_hidekeys.aoe = true;


 	//hide aoe if it covers same area as original
	if (o_parent  &&  ((o.aoe == '1' && o_parent.aoe != '0') || (o.aoe == '0' && o_parent.aoe == '0')))
		_hidekeys.aoe = true;
	
	
	//lookup effect in main table
	var res = MSpell.effectlookup[neffect] || MSpell.effectlookup['unknown'];
	//if its a function then run it
	if (typeof(res) == 'function')	res = res(o);
	//wrap in array if not already
	if (!$.isArray(res))	res = [res];

	
	//render results
	var h= '';
	function renderRow( key, th, td ) {
		var cssclass = key; 
		if (_hidekeys[key]) 
			cssclass += ' modding-row';
		
		h+= '<tr class="'+cssclass+'"><th>'+th+':</th><td>'+td+'</td></tr>';
	}
	//loop objects	
	for (var i=0, e;  e= res[i];  i++) {
		//th:td pairs
		var prefix = '';
		for (var th in e) {
			var td = e[th];
			//special value.. rows to hide, comma separated
			if (th == '_hidekeys') 
				for (var j=0, k, arr=td.split(',');  k= arr[j];  j++) 
					_hidekeys[k] = true;
			else {
				if (typeof(td) == 'function') td = td(o.damage, o);
				renderRow( th.replace(' ', '-'), prefix+th, td );
				
				//all rows but first have bullet prefix
				prefix='&bull; ';
			}
		}
	}
	//final rows
	if (aoe)   renderRow( 'aoe',   '&bull; area of effect',  aoe   );
	if (cloud) renderRow( 'cloud', '&bull; repeats for',   cloud );
	if (num)   renderRow( 'num',   '&bull; nbr of effects',  num   );
	
	return h;	
}	
	


//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
