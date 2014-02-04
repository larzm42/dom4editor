//namespace scope
(function( DMI, $, undefined ){
		
var MItem = DMI.MItem = DMI.MItem || {};

var Format = DMI.Format;
var Utils = DMI.Utils;

var modctx = DMI.modctx;
var modconstants = DMI.modconstants;


//////////////////////////////////////////////////////////////////////////
// PREPARE DATA
// PREPARE DATA
//////////////////////////////////////////////////////////////////////////

MItem.initItem = function(o) {
}

MItem.prepareData_PreMod = function() {
	for (var oi=0, o;  o= modctx.itemdata[oi];  oi++) {
	}
}

MItem.prepareData_PostMod = function() {
	var forgeCost = { 1:5, 2:10, 3:15, 4:20, 5:30, 6:40, 7:55, 8:70, 9:90 };
	var sum = Utils.sum;
	var negative = Utils.negative;
	var mult = Utils.mult;

	for (var oi=0, o;  o= modctx.itemdata[oi];  oi++) {
		
		o.renderOverlay = MItem.renderOverlay;
		o.matchProperty = MItem.matchProperty;
		
		//convert to numbers (for column ordering)
		//doesn't seem to cause any further problems..
		o.id = parseInt(o.id);
		o.constlevel = parseInt(o.constlevel);
		
		if (o.descr)
			o.descr = '<p>' + o.descr.replace('\n','</p><p>') + '</p>';	
		
		
		//serachable string
		o.searchable = o.name.toLowerCase();
		
		//sprite
		if (o.copyspr) {
			o.copyspr = modctx.itemlookup[o.copyspr];
		}
		if (!o.copyspr)
			o.copyspr = o;
			
		//combine linked armor stats
		if (o.armor) {
			var a = modctx.armorlookup[o.armor];
			if (!a) {
				console.log( 'armor '+o.armor+' not found (item '+o.id+')');
				continue;
			}
			//backlink on armor
			a.used_by.push( Utils.itemRef(o.id) + ' (item)' );
			o.armor = a;
			
			o.prot = a.prot
			o.protbody = a.protbody;
			o.protshield = a.protshield;
			o.prothead = a.prothead;
			o.enc = a.enc;
			o.parry = a.parry;
			
			o.def = sum(o.def, a.def);
		}
		
		//clear secondarylevel if secondarypath was removed
		if (o.secondarypath=='') o.secondarylevel = '';
		
		//path: E1D1
		if (o.mainpath) {
			o.mpath = o.mainpath + o.mainlevel + (o.secondarypath || "") + (o.secondarylevel || "");
		}
		
		//gemcost: 5E5D
		o.gemcost = forgeCost[o.mainlevel] + o.mainpath + (forgeCost[o.secondarylevel] || "") + (o.secondarypath || ""); 
		
		//booster +DDD
		o.boosters = "";
		for (var i=0; i<modconstants.pathkeys.length; i++) {
			var p = modconstants.pathkeys[i];
			for (var j=0; j<parseInt(o[p]); j++) 
				o.boosters += p;
		}
		//lookup weapon
		if (o.weapon) {
			w = modctx.wpnlookup[o.weapon];
			if (!w) console.log( 'weapon '+o.weapon+' not found (item '+o.id+')');
			//backlink on wpn
			else w.used_by.push( Utils.itemRef(o.id) + ' (item)' ); 
			o.weapon = w;
		}			
		//set weapon class (ranged or melee)
		if (o.type == '1-h wpn' || o.type == '2-h wpn') {
			var w = o.weapon;
			if (w && w.ammo && w.ammo != '0')
				o.wpnclass = 'missile';
			else 
				o.wpnclass = 'melee';
		}
			
		if (o.boosters)
			o.boosters = '+'+o.boosters;
	}
}



//////////////////////////////////////////////////////////////////////////
// DEFINE GRID
//////////////////////////////////////////////////////////////////////////

function itemConFormatter(row, cell, value, columnDef, dataContext) {
	if (value==12) return 'Unforgeable';
	return "Constr " + value;
}


MItem.CGrid = Utils.Class( DMI.CGrid, function() {
	//grid columns
	var columns = [
		{ id: "name",     width: 145, name: "Item Name", field: "name", sortable: true },
		{ id: "type",     width: 60, name: "Type", field: "type", sortable: true },
		{ id: "constlevel",      width: 70, name: "Research", field: "constlevel", sortable: true, formatter: itemConFormatter },
		{ id: "mpath",    width: 70, name: "Path req", field: "mpath", sortable: true, formatter: DMI.GridFormat.Paths },
		{ id: "boosters", width: 165, name: "Boosters", field: "boosters", sortable: true, formatter: DMI.GridFormat.Booster }
	];
	
	this.superClass.call(this, 'item', modctx.itemdata, columns); //superconstructor
	
	$(this.domsel+' .grid-container').css('width', 530);//set table width

	
	//in closure scope
	var that = this;
	
	
	//reads search boxes
	this.getSearchArgs = function() {
		var args = Utils.merge(this.getPropertyMatchArgs(), {
			str: $(that.domselp+" input.search-box").val().toLowerCase(),
			type: Utils.splitToLookup( $(that.domselp+" select.type").val(), ','),
			constlevel: parseInt( $(that.domselp+" select.constlevel").val() ),

			mpaths: ''
		});
		if ($.isEmptyObject(args.type)) delete args.type;

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
		
		//check construction level
		if (args.constlevel==12 && o.constlevel!=12)
			return false;
		if (o.constlevel > args.constlevel)
			return false;

		//search string
		if (args.str && o.searchable.indexOf(args.str) == -1)
			return false;
		
		//magic paths
		if (args.mpaths) {
			if (args.mpaths.indexOf(o.mainpath) == -1)
				return false;
			if (o.secondarypath && args.mpaths.indexOf(o.secondarypath) == -1)
				return false;
		}
		
		//item type
		if (args.type && !( args.type[o.type] || args.type[o.wpnclass] ))
				return false;

		//key =~ val
		if (args.key) {
			var r = o.matchProperty(o, args.key, args.comp, args.val);
			if (args.not  ?  r  :  !r)
				return false;
		}
		return true;
	}

	//customise sort	
	this.preSort = function(){
		//bound scope
		var boosterSortPriority = ['F', 'A', 'W', 'E', 'S', 'D', 'N', 'B', 'H'];
		var isSortedOnBoosters = false;
		var data = modctx.itemdata;
			
		//the actual callback
		return function(e, args) {
			if (args.sortCol.field == 'boosters') {
				//rotate booster priority 
				// if (isSortedOnBoosters)
				// 	boosterSortPriority.unshift(boosterSortPriority.pop());
				
				// var L = boosterSortPriority[0];
				
				// //pull priority to front of booster strings
				// var regex = new RegExp('^.([^'+L+']*)('+L+'+)([^'+L+']*)$');
				// for (var i=0; i<data.length; i++) {
				// 	var b = data[i].boosters;
				// 	if (b && b.indexOf(L)!=-1)
				// 		data[i].boosters = b.replace(regex, "_$2$1$3");
				// 	else if (b)
				// 		data[i].boosters = b.replace('_','+');
				// }
				if (isSortedOnBoosters) {
					//rotate priority 
					var pL = boosterSortPriority[0];
					boosterSortPriority.push(boosterSortPriority.shift());
					
					//push last priority to end
					var regex = new RegExp('^.('+pL+'+)(.*)$');
					for (var i=0; i<data.length; i++) {
						var b = data[i].boosters;
						if (b && b.indexOf(pL)!=-1)
							data[i].boosters = b.replace(regex, "+$2$1");
					}
				}
				var L = boosterSortPriority[0];
				
				//set first character to number of instances of L
				for (var i=0; i<data.length; i++) {
					var b = data[i].boosters;
					if (b && b.indexOf(L)!=-1)
						data[i].boosters =  String(b.split(L).length-1) + b.substr(1);
				}
				//switch sort column header icon
				if ( $('#itemboosterordericon')
				     .attr({alt:L, src:'images/magicicons/Path_'+L+'.png', 'class':'pathicon Path_'+L})
				     .css('visibility','visible')
				     .length==0 ) 
				{
					//add icon if not exists yet
					$(".slick-header-column[id*=boosters]")
					.append('<img id="itemboosterordericon" alt="'+L+'" class="pathicon Path_'+L+'" src="images/magicicons/Path_'+L+'.png" />')
					.find(".slick-sort-indicator").css('visibility','hidden');
				}
				//fix sort direction
				args.sortAsc = false;
				isSortedOnBoosters = true;
			} 
			else  {
				//hide sort column header icon if sorting another column
				$('#itemboosterordericon').css('visibility','hidden');
				isSortedOnBoosters = false;
			}
		}
		//exit bound scope
	}();

	//final init
	this.init();
});
MItem.matchProperty = function(o, key, comp, val) {
	if (key=='ap') key = 'armorpiercing';
	if (DMI.matchProperty(o, key, comp, val))
		return true;

	//check attached weapon
	if (o.weapon)
		return DMI.MWpn.matchProperty(o.weapon, key, comp, val);
}


//////////////////////////////////////////////////////////////////////////
// OVERLAY RENDERING
//////////////////////////////////////////////////////////////////////////

var aliases = {};
var formats = {};
var displayorder = DMI.Utils.cutDisplayOrder(aliases, formats,
[
//	dbase key	displayed key		function/dict to format value
	'boosters',	'magic bonus',		Format.Booster,
	'ptr',		'magic penetration',

	'ap',		'bonus action points',
	'ga',		'generates fire gems',		function(v){ return Format.PerTurn(Format.Gems(v+'A')); },
	'gb',		'generates blood slaves',	function(v){ return Format.PerTurn(Format.Gems(v+'B')); },
	'gd',		'generates death gems',		function(v){ return Format.PerTurn(Format.Gems(v+'D')); },
	'ge',		'generates earth gems',		function(v){ return Format.PerTurn(Format.Gems(v+'E')); },
	'gf',		'generates fire gems',		function(v){ return Format.PerTurn(Format.Gems(v+'F')); },
	'gs',		'generates astral gems',	function(v){ return Format.PerTurn(Format.Gems(v+'S')); },
	'gn',		'generates nature gems',	function(v){ return Format.PerTurn(Format.Gems(v+'N')); },
	'gw',		'generates water gems',		function(v){ return Format.PerTurn(Format.Gems(v+'W')); },
	
	'ta',		'temporary fire gems',		function(v){ return Format.PerTurn(Format.Gems(v+'A')); },
	'tb',		'temporary blood slaves',	function(v){ return Format.PerTurn(Format.Gems(v+'B')); },
	'td',		'temporary death gems',		function(v){ return Format.PerTurn(Format.Gems(v+'D')); },
	'te',		'temporary earth gems',		function(v){ return Format.PerTurn(Format.Gems(v+'E')); },
	'tf',		'temporary fire gems',		function(v){ return Format.PerTurn(Format.Gems(v+'F')); },
	'ts',		'temporary astral gems',	function(v){ return Format.PerTurn(Format.Gems(v+'S')); },
	'tn',		'temporary nature gems',	function(v){ return Format.PerTurn(Format.Gems(v+'N')); },
	'tw',		'temporary water gems',		function(v){ return Format.PerTurn(Format.Gems(v+'W')); },

	'str',		'strength',		Format.Signed,
	'inv',		'reinvigoration',
	'att',		'attack',		Format.Signed,
			
	'protbody',	'protection, body',
	'prothead',	'protection, head',
	'protshield',	'protection, shield',
	'def',		'defence',		Format.Signed,
	'parry',	'parry',
	'enc',		'encumbrance',
	'invul',	'invulnerable',
	'darkvis',	'darkvision',
	
	'airshield',	'air shield',		Format.Percent,
	'mr',		'magic resistance',	Format.Signed,
	'regen',	'regeneration',		Format.Percent,
	
	'spelleffect',	'bearer affected by spell',	Utils.spellRef,
	
	'prot',		'basic protection', 
	'protf',	'protective force', 
	'poisonres',	'resist poison',	Format.Signed,

	'barkskin',	'barkskin',		{1: '10 protection (+1 if already over 10)'},
	'fireres',	'resist fire', 		Format.Signed,

	'stoneskin',	'stoneskin',		{1: '15 protection (+2 if already over 15)'},
	'coldres',	'resist cold', 		Format.Signed,
	'iceprot',	'ice protection', 
	
	'shockres',	'resist shock',		Format.Signed,
	'aff_prot',	'affliction protection',		Format.Percent,
	
	'mor',		'morale bonus',		Format.Signed,
	'exp',		'experience bonus',	Format.SignedPerTurn,
	'research',	'research bonus',	Format.Signed,
	'prc',		'precision bonus',	Format.Signed,
	'ritr_f',	'fire ritual range bonus',	Format.Signed,
	'ritr_a',	'air ritual range bonus',	Format.Signed,
	'ritr_w',	'water ritual range bonus',	Format.Signed,
	'ritr_e',	'earth ritual range bonus',	Format.Signed,
	'ritr_s',	'astral ritual range bonus',	Format.Signed,
	'ritr_d',	'death ritual range bonus',	Format.Signed,
	'ritr_n',	'nature ritual range bonus',	Format.Signed,
	'ritr_b',	'blood ritual range bonus',	Format.Signed,
	
	'sumbat',	'monster autosummons', Utils.unitRef,
	'#sumbat',	'number of autosummons',

	'fshld',	'fire shield',
	'disheal',	'disease healer',
	'heal',		'healer',
	'chill',	'cold aura',
	'bldvng',	'blood vengeance',
	'taskm',	'taskmaster',
	
	'ivylord',		'ivy lord',		function(v){ return '+'+v+' '+Utils.unitRef(361)+' / '+Utils.unitRef(362)+' awakening'; },
	'corpselord',		'corpse lord',		function(v){ return '+'+v+' '+Utils.unitRef(534)+' construction'; },
	
	'startbattlespell',	'casts each battle',	Utils.spellRef,
	'ritual',		'overland spell',	Utils.spellRef,
	'autocombatspell',	'casts every round',	Utils.spellRef,
	'itemspell',		'combat spell',		Utils.spellRef,
	
	'ldr-n',		'leadership',		Format.Signed,
	'ldr-m',		'leadership (magic)',	Format.Signed,
	'ldr-u',		'leadership (undead)',	Format.Signed,
	'ldr-i',		'inspirational leadership',	Format.Signed,
	
	'airtransport',		'map flight',		function(v){ if (v==1) return '(self only)'; else return 'self + total size '+v; },
	'wbreath',	'water breathing',	function(v){ if (v==1) return '(self only)'; else return 'self + total size '+v; },
	'giftwaterbr',	'water breathing',	function(v){ if (v==1) return '(self only)'; else return 'self + total size '+v; },
	'map',		'map move bonus',
	
	'insa',		'bearer grows insane',	function(v){ return '+'+v+'% chance per turn'; },
	'horrormarks',		'horror marks bearer',	function(v){ return v+'% chance per turn'; },
	'brsrk',		'berserk when wounded',	Format.SignedZero,
	'goneberserk',		'gone berserk',		Format.SignedZero,
	'awe',			'awe',			Format.SignedZero,
	'aawe',			'animal awe',		Format.SignedZero,
	'fear',			'fear',			Format.SignedZero,
	
	'standard',		'battle standard (morale)',

	'heretic',		'heretic',

	'sailsz',		'sailing size',
	'maxsail',		'max size of passenger',
	'flytr',		'flying transport',
	
	'ptrl',			'patrol bonus',		Format.Signed,
	'bhunt',		'blood hunt bonus',	Format.Signed,
	'sup',			'supply bonus',		Format.Signed,
	'siege',		'siege bonus',		Format.Signed,
	'forge',		'forge bonus',		Format.Percent,
	'fixforge',		'fixed forge bonus',	
	'pllg',			'pillage bonus',	Format.Signed,
	'stealth',		'stealth',	Format.Signed,
	'stealthb',		'stealth bonus',	Format.Signed,
	'gold', 		'gold generation',	Format.PerTurn, 

	'sumrit',		'ritual summoned unit',	Utils.unitRef,
	'#sumrit',		'ritual summoned amount',
	'sumauto',		'auto summoned unit',	Utils.unitRef,
	'#sumauto',		'auto summoned amount',
	
	'affliction',		'afflicts bearer',	Utils.afflictionRef,
	'cannotwear',		'restriction',		{2:'cannot be worn by mounted units', 1073741824:'cannot be worn by lifeless units'},
	'restrictions',		'restrictions',		
	'special',		'special',		Utils.parseObjectRefs
]);
var flagorder = DMI.Utils.cutDisplayOrder(aliases, formats,
[
//	dbase key	displayed key		function/dict to format value
	'taint',	'horrormark chance',
	'eth',	'ethereal',
	'mount',	'mountain survival',
	'forest',	'forest survival',
	'waste',	'waste survival',
	'swamp',	'swamp survival',
	'nodrop',	'unremovable',
	'bless',	'blessed',
	'trmpl',	'trample',
	'fly',		'flying',
	'quick',	'quickness',
	'disease',	'diseases bearer',
	'reaper',	'spreads disease',
	'xbreed',	'crossbreeding bonus',
	'airbr',	'air breathing',
	'float',	'grants floating',
	'luck',		'lucky',
	'fluck',	'fool\'s luck',
	'curse',	'curses bearer',
	'nopickup',	'won\'t be picked up',
	'haste',	'haste',
	'nodiscount',	'no forge discounts'	
]);
var hiddenkeys = DMI.Utils.cutDisplayOrder(aliases, formats,
[
	'id', 		'item id',
	'armor',	'armor id',	function(v,o){ return v.id+' ('+v.name+')'; }
]);
var modderkeys = Utils.cutDisplayOrder(aliases, formats,
[
	'copyspr',	'copyspr', function(v,o){ return v && v.id!=o.id && v.id; },
	'notes',	'notes'
]);
var ignorekeys = {
	modded:1,
	mpath:1, 
	type:1, 
	weapon:1, 
	armor:1, 
	constlevel:1, 
	mainpath:1, mainlevel:1, secondarypath:1, secondarylevel:1, 
	A:1, B:1, D:1, E:1, F:1, N:1, S:1, W:1, H:1,
	
	gemcost:1,
	wpnclass:1,
	alch:1,
	protb:1,
	dcloud:1,
	
	//common fields
	name:1,descr:1,
	searchable:1, renderOverlay:1, matchProperty:1
};		
	
var formatItemType = {	'2-h wpn':'two handed weapon', 
			'1-h wpn':'one handed weapon', 
			'misc':'miscellaneous',
			'helm':'helmet',
			'shield':'shield',
			'armor':'armor',
			'boots':'boots'
		};
var formatItemCon = {	0:'(lvl 0)',
			2:'(lvl 2)',
			4:'(lvl 4)',
			6:'(lvl 6)',
			8:'(lvl 8 - unique)',
			12:''
		};

MItem.renderOverlay = function(o) {
	var descrpath = 'gamedata/itemdescr/';
	
	//template
	var h=''
	h+='<div class="item overlay-contents"> ';
	
	//header
	h+='	<div class="overlay-header" title="item id: '+o.id+'"> ';
	h+='		<div class="item-image" style="background-image:url(\'images/items/item'+o.copyspr.id+'.png\');">&nbsp;</div> ';
	h+='		<h2>'+o.name+'</h2> ';
	h+='		<p>'+formatItemType[o.type]+' '+formatItemCon[o.constlevel]+'</p>';
	
	//mid
	h+='	</div>';
	h+='	<div class="overlay-main">';
	h+=' 		<input class="overlay-pin" type="image" src="images/PinPageTrns.png" title="unpin" />';
	h+='		<table class="overlay-table"> ';
	h+= 			Utils.renderDetailsRows(o, hiddenkeys, aliases, formats, 'hidden-row');
	h+= 			Utils.renderDetailsRows(o, modderkeys, aliases, formats, 'modding-row');
	h+= 			Utils.renderDetailsRows(o, displayorder, aliases, formats);
	h+= 			Utils.renderDetailsFlags(o, flagorder, aliases, formats);
	h+= 			Utils.renderStrangeDetailsRows(o, ignorekeys, aliases, 'strange');
	
	if (o.modded) {
		h+='	<tr class="modded hidden-row"><td colspan="2">Modded<span class="internal-inline"> [modded]</span>:<br />';
		h+=		o.modded.replace('ERROR:', '<span style="color:red;font-weight:bold;">ERROR:</span>');
		h+='	</td></tr>';
	}
	h+='		</table> ';
	
	//weapon
	if (o.weapon ){//&& modctx.wpnlookup[o.weapon]) {
		var isImplicitWpn = (o.type == '1-h wpn' || o.type == '2-h wpn'); 
		h+= DMI.MWpn.renderWpnTable(o.weapon, isImplicitWpn);
	} 
	h+='	</div>';
	
	//footer
	h+='	<div class="overlay-footer">';
	
	//wikilink
	h+='		<div class="overlay-wiki-link non-content">';
	// h+='			<a class="select-text-button hidden-inline" href="javascript:void(0);">[text]</a>';
	h+='			<a href="http://dom3.servegame.com/wiki/'+o.name.replace(/ /g, '_')+'">[wiki]</a>';
	h+='		</div>';

	//cost
	if (o.constlevel == '12')
		h+='	<p class="firstline">Item cannot be forged.</p>';
	else
		h+='	<p class="firstline">Requires '+Format.Gems(o.gemcost) +' to forge ('+Format.Paths(o.mpath)+')</p>';
	
	//descr
	var uid = 'c'+(Math.random());
	uid = uid.replace('.','');
	h+='		<div class="overlay-descr pane-extension '+uid+'"></div>';
	
	if (o.descr)
			Utils.insertContent( '<p>'+o.descr+'</p>', 'div.'+uid );
	else {
			var url = descrpath + Utils.descrFilename(o.name);
			Utils.loadContent( url, 'div.'+uid );
	}
	h+='	</div> ';
	
	h+='</div> ';
	
	return h;
}


//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
