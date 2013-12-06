//namespace scope
(function( DMI, $, undefined ){
		
var MUnit = DMI.MUnit = DMI.MUnit || {};

var Format = DMI.Format;
var Utils = DMI.Utils;

var modctx = DMI.modctx;
var modconstants = DMI.modconstants;


//determines unit type sort order (and identifies commanders)
var unitSortableTypes = {
	'cmdr (cap only)': 	'10.cmdr',
	'commander': 		'11.cmdr',
	'cmdr (u-water)': 	'11.cmdr-uw',
	
	'unit (cap only)': 	'20.unit',
	'unit': 		'21.unit',
	'unit (u-water)': 	'21.unit-uw',
	
	'hero (multi)': 	'30.hero-cmdr',
	'hero (unique)': 	'31.hero-cmdr',
	
	'cmdr (summoned)': 	'40.cmdr-sum',
	'unit (summoned)': 	'41.unit-sum',
	
	'combat summon (cmdr)': '50.combat-cmdr',
	'combat summon':	'50.combat-sum',
	
	'unit (created)':	'45.created',
	
	'pretender': 		'60.pret-cmdr',
	
	'special': 		'70?spec',
	'unit (indy summon)':	'80?unit',
	'cmdr (indy summon)':	'80?cmdr',
	'': 			'90?cmdr' //??
}
function isCmdr(u) {
	return u.sorttype.indexOf('cmdr') != -1 && !u.createdby;
}
function isSummon(u) {
	return u.type.indexOf('summon') != -1;
}


//////////////////////////////////////////////////////////////////////////
// PREPARE DATA
//////////////////////////////////////////////////////////////////////////

MUnit.initUnit = function(o) {
	o.weapons = [];
	o.armor = [];
	o.randompaths = [];
	
	o.hand = '2';
	o.head = '1';
	o.body = '1';
	o.foot = '1';
	o.misc = '2';
	
	o.leader = '40';
	o.sprite = { unitid: o.id };
}

MUnit.prepareData_PreMod = function() {
	for (var oi=0, o;  o= modctx.unitdata[oi];  oi++) {
		
		o.nationname = '';
		o.weapons = Utils.keyListToTable(o, 'wpn');
		
		//consolidate armor types to a single array
		var arr = [];
		for (var k in {armor:1, helmet:1, shield:1, msc:1}) {
			if (!o[k]) continue;
			arr.push(o[k])
			delete o[k];
		}
		o.armor = arr;	
		
		//native sprite object (may be copied to another unit)
		o.sprite = { unitid: o.id };
		
		//random magic paths
		o.randompaths = [];
		var pmasks = {
			128:'F',  256:'A',  512:'W',  1024:'E',  2048:'S',  4096:'D',  8192:'N',  16384:'B',  32768:'H'
		};
		for (var i=1, bf;  bf= o['mask'+i];  i++) {
			bf = parseInt(bf);			delete o['mask'+i];
			var repeat=  parseInt( o['nbr'+i] );	delete o['nbr'+i];
			var levels=  o['link'+i];		delete o['link'+i];
			var chance=  o['rand'+i];		delete o['rand'+i];
			
			var pstr = '';
			for (k in pmasks) if (bf & parseInt(k)) pstr += pmasks[k];
			
			for (var j=0; j<repeat; j++)
				o.randompaths.push({ paths: pstr,  levels: levels || '1',  chance: chance || '100'});
		}
	}
}
	

MUnit.prepareData_PostMod = function() {
	//helpers
	for (var oi=0, o;  o= modctx.unitdata[oi];  oi++) {

		//convert to numbers (for column ordering)
		//doesn't seem to cause any further problems..
		o.id = parseFloat(o.id);
		o.type = '';
		
		o.renderOverlay = MUnit.renderOverlay;
		o.matchProperty = MUnit.matchProperty;
		
		//unique
		o.fullname = o.name;
		if (o.uniquename) {
			if (o.uniquename != 'Random')
				o.fullname = '"'+o.uniquename + '" - '+o.name;
			
			o.unique='1';
		}
		//searchable string
		o.searchable = o.fullname.toLowerCase();
		
		//localise useful functions
		var sum = Utils.sum;
		var negative = Utils.negative;
		var mult = Utils.mult;
		var is = Utils.is;
		var normalise = Utils.normalise;
		
		//magic paths
		o.mpath = '';
		var research = 0;
		for (var i=0; i<modconstants.pathkeys.length; i++) {
			var k = modconstants.pathkeys[i];
			var plevel  = o[k];
			
			// //apply bonus
			//var pboost = o['magicboost_'+k];
			// if (pboost) {
			// 	if (plevel) o[k] = plevel = normalise(sum(plevel, pboost));
			// 	delete o['magicboost_'+k];
			// }
			
			//append to pathcost code
			if (is(plevel)) {
				o.mpath +=  k + plevel + ' '; //string
				
				//add to research total
				if (k != 'H')
					research += parseInt(plevel);
			}
		}		
		//append random magic to pathcode
		if (o.randompaths.length) {
			//avg path bonus
			var n = 0;
			for (var i=0, r; r= o.randompaths[i]; i++)
				n += parseInt(r.levels) * parseInt(r.chance) / 100;
			
			n = Math.floor(n);
			research += n;
			
			o.mpath += 'U' + String(n) + ' ';
		}
		
		//casters have base research 2 
		if (research) 
			research += 2
		//add research bonus
		if (is(o.researchbonus)) 
			research += parseInt(o.researchbonus);
		//append research to pathcode
		if (research) 
			o.mpath += 'R' + String(research) + ' ';
		
		//resource costs
		o.rcost = parseInt(o.rcost);
		o.ressize = parseInt(o.ressize) || 2; //{1:0.5,  2:1,  3:1.5,  4:2,  5:2.5,  6:3}[o.ressize || '2'];

		//filter out weapons we cant find
		var weapons = [];
		for (var i=0, wid, w;  wid= o.weapons[i];  i++) {
			if (! (w= modctx.wpnlookup[wid])) {
				console.log('weapon '+wid+' not found (unit '+o.id+')');
				continue;	
			}
			weapons.push(w);
			
			//backlink on wpn
			Utils.joinArray( Utils.unitRef(o.id), w.used_by )
				
			//add resource cost to unit
			o.rcost += parseInt(w.rcost || '0') * o.ressize / 2;
		}
		o.weapons = weapons;
		
		//create a temp dict of armor by type (so only last of each type will be remembered)
		var adict = {},  armor=[];
		for (var i=0, aid, a; aid= (o.armor || [])[i]; i++) {
			if (! (a= modctx.armorlookup[aid]))  {
				console.log('armor '+aid+' not found (unit '+o.id+')');
				continue;
			}
			adict[a.type] = a;
		}
		//and... now put them back in an array
		for (var k in adict) {
			var a = adict[k];
			armor.push(a);
			
			//backlink on armor
			a.used_by.push( Utils.unitRef(o.id) );
					
			//add resource cost to unit
			o.rcost += parseInt(a.rcost || '0') * o.ressize / 2;
		}
		o.armor = armor;
		
		if (o.rcost > 60000)	o.rcost = 1; //gladiators
		
		//numeric gold and resource costs (for sorting)
		o.gcost = parseInt(o.gcost || '0');
		if (!o.gcost)
			o.rcost = 0;
		else
			o.rcost = Math.floor(o.rcost || 1);
	}
}

//stuff that depends on unit type must come after parsing nation data
MUnit.prepareData_PostNationData = function(o) {
	for (var oi=0, o;  o= modctx.unitdata[oi];  oi++) {

		//clear pretender cost
		if (o.type == 'pretender') {
			//delete o.gcost;
			delete o.rcost;
		}
		//sorttype
		o.sorttype = unitSortableTypes[o.type];

		//show magic paths on grid for commanders only
		if (isCmdr(o) && o.mpath) 
			o.listed_mpath = '0'+o.mpath;
		else o.listed_mpath = '';

		o.holy = o.holy || '';
		//~ o.gcost = o.gcost || '';
		//~ o.rcost = o.rcost || '';

		//add backlinks to units created by other units
		var sumu;
		if (o.domsummon && (sumu= modctx.unitlookup[o.domsummon])) {
			sumu.createdby = sumu.createdby || [];
			sumu.createdby.push(o)
		}
		if (o.makemonster && (sumu= modctx.unitlookup[o.makemonster])) {
			sumu.createdby = sumu.createdby || [];
			sumu.createdby.push(o)
		}
		if (o.summon && (sumu= modctx.unitlookup[o.summon])) {
			sumu.createdby = sumu.createdby || [];
			sumu.createdby.push(o)
		}
		o.unprep = true;
	}
}

//preparation deferred till rendering overlay
MUnit.prepareForRender = function(o) {
	if (o.unprep) {
		delete o.unprep;
		
		var sum = Utils.sum;
		var negative = Utils.negative;
		var mult = Utils.mult;
		var is = Utils.is;
		
		//set sprite url
		if (o.sprite.spr1)
			o.sprite.url = 'mods/' + o.sprite.spr1.replace('.tga', '.png').replace(/^.\//, '')
		else
			o.sprite.url = 'images/sprites/' + Utils.paddedNum(o.sprite.unitid,4)+'_1.png'; 
		
		
		//local helper: apply bonus to stat and add it to tooltip
		o.titles = {};
		function bonus(reason, stat, inc) {
			inc = parseInt(inc);
			if (inc) {
				var oldv =  o[stat] || '0';
				o.titles[stat] =  o.titles[stat]  ?  o.titles[stat]+', '  :  oldv;
				o.titles[stat]+= ' '+Format.Signed(String(inc)) +' ('+reason+')';
				o[stat] = sum(oldv, inc);
			}
		}
		
		//init age
		if (o.age == '0') delete o.age;
		if (o.age == '-1') o.age = '0';
		if (o.maxage == '0') delete o.maxage;
		
		//default age
		if (is(o.inanimate)) {
			if (!o.startage) o.startage = '??';
			if (!o.maxage) o.maxage = '2000??';
			//if (o.E) bonus('earth magic', 'maxage', mult(o.maxage, parseInt(o.E) * 0.5));
		}
		else if (is(o.undead)) {
			if (!o.startage) o.startage = '187';
			if (!o.maxage) o.maxage = '500';
			if (o.D) bonus('death magic', 'maxage', mult(o.maxage, parseInt(o.D) * 0.5));
		}
		else if (is(o.demon)) {
			if (!o.startage) o.startage = '370';
			if (!o.maxage) o.maxage = '1000';
			if (o.B) bonus('blood magic', 'maxage', mult(o.maxage, parseInt(o.B) * 0.5));
		}
		else {
			if (!o.startage) o.startage = '22';
			if (!o.maxage) o.maxage = '50';
			if (o.N) bonus('nature magic', 'maxage', mult(o.maxage, parseInt(o.N) * 0.5));
			if (o.F) {
				if (parseInt(o.maxage) >= 200)
					bonus('fire magic', 'maxage', mult(o.F, -5));
				else if (parseInt(o.maxage) >= 50)
					bonus('fire magic', 'maxage', mult(o.F, -2));
				else if (parseInt(o.maxage))
					bonus('fire magic', 'maxage', mult(o.F, -1));
			}
		}
		//older
		if (is(o.older)) o.startage = sum(o.startage, o.older);
		
		//magic boost
		if (is(o.magicboost_all)) {
			for (var i=0, k; k= modconstants.pathkeys[i]; i++) {
				o['magicboost_'+k] ++;
				o['magicboost'] = Format.Signed(o.magicboost_all);
			}
		}
		var mbstr = '';
		var display_individual_boosts = false;
		for (var i=0, k; k= modconstants.pathkeys[i]; i++) {
			var b = o['magicboost_'+k];
			if (b) {
		 		mbstr += Format.Signed(b) + k + ' ';
		 		if (b != o.magicboost_all)
		 			display_individual_boosts = true;
		 	}
		}
		if (display_individual_boosts)
			o['magicboost'] = mbstr;
			
		
		// var equalboost = true, aboost = o['magicboost_A'];
		
		// for (var i=0, k; k= modconstants.pathkeys[i]; i++) {
		// 	if (o['magicboost_'+k])
		// 		equalboost = false;
		// }
		// if (aboost || !equalboost) {
		// 	var mbstr = '';
		// 	//general boost
		// 	if (equalboost)
		// 		mbstr = Format.Signed(aboost);
		// 	//path specific boosts
		// 	else {
		// 		for (var i=0, k; k= modconstants.pathkeys[i]; i++) {
		// 			if (o['magicboost_'+k])
		// 				mbstr += Format.Signed(o['magicboost_'+k]) + k + ' ';
		// 		}
		// 	}
		// 	o['magicboost'] = mbstr;
		// }
		
		//magic pathcost bonuses
		var n;
		var isldr = is(o.leader);
		if (n= parseInt(o.A)) {
			if (isldr) bonus('air magic', 'magicleader', n*5);
			bonus('air magic','prec', n);
		}
		if (n= parseInt(o.B)) {
			if (isldr) bonus('blood magic', 'magicleader', n*5);
			if (isldr) bonus('blood magic', 'undeadleader', n*5);
		}
		if (n= parseInt(o.D)) {
			//if (isldr) bonus('death magic', 'magicleader', n*5);
			if (isldr) bonus('death magic', 'undeadleader', n*30);
			if (o.fear)
				bonus('death magic', 'fear', n);						
			else if (n >= 5)
				bonus('death magic', 'fear', n-5);
			
		}
		if (n= parseInt(o.S)) {
			if (isldr) bonus('astral magic', 'magicleader', n*10);
		}
		if (n= parseInt(o.E)) {
			if (isldr) bonus('earth magic', 'magicleader', n*5);
			bonus('earth magic', 'prot', n);
		}
		if (n= parseInt(o.F)) {
			if (isldr) bonus('fire magic', 'leader', n*5);  
			//if (isldr) bonus('fire magic', 'leader', n*5);
			if (isldr) bonus('fire magic', 'magicleader', n*5);
			bonus('fire magic', 'att', n);
			
			if (is(o.fireshield))
				bonus('fire magic', 'fireshield', n);
			if (is(o.heat))
				bonus('fire magic', 'heat', n);
		}
		if (n= parseInt(o.N)) {
			if (isldr) bonus('nature magic', 'magicleader', n*5);
			bonus('nature magic', 'supplybonus', n*10);
		}
		if (n= parseInt(o.W)) {	
			if (isldr) bonus('water magic', 'magicleader', n*5);
			bonus('water magic', 'def', n);
			
			if (is(o.cold))
				bonus('water magic', 'cold', n);
		}				

		//formatted leadership
		var ldr_arr = [];
		if (Utils.is(o.leader)) ldr_arr.push(o.leader);
		if (Utils.is(o.undeadleader)) ldr_arr.push('('+o.undeadleader+' undead)');
		if (Utils.is(o.magicleader)) ldr_arr.push('('+o.magicleader+' magic)');
		o.ldr_str = ldr_arr.join(' + ');		
		
		//item slots
		var slotwords = [];
		var slotorder = ['hand','hands',  'head','heads',  'body','bodies',  'foot','feet',  'misc','misc'];
		for (var j=0; j<slotorder.length; j+=2) {
			var t = slotorder[j];
			var n = parseInt(o[t]);
			if (!n)
				continue;
			if (n > 1)
				t = String(n) + ' '+ slotorder[j+1];
			if (t == 'foot')
				t = 'feet';
			slotwords.push(t);
		}			
		if (slotwords.length)
			o.slots = slotwords.join(', ');
		
		//old age
		var oldyears = parseInt(o.startage) - parseInt(o.maxage);
		if (oldyears >= 0) {
			o.isold = '1';
			
			var oldmult = 1 + Math.floor(5 * oldyears / parseInt(o.maxage));
			bonus('old age', 'str', -1 * oldmult);
			bonus('old age', 'att', -1 * oldmult);
			bonus('old age', 'def', -1 * oldmult);
			bonus('old age', 'prec', -0.5 * oldmult);
			bonus('old age', 'enc',  1 * oldmult);
			bonus('old age', 'hp', mult(o.hp, -0.05 * oldmult));
			bonus('old age', 'ap', mult(o.ap, -0.05 * oldmult));
		}
		
		//mounted def bonus
		if (is(o.mounted))
			bonus('mounted', 'def', 3);
				
		//weapons
		var def_wpns = 0;
		for (var i=0, w;  w= o.weapons[i];  i++) 
			def_wpns += parseInt(w.def || '0');
		bonus('weapons', 'def', def_wpns);
			
		//multi weapon penalty
		var countarms = 0;
		var mwpnpen = 0;		
		for (var i=0, w;  w= o.weapons[i]; i++) {
			if (w.wpnclass == 'melee' && !w.bonus) {
				countarms++;
				mwpnpen -= parseInt(w.len || '0');
			}
		}
		if (countarms > 1 && mwpnpen < 0) {
			//ambidextrous
			var ambidextrous = parseInt(o.ambidextrous || '0');
			if (ambidextrous > -mwpnpen) ambidextrous = -mwpnpen;
				
			bonus('dual wield', 'att', mwpnpen);
			bonus('ambidextrous', 'att', ambidextrous);
		}
		
		//wpn att / prec tooltips
		for (var i=0, w;  w= o.weapons[i]; i++) {
			if (w.wpnclass == 'melee') {
				o.titles.att = o.titles.att  ?  o.titles.att+', \n'  :  '';
				o.titles.att += ' '+w.name+'  ->  '+ sum(o.att, w.att);
				
			} else {
				o.titles.prec = o.titles.prec  ?  o.titles.prec+', \n'  :  '';
				o.titles.prec += ' '+w.name+'  ->  '+ sum(o.prec, w.prec);
			}
		}
		
		//protection & encumbrance from armor
		var p_nat = parseInt(o.prot || '0');
		
		var p_body = 0, p_head = 0;
		var def_armor = 0, enc_armor = 0;
		var def_parry = 0;
		for (var i=0, a; a= o.armor[i]; i++) {
			enc_armor += parseInt(a.enc || '0');
			def_armor += parseInt(a.def || '0');
			
			if (a.type == 'armor') 
				p_body = parseInt(a.protbody);

			else if (a.type == 'helm')
				p_head = parseInt(a.prothead);
			
			else if (a.type == 'shield')
				def_parry = a.parry;
			
			else if (a.type == 'misc') { 
				//use misc armor prot instead of basic?                                                
				p_inc = parseInt(a.prot || '0') - p_nat;
				if (p_inc > 0) {
					p_nat += p_inc;
					bonus(a.name, 'prot', p_inc);
				}
			}
		}
		bonus('armor', 'def', def_armor);
		bonus('shield parry', 'def', def_parry);
		
		if (p_body || p_head) {
			//displayed values
			p_body = (p_nat + p_body - (p_nat * p_body/40));
			p_head = (p_nat + p_head - (p_nat * p_head/40));
			var p_total = ((p_body * 4) + p_head) / 5;
			
			// p_total = p_total > 10  ?  math.floor(p_total)  :  

			//display strings			
			o.prot = String(Math.round(p_total));
			o.titles.prot = 'basic '+(o.titles.prot || String(p_nat));
			o.titles.prot += ',  head '+Math.round(p_head)+',  body '+Math.round(p_body);
		}		
		//armor encumbrance 
		if (enc_armor) {
			//casting encumbrance (double armor)
			o.casting_enc = parseInt(o.enc) + (enc_armor*2);
			
			//mounted ignore armor
			if (!is(o.mounted)) {
				//for enc 0 (undead) armor only affects speed
				bonus('armor', 'ap', -enc_armor);
				if (o.enc!='0')
					bonus('armor', 'enc', enc_armor);
			}
			//is caster?
			if (o.mpath) {
				o.titles.enc = o.titles.enc ? o.titles.enc+',  \n' : ''; 
				o.titles.enc += 'spellcasting encumbrance: '+o.casting_enc;
			}
		}
	}
}


//////////////////////////////////////////////////////////////////////////
// DEFINE GRID
//////////////////////////////////////////////////////////////////////////

 function formatGold(_,__,v){ return v || ''; }
 function formatRes(_,__,v){ return v || ''; }
 function formatType(_,__,v,___,o){ return o.type; }
 function formatHoly(_,__,v,___,o){  return v=='1' ?  Format.AbilityIcon('holy', 'sacred')  :  ''; }

MUnit.CGrid = Utils.Class( DMI.CGrid, function() {
		
	//grid columns
	var columns = [
		{ id: "name",     width: 100, name: "Unit Name", field: "name", sortable: true },
		{ id: "nation",   width: 60, name: "Nation", field: "nationname", sortable: true },
		{ id: "type",     width: 60, name: "Type", field: "sorttype", sortable: true, formatter: formatType },
		{ id: "gcost",     width: 30, name: "Gold", field: "gcost", sortable: true, cssClass: "numeric", formatter: formatGold },
		{ id: "rcost",     width: 27, name: "Res", field: "rcost", sortable: true, cssClass: "numeric", formatter: formatRes },		
		{ id: "sacred",     width: 23, name: "Sacred", field: "holy", sortable: true, formatter: formatHoly },
		{ id: "listed_mpath",     width: 100, name: "Magic", field: "listed_mpath", sortable: true, formatter: DMI.GridFormat.OrderedPaths }
	];
	
	this.superClass.call(this, 'unit', modctx.unitdata, columns); //superconstructor
	
	//replace text with holy icon
	$(this.domsel+" div.slick-header-column[title=Sacred] span.slick-column-name").replaceWith(Format.AbilityIcon('holy'));
	
	
	//closure scope
	var that = this;
	
	
	//selecting a nation
	$(that.domselp+" select.nation").bind('change', function(e) {
		//clicked a nation? (or era.. but not "any")
		if (! $(that.domselp+" select.nation option.default").prop('selected')) {
			//currently showing "all units"?
			if ( $(that.domselp+" select.type option.default").prop('selected')) {
				//show only national units
				$(that.domselp+" select.type option.available").prop('selected', true).parent().saveState();
				$(that.domselp+" input.national").prop('checked', true).saveState();
				$(that.domselp+".filters-units input.clear-filters-btn").show();
			}
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
			types: Utils.splitToLookup( $(that.domselp+" select.type").val(), ','),
			
			generic: $(that.domselp+" input.generic:checked").val(),
			national: $(that.domselp+" input.national:checked").val()
		});
		if ($.isEmptyObject(args.types)) delete args.types;
		
		//whole era
		if (args.nation == 'EA' || args.nation == 'MA' || args.nation == 'LA') {
			args.eracode = args.nation;
			delete args.nation;
		}
		else args.nation = modctx.nationlookup[ args.nation ];

		return args;
	}
	
	//apply search
	this.searchFilter =  function(o, args) {
		//type in id to ignore filters
		if (args.str && args.str == String(o.id)) return true;
		
		//search string
		if (args.str && o.searchable.indexOf(args.str) == -1)
			return false;

		//type		
		if (args.types && !args.types[o.type])
			return false;
		
		//national (national units only)
		if (args.national && !(o.nation || o.nations))
			return false;
		//generic (generic units only)
		if (args.generic && (o.nation || o.nations))
			return false;

		//era
		if (args.eracode && o.eracodes) {
			if (!o.eracodes[args.eracode])
				return false;			
		}
		else if (args.eracode && o.nation) {
			if (o.nation.eracode != args.eracode)
				return false;
		}
				
		//nation
		if (args.nation && o.nations) {
			if (!o.nations[args.nation.id])
				return false;
		}
		else if (args.nation && o.nation) {
			if (o.nation != args.nation)
				return false;
		}
		
		//key =~ val
		if (args.key) {
			//need to finalise stats now..
			DMI.MUnit.prepareForRender(o);
			
			var r = o.matchProperty(o, args.key, args.comp, args.val);
			if (args.not  ?  r  :  !r)
				return false;
		}
		return true;
	}	
	
	//customise initial sort
	this.initialSortTrigger = this.domsel+" div.slick-header-column[title=Type]";

	//customise sort	
	this.preSort = function(){
		//bound scope
		var boosterSortPriority = ['F', 'A', 'W', 'E', 'S', 'D', 'N', 'B', 'H', 'U', 'R'];
		var isSortedOnBoosters = false;
		var data = modctx.unitdata;
			
		//the actual callback
		return function(e, args) {
			if (args.sortCol.field == 'listed_mpath') { //boosters == paths
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
					var regex = new RegExp('^.('+pL+'\\d+ )(.*)$');
					//console.log(regex);
					for (var i=0; i<data.length; i++) {
						var b = data[i].listed_mpath;
						if (b && b.indexOf(pL)!=-1)
							data[i].listed_mpath = b.replace(regex, "0$2$1");
					}
				}
				var L = boosterSortPriority[0];
				
				//set first character to number
				for (var i=0; i<data.length; i++) {
					var b = data[i].listed_mpath;
					if (b && b.indexOf(L)!=-1)
						data[i].listed_mpath =  String.fromCharCode(100+parseInt(b.substr(2,2))) + b.substr(1);
				}
				//switch sort column header icon
				if ( $('#unitboosterordericon')
				     .attr({alt:L, src:'images/magicicons/Path_'+L+'.png', 'class':'pathicon Path_'+L})
				     .css('visibility','visible')
				     .length==0 ) 
				{
					//add icon if not exists yet
					$(".slick-header-column[title=Magic]")
					.append('<img id="unitboosterordericon" alt="'+L+'" class="pathicon Path_'+L+'" src="images/magicicons/Path_'+L+'.png" />')
					.find(".slick-sort-indicator").css('visibility','hidden');
				}
				//fix sort direction
				args.sortAsc = false;
				isSortedOnBoosters = true;
			} 
			else  {
				//hide sort column header icon if sorting another column
				$('#unitboosterordericon').css('visibility','hidden');
				isSortedOnBoosters = false;
			}
		}
		//exit bound scope
	}();
	
	this.defaultSortCmp = function(r1,r2) {
		return (r2.gcost - r1.gcost) || (r2.rcost - r1.rcost);
	}	
	
	this.init();
	$(this.domsel+' .grid-container').attachRefMouseEvents();
});
MUnit.matchProperty = DMI.matchProperty;

function chainedUnitRef(o, key, refq) {
	var ref = '<span style="white-space:nowrap;">'+Utils.unitRef(parseInt(o.id))+'</span>';
	//var ref = Utils.unitRef(parseInt(o.id))+'</span>';
	//is already in queue?
	if (Utils.inArray(ref, refq)) {
		if (ref == refq[0]) {
			if (refq.length == 2)
				return '<=> '+refq[1];
			else
				refq.push('(this)');			
		}
		else {
			var i=0; while (refq[i]!=ref) i++;
			refq.push('(repeat from '+i+')');
		}
	}
	else {
		refq.push(ref);
		var nextu;
		if (o[key] && (nextu= modctx.unitlookup[o[key]]))
			return chainedUnitRef(nextu, key, refq);
	}
	var n= 1;
	return '> '+refq.slice(1).join(' > ');
}

//////////////////////////////////////////////////////////////////////////
// OVERLAY RENDERING
//////////////////////////////////////////////////////////////////////////

var aliases = {};
var formats = {};
var displayorder = Utils.cutDisplayOrder(aliases, formats,
[
	//	dbase key	displayed key		function/dict to format value
	'hp',	'hit points',	function(v,o){ return v + ' &nbsp;(size '+o.size+')'; },
	'prot',	'protection',	{'0':'0 '},
	'mor',	'morale',	{'0':'0 '},
	'mr',	'magic res',	{'0':'0 '},
	'enc',	'encumbrance',	{'0':'0 '},
	
	'str',	'strength',	{'0':'0 '},
	'att',	'attack skill',	{'0':'0 '},
	'def',	'defence skill',{'0':'0 '},
	'prec',	'precision',	{'0':'0 '},
	'ap', 	'move',		function(v,o){ return o.mapmove + ' / '+o.ap; }
]);
var displayorder_cmdr = Utils.cutDisplayOrder(aliases, formats,
[
	'ldr_str', 'leadership',
	'mpath',	'magic paths',	function(v,o){
		return Format.Paths(v.replace(/U\d*/, function(s){return Utils.rndMagicRef(Math.floor(o.id), s);}));
	},
	'magicboost',	'magic boost', Format.Paths, 
	'slots',	'item slots'
]);
var displayorder_pret = Utils.cutDisplayOrder(aliases, formats,
[
	'startdom',		'base dominion',
	'pathcost',		'new path cost'
]);
var displayorder2 = Utils.cutDisplayOrder(aliases, formats,
[
	'maxage',	'age',	function(v,o){ return o.startage + ' ('+v+')'; },
	
	'gA',		'generates fire gems',		function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'A')); },
	'gB',		'generates blood slaves',	function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'B')); },
	'gD',		'generates death gems',		function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'D')); },
	'gE',		'generates earth gems',		function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'E')); },
	'gF',		'generates fire gems',		function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'F')); },
	'gS',		'generates astral gems',	function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'S')); },
	'gN',		'generates nature gems',	function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'N')); },
	'gW',		'generates water gems',		function(v){ return v!='0' && Format.PerTurn(Format.Gems(v+'W')); },
	
	'onebattlespell','casts each battle',		Utils.spellRef,
	
	'regeneration',	'regeneration',	Format.Percent,
	'fireres',	'resist fire',	Format.Percent,
	'coldres',	'resist cold',	Format.Percent,
	'poisonres',	'resist poison',Format.Percent,
	'shockres',	'resist shock',	Format.Percent,
	
	'darkvision',	'dark vision',	Format.Percent,
	'stealthy',	'stealthy',	Format.SignedZero,//{0:'+0'},
	'healer',	'healer',
	
	'cold',		'cold aura',
	'heat',		'heat aura',
	'poisoncloud',	'poison cloud',
	'diseasecloud',	'disease cloud',
	'fireshield',	'fire shield',
	'banefireshield','banefire shield',
	'bloodvengeance','blood vengeance', Format.SignedZero,
	
	'iceprot',	'ice protection',
	'firepower',	'fire power',
	'stormpower',	'storm power',
	'coldpower',	'cold power',
	'darkpower',	'dark power',
	
	'springpower',	'spring power',
	'summerpower',	'summer power',
	'fallpower',	'fall power',
	'winterpower',	'winter power',
	
	'fear',		'fear',		Format.SignedZero,
	'awe',		'awe',		Format.SignedZero,
	'sacredawe',	'halt heretic',	Format.SignedZero,
	'animalawe',	'animal awe',	Format.SignedZero,
	
	'standard',	'standard',
	
	'ambidextrous',	'ambidextrous',
	'reinvigoration','reinvigoration',
	'berserk',	'berserker',		Format.SignedZero,
	
	'supplybonus',	'supply bonus',		Format.Signed,
	'siegebonus',	'siege bonus',		Format.Signed,
	'castledef',	'castle defence',	Format.Signed,
	'patrolbonus',	'patrol bonus',		Format.Signed,
	'pillagebonus',	'pillage bonus',	Format.Signed,
	'alchemy',		'alchemy bonus',	Format.Percent,
	'forgebonus',	'forge bonus',		Format.Percent,
	'douse',	'blood hunt bonus',	Format.Signed,
	'nobadevents',	'fortune teller',	Format.Percent,
	'spreaddom',	'spreads dominion',
	'incunrest',	'increases unrest',	Format.SignedPerTurn,
	
	'seduce',	'capture cmdr (seduction)',	function(v){ if (v=='0') return '0'; return 'morale vs '+v; },
	'succubus',	'capture cmdr (succubus)',	function(v){ if (v=='0') return '0'; return 'morale vs '+v; },
	'corruption',	'capture cmdr (corruption)',	function(v){ if (v=='0') return '0'; return 'morale vs '+v; },
	'beckon',	'lure cmdr into sea',		function(v){ if (v=='0') return '0'; return 'morale vs '+v; },
	
	
	'leper',	'leper',		Format.Percent,
	'popkill',	'kills population',	function(v,o){ return Format.PerTurn( parseInt(v)*10 ) },
	'homesick',	'homesick',		Format.Percent,
		
	'special',	'special',
	
	'firstshape',	'natural shape',	function(v,o){	return chainedUnitRef(o, 'firstshape', []);	},
	'secondshape',	'wounded shape',	function(v,o){	return chainedUnitRef(o, 'secondshape', []);	},
	'shapechange',	'shape changer',	function(v,o){	return chainedUnitRef(o, 'shapechange', []);	},
	'secondtmpshape','dying shape',	function(v,o){	return chainedUnitRef(o, 'secondtmpshape', []);	},
	'landshape',	'land shape',	function(v,o){	return chainedUnitRef(o, 'landshape', []);	},
	'watershape',	'sea shape',	function(v,o){	return chainedUnitRef(o, 'watershape', []);	},
	'forestshape',	'forest shape',	function(v,o){	return chainedUnitRef(o, 'forestshape', []);	},
	'plainshape',	'normal shape',	function(v,o){	return chainedUnitRef(o, 'plainshape', []);	},
	'prophetshape',	'prophet shape',	function(v,o){	return chainedUnitRef(o, 'prophetshape', []);	},
	
	'domsummon',	'dominion attracts units',	function(v,o){ 
		return Format.PerTurn( (o.n_domsummon || '1')+' x '+Utils.unitRef(v) ); 
	},
	'makemonster',	'makes units',	function(v,o){ 
		return Utils.is(o.n_makemonster) ?  Utils.unitRef(v)+' x '+o.n_makemonster  :  Utils.unitRef(v); 
	},
	'summon',	'automatically summons',function(v,o){ 
		return Format.PerTurn( (o.n_summon || '1')+' x '+Utils.unitRef(v) );
	},		
	'batsum',	'summons in battle',	function(v,o){ 
		return Utils.is(o.n_batsum) ?  Utils.unitRef(v)+' x '+o.n_batsum  :  Utils.unitRef(v); 
	},
	
	'heretic',		'heretic',
	'shatteredsoul',	'shattered soul', 	Format.Percent, //tartarian
	'insane',	'insane',		Format.Percent,
	
	'void',		'void summoning',	Format.Signed //rl'yeh	
	
]);
var flagorder = Utils.cutDisplayOrder(aliases, formats,
[
//	dbase key	displayed key		function/dict to format value
	'unique',	'unique',
	'immortal',	'immortal',
	'isold',	'old age',
	'holy',		'sacred',
	'sailing',	'sailor',
	'mounted',	'mounted',
	'immobile',	'immobile',
	
	'animal',	'animal',
	'undead',	'undead',
	'demon',	'demon',
	'magicbeing',	'magic being',
	'inanimate',	'inanimate',
	'mind',		'mindless',
	'ethereal',	'ethereal',
	'ethtrue',	'true ethereal',
	'illusion',	'glamour',	
	'flying',	'flying',
	'stormimmune',	'flies in storms',
	
	'trample',	'trample',
	'blind',	'blind fighter',
	'heal',		'recuperation',
	'spy',		'spy',
	'assassin',	'assassin',
	'drainimmune',	'ignores drain scales',
		
	'coldblood',	'cold blooded',
	'pooramphibian','poor amphibian',
	'amphibian',	'amphibious',
	'aquatic',	'aquatic',
	'neednoteat',	'need not eat',
	'noheal',	'only heals in lab',
	
	'wastesurvival',	'wasteland survival',
	'mountainsurvival',	'mountain survival',
	'swampsurvival',	'swamp survival',
	'forestsurvival',	'forest survival',
	
	'female',	'female',
	'stonebeing',	'stone being',
	'plague',	'spreads plague',
	
	'poisonarmor',	'poison barbs',
	'petrify',	'petrifies attackers',
	'entangle',	'entangles attackers',
	'eyeloss',	Utils.afflictionRef('Eyeloss')+' on attackers',
	
	'inquisitor',		'inquisitor'
]);
var hiddenkeys = Utils.cutDisplayOrder(aliases, formats,
[
	'id', 		'unit id',	function(v){ return Math.floor(v); }
]);
var modderkeys = Utils.cutDisplayOrder(aliases, formats,
[
	'nametype',	'#nametype',
	//'dupes',	'duplicates',	function(v,o){ return v.length; },
	
	'ressize',	'#ressize',
	'eyes',		'#eyes',
	'sprite',	'sprite', function(v,o){ return v && v.url; }
]);
var ignorekeys = {
	modded:1,
	dupes:1,
	sorttype:1,	
	
	titles:1, fullname:1, 
	size:1, uniquename:1,
	
	leader:1,
	magicleader:1,
	undeadleader:1,
	
	mapmove:1,
	startage:1, older:1,
	casting_enc:1,	
	
	searchable:1,
	notes:1,
	
	researchbonus:1, listed_mpath:1, 
	n_domsummon:1, n_makemonster:1, n_batsum:1, n_summon:1,	
	
	hand:1, head:1, body:1, foot:1, misc:1, 
	

	A:1, B:1, D:1, E:1, F:1, N:1, S:1, W:1, H:1, randompaths:1,
	magicboost_A:1, magicboost_B:1, magicboost_D:1, magicboost_E:1, magicboost_F:1, magicboost_N:1, magicboost_S:1, magicboost_W:1, magicboost_H:1,
	magicboost_all:1,
	
	gcost:1, rcost:1,
	
	weapons:1, armor:1, helmet:1, shield:1, wpn1:1, wpn2:1, wpn3:1, wpn4:1, wpn5:1, wpn6:1,
	
	eracodes:1, nations:1, nation:1, nationname:1, type:1,
	summonedby:1, createdby:1,

	//common fields
	name:1,descr:1,
	searchable:1, renderOverlay:1, matchProperty:1
};	

MUnit.renderOverlay = function(o) {
	MUnit.prepareForRender(o);		
	var descrpath = 'gamedata/unitdescr/';
	
	//template
	var h=''
	h+='<div class="unit overlay-contents">';
	
	//header
	h+='	<div class="overlay-header" title="unit id: '+o.id+'"> ';
	h+=' 		<input class="overlay-pin" type="image" src="images/PinPageTrns.png" title="unpin" />';
	// if (o.uniquename && o.uniquename != 'Random')
	// 	h+='	<h2>"'+o.uniquename + '" - '+o.name+'</h2> ';
	// else
	// 	h+='	<h2>'+o.name+'</h2> ';
	
	
	h+='		<h2>'+o.fullname+'</h2> ';
	
	//nation/commander info
	var nname = o.nation ?  Utils.nationRef(o.nation.id) : o.nationname;
	var ntitle = '';
	if (o.nations) {
		var nnlist = []; 
		for (var k in o.nations) {
			nname = Utils.nationRef( o.nations[k].id );//.fullname;
			nnlist.push(o.nations[k].shortname);
		}
		if (nnlist.length > 1) {
			nname = o.nationname.replace('various', 'various nations');
			ntitle = 'title="'+nnlist.join(',  \n')+'"';
		}
	}
	if (nname || o.type) {
		h+='	<p style="float:right; clear:right">'+(o.type || '&nbsp;')+'</p> ';
		h+='	<p '+ntitle+'>'+(nname || '&nbsp;')+'</p> ';
	}
	h+='	</div>';
			
	//body
	h+='	<div class="overlay-main">';
	h+='	<img style="float:right; clear:right;" src="'+o.sprite.url+'" />';
	h+='	<div style="float:right; clear:right; max-width:50%;">';
	var tags = [];
	for (var i=0; i<o.weapons.length; i++)
		tags.push(Utils.wpnRef(o.weapons[i].id));
	if (tags.length)
		h+='	<p>Weapons:<br />'+ tags.join('<br /> ') +'</p>';		
	
	var tags = [];
	for (var i=0; i<o.armor.length; i++)
		tags.push(Utils.armorRef(o.armor[i].id));
	if (tags.length)
		h+='	<p>Armor:<br />'+ tags.join('<br /> ') +'</p>';
	h+='	</div>';	
	
	
	h+='		<table class="overlay-table"> ';
	h+= 			Utils.renderDetailsRows(o, hiddenkeys, aliases, formats, 'hidden-row');
	h+= 			Utils.renderDetailsRows(o, displayorder, aliases, formats);
	h+=' 		</table> ';
	
	h+='		<table class="overlay-table"> ';
	// h+= 			Utils.renderDetailsRows(o, displayorder_cmdr, aliases, formats, isCmdr(o) ? '' : 'hidden-row');
	h+= 			Utils.renderDetailsRows(o, displayorder2, aliases, formats);
	h+= 			Utils.renderStrangeDetailsRows(o, ignorekeys, aliases, 'strange');
	h+=' 		</table> ';
	
	var flagrows =		Utils.renderDetailsFlags(o, flagorder, aliases, formats);
	if (flagrows) h+='<p style="margin-top:0px;padding-top:0px;"> '+flagrows+'</p>';
	
	
	//commander details
	h+='		<table class="overlay-table commander '+(isCmdr(o) ? '' : 'hidden-block')+'"> ';
	h+= 			Utils.renderDetailsRows(o, displayorder_cmdr, aliases, formats);
	h+= 			Utils.renderDetailsRows(o, modderkeys, aliases, formats, 'modding-row');
	h+='		</table> ';
	
	//pretender details
	// h+='		<table class="overlay-table pretender '+(o.type=='pretender' ? '' : 'hidden-block')+'"> ';
	// h+= 			Utils.renderDetailsRows(o, displayorder_pret, aliases, formats);
	// h+='		</table> ';

	if (o.modded) {
		h+='	<div class="modded hidden-block">Modded<span class="internal-inline"> [modded]</span>:<br />';
		h+=		o.modded.replace('ERROR:', '<span style="color:red;font-weight:bold;">ERROR:</span>');
		h+='	</div>';
	}
	
	//footer
	h+='	</div>';
	h+='	<div class="overlay-footer">';
	//wikilink
	h+='		<div class="overlay-wiki-link non-content">';
	// h+='			<a class="select-text-button hidden-inline" href="javascript:void(0);">[text]</a>';
	h+='			<a href="http://dom3.servegame.com/wiki/'+o.name.replace(/ /g, '_')+'">[wiki]</a>';
	h+='		</div>';

	//source details
	var isfree = false;
	var noupkeep = false;
	if (o.summonedby && isSummon(o)) {
		for (var i=0, refarr=[], s; s= o.summonedby[i]; i++) 
			refarr.push(Utils.spellRef(s.id)); 
		h+='	<p class="firstline">summoned with '+refarr.join(', ')+'</p>';
		isfree = true;
	}
	else if (o.type=='pretender') {
		h+='<p class="firstline">';
		h+= ' Cost<span class="internal-inline"> [gcost]</span>: ' + o.gcost +' pts ';
		
		h+= ' +<span class="internal-inline"> [pathcost]</span> '+o.pathcost + ' pts per magic path';
		// h+='<br />';
		// h+= ' New magic paths cost<span class="internal-inline"> [pathcost]</span>: '+o.pathcost + ' pts ';
		
		h+='<br />';
		h+= ' Dominion<span class="internal-inline"> [startdom]</span>: '+o.startdom;
		h+='</p>';
		isfree = noupkeep = true;
	}
	else if ((!o.type) || o.type=="special") {
		if (o.createdby) {
			for (var i=0, refarr=[], s; s= o.createdby[i]; i++) 
				refarr.push(Utils.unitRef(s.id)); 
			h+='	<p class="firstline">created by '+refarr.join(', ')+'</p>';
			isfree = true;
		}
	}	
	//cost line
	var gunit = ' gold <span class="internal-inline"> [gcost]</span>';
	var runit = ' resources <span class="internal-inline"> [rcost]</span>';
	if (isfree) {
		if (Utils.is(o.gcost) && !noupkeep)
			h+='	<p class="firstline">Upkeep: '+Math.ceil((o.holy ? 0.5 : 1) * parseInt(o.gcost) / 15) +gunit+'</p>';
	}
	else if (o.gcost == '0')
		h+='	<p class="firstline">no gold cost</p>';
	else
		h+='	<p class="firstline">costs&nbsp; '+ o.gcost+gunit +',&nbsp; '+ o.rcost+runit +'</p>';

	
	//descr
	var uid = 'c'+(new Date()).getTime();
	h+='		<div class="overlay-descr pane-extension '+uid+'"></div>';
	
	if (o.descr)
			Utils.insertContent( '<p>'+o.descr+'</p>', 'div.'+uid );
	else {
			 var url = descrpath + Utils.paddedNum(o.id, 4) + '.txt';
			 Utils.loadContent( url, 'div.'+uid );
	}	
	
	h+='	</div> ';
	h+='</div> ';
	return h;	
}


MUnit.renderRandomMagic = function(o) {
	//template
	var h=''
	h+='<div class="random-magic overlay-contents"> ';
	
	//header
	h+='	<div class="overlay-header"> ';
	h+='		<h2>'+o.name+' - random magic</h2>';
	h+='	</div>';
	
	//body
	h+='	<div class="overlay-main">';
	h+=' 		<input class="overlay-pin" type="image" src="images/PinPageTrns.png" title="unpin" />';
	
	h+=' 		<p class="hidden-block">unit id: '+o.id+'</p>';
	
	h+='		<table class="random-magic">';
	h+='			<tr class="header-row">';
	h+='				<th>possible paths</th>';
	h+='				<th>level</th>';
	h+='				<th>chance</th>';
	h+='			</tr>';
	
	for (var i=0, r; r= o.randompaths[i]; i++) {
		h+='		<tr>';
		h+='			<td> '+Format.Paths(r.paths)+' </td>';
		h+='			<td> +'+r.levels+' </td>';
		h+='			<td> '+r.chance+'% </td>';
		h+='		</tr>';
	}		
	h+='		</table>';
	h+='	</div>';
	h+='</div>';
	return h;		
}


Utils.dumpCSV = function(keys, objects) {
	var str = '';
	
	//header
	for (var i=0, k; k= keys[i]; i++)
		str += k+'\t';
	str += '\n';
	
	for (var oi=0, o;  o= objects[oi];  oi++) {
		//object
		for (var i=0, k; k= keys[i]; i++) {
			//format str/num
			if (o[k] && (typeof(o[k])=='string'  ||  typeof(o[k])=='number'))
				str += String(o[k]).replace(/\\[trn]/ig,' ')+'\t';
			//obj.id
			else if (o[k] && typeof(o[k])=='object' && o[k].id)
				str += o[k].id+'\t';
			//?
			else
				str += '\t';
		}
		str += '\n';
	}
	return str;
}
MUnit.dumpCSV = function( showkeys ) {
	for (var oi=0, o;  o= modctx.unitdata[oi];  oi++) {
		MUnit.prepareForRender(o);
	}
	
	if (!showkeys) {
		showkeys = ['id', 'name'].concat(displayorder, displayorder_cmdr, displayorder_pret, displayorder2, flagorder, Utils.objectKeys(ignorekeys));
		Utils.weedArray('modded', showkeys);
		Utils.weedArray('descr', showkeys);
		Utils.weedArray('[object Object]', showkeys);
	}
	return Utils.dumpCSV(showkeys, modctx.unitdata);
}

//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
