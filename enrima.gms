*GAMS file for enrimaSMS model
$if NOT set outfile $set outfile outSolenrimaSMS
$onempty
option limrow = 5000;

* Normal Sets
Sets
	a	'Technology age'	/0/
	i(*)	'Energy technology'	//
	k	'Energy type'	/electricity/
	l(*)	'Type of pollutant'	//
	n	'Energy tariff'	/normalRTEp/
	v	'Tree node'	/1/
	m	'Operational profile'	/profile1/
	t	'Short-term period'	/time1/
;

* Sets Aliases
alias (kp,k);
alias (vp,v);

* Subsets
Sets
	Cn(i)	'Continuously-sized technologies'	//
	Ds(i)	'Discretely-sized technologies'	//
	Gen(i)	'Energy-generation technologies'	//
	PU(i)	'Passive technologies (unitary)'	//
	Sto(i)	'Storage technologies'	//
	Cool(k)	'Type of energy for cooling'	//
	Dem(k)	'Types of energy on the demand side'	/electricity/
	Elec(k)	'Type of energy for electricity'	/electricity/
	EP(k)	'Energy to purchase'	/electricity/
	ES(k)	'Energy to be sold'	//
	Heat(k)	'Type of energy for heat'	//
	Ren(k)	'Renewable energy'	//
	TP(n)	'Purchasing tariffs'	/normalRTEp/
	TS(n)	'Sales tariffs'	//
	Fut(v)	'Future nodes'	//
	Root(v)	'Root node'	/1/
	New(a)	'Age = 0'	/0/
	Old(a)	'Age != 0'	//
;

* Multidimensional Sets
Sets
	In(i,k)	'Input energy types for a technology'
		//
	Po(i,k)	'Principal energy of technologies'
		//
	Pur(k,n)	'Purchase tariffs for each energy type'
		/electricity. normalRTEp/
	S(k,n)	'Sales tariffs for each energy type'
		//
	Tr(k,n)	'Energy that can be traded in each market'
		/electricity. normalRTEp/
	Tm(m,t)	'Short-term periods by profile'
		/profile1. time1/
	Ages(i,v,a)	'Possible ages of a technology at a node'
		//
	Out(i,kp)	'Output energy types for a technology'
		//
	Pa(v,vp)	'Parent for each node'
		//
;

* Scalars ---------------------
Scalars
	DR	'Discount rate, per year'	/ 0.05 /
;

* Parameters ----------------

* Variables ----------------

Variables
	xi(i,v)	'Number of units of a technology to be installed'
	xd(i,v,a)	'Number of units of a technology to be decommissioned'
	xc(i,v)	'Available capacity of a technology at each node'
	x(i,v,a)	'Installed units of a given age for each technology and node'
	h(k,n,v)	'Tariff choice'
	y(i,k,v,m,t)	'Energy generator input'
	z(i,k,v,m,t)	'Energy generator output'
	u(k,n,v,m,t)	'Energy to purchase, under a given tariff'
	w(k,n,v,m,t)	'Energy to sell, under a given tariff'
	ri(i,k,v,m,t)	'Energy input to storage'
	ro(i,k,v,m,t)	'Energy output from storage'
	r(i,k,v,m,t)	'Energy stored'
	c	'Total cost'
	e(v,m,t)	'Primary energy consumed per operational period'
;

Positive Variables xc, x, y, z, u, w, ri, ro, r, e;

Integer Variables xi, xd;

Binary Variables h;


$include enrimaEqs.gms

Model enrimaSMS /eqEnergyBal, eqTotalCost/;

solve enrimaSMS using MIP minimizing c ;
scalars modelstat, solvestat, obj;

modelstat = enrimaSMS.modelstat;
solvestat = enrimaSMS.solvestat;
obj = enrimaSMS.objVal;

execute_unload '%outfile%', modelstat, solvestat, obj,
xi, xd, xc, x, h, y, z, u, w, ri, ro, r, c, e;

