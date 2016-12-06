// single line var
var baz;

// var block
var foo = 5,
	bar;

// control statement
if (foo > 5) {
	console.log('bar');
	foo--;
}

// default:

switch (bar) {
case 0:
	console.log('baz');
	break;
case 1:
	console.log('foo');
	break;
}

switch (baz)
{
case 0: console.log('baz'); break;
case 1:
	console.log('foo'); break;
case 2: console.log('foo'); break;
}

var a, b, c, d;

var array1 = [ a, b,
	c, d ];
var array2 = [
	a,
	b,
	c,
	d
];

console.log('this is something', a, b, c,
	d, e, f, g);

var object1 = { a, b,
	c, d };
var object2 = {
	a, b,
	c, d
};

(function () {
	console.log('iife');
});

function (a, b, c, d,
	e, f, g) {
	console.log('inner');
}

define([
	'a',
	'b',
	'c'
], function (
	a,
	b,
	c
) {
	return {
		d: 1,
		e: 2,
		f: 3
	};
});

// (this should not leave a container open...
console.log('something');

var value = {
	foo: '/baz/${id}/${person}/${task}',
	bar: '/baz'
};

var bob = 'this is a test of something' ||
	'this is a test of something else';

return function webpackDevMiddleware(context, next) {
	const hasNext = applyMiddleware(middleware, context.req, {
		send: content => context.body = content,
		setHeader: function() {context.set.apply(context, arguments)}
	});
}

var foo = something === 'bar' ||
	(index > start + 1 && index < start + 4);

exports.creator = function (param) {
	var qux = this.create('thing', {
		foo: 'foo',
		bar: 'bar'
	}, 'bar', 'baz');

	qux.addEventListener('load', function () {
		doSomething();
	});
}

this.on('[type="checkbox"]:change', function (event) {
	var checkbox = event.target;
	var isChecked = checkbox.checked;
});

for (var i = 0; i < links.length; i++) {
	range.selectNodeContents(links[i]);
	console.log('text: ' + range.toString());
}

if (resultOrError instanceof Error) {
	throw resultOrError;
}
else if (resultOrError === null) {
	var error = new Error('Polling timed out with no result');
	error.name = 'ScriptTimeout';
	throw error;
}

if (
	this.browserName === 'safari' &&
	this.platform === 'MAC' &&
	this.platformName !== 'ios'
) {
	console.log('foo');
}

// vim:noexpandtab tabstop=4
