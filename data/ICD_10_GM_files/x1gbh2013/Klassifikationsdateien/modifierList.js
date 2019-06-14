function openModifierListWindow(content, clTitle) {

	var buf = [];
	buf.push('<?xml version="1.0" encoding="UTF-8"?>\n');
	buf.push('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n');
	buf.push('<html xml:lang="de" lang="de" dir="ltr" xmlns="http://www.w3.org/1999/xhtml">\n');
	buf.push('<head>\n');
	buf.push('<meta http-equiv="content-language" content="de" />\n');
	buf.push('<meta http-equiv="content-type" content="text/html; charset=UTF-8" />\n');
	buf.push('<title>');
	buf.push('Subklassifikationsliste ' + clTitle);
	buf.push('</title>\n');
	buf.push('<link href="styles.css" rel="stylesheet" media="all" type="text/css"/>\n');
	buf.push('<style type="text/css">div#classicont { margin:20px; }</style>\n');	// overwrite some style here
	buf.push('</head>\n');
	buf.push('<body onLoad="self.focus()">\n');
	buf.push('</body>\n');
	buf.push('</html>');
	
	// Google Chrome bug: http://code.google.com/p/chromium/issues/detail?id=31068 => workaround: open a new window for each link
	var isChrome = navigator.userAgent.toLowerCase().indexOf('chrome') > -1;
	var modifierWindow = window.open('', isChrome ? '' : 'modifierListWindow', 'scrollbars=1,resizable=1,left=400,top=150,height=500,width=400'); // location bar is visible although set to false
	var doc = modifierWindow.document.open();
	doc.write(buf.join(""));
	doc.body.innerHTML = '<div id="classicont">' + content + '</div>\n';
	doc.close();
}

function adaptTitle(linkObjekt) {
	var title = 'Subklassifikationsliste öffnet sich in neuem Fenster';
	if (linkObjekt.getAttribute) {
		linkObjekt.setAttribute('title', title);
	}
}