// jsBrowser.js
//
// This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

$(document).ready(init);

function init () {
    $("button").click(browse);
}

function browse (url) {
    function handler (data) {
        $("#content").html(restas.jsBrowser.view.directoryBrowse(data));
    }

    $.getJSON("/api/", handler);
}