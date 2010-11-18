// jsBrowser.js
//
// This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

$(document).ready( function () { browse("/api/"); } );

function browse (url) {
    function directoryClick (evt) {
        browse($(evt.currentTarget).attr("href"));
    }

    function fileClick (evt) {
        $("h1").html(decodeURI($(evt.currentTarget).attr("href")));
    }

    function handler (data) {
        $("#content").html(restas.jsBrowser.view.directoryBrowse(data));
        $("#content .directory").click(directoryClick);
        $("#content .file").click(fileClick);
    }


    $.getJSON(url, handler);
}