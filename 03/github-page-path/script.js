
$("#gitpage_20161127 .local").keyup(function () {
    var _path = $("#gitpage_20161127 .local").val();
    _path = $.trim(_path);

    if (_path.substr(0, 1) === '"' || _path.substr(0, 1) === "'") {
        _path = _path.substring(1, _path.length-1);
    }

    var _url = _path;
    _url = _url.replace(/\\/g, "/");

    var _gp_url, _lh_url;
    if (_url.indexOf("/blogger-page/") > -1) {
        _url = _url.substring(_url.indexOf("/blogger-page/")+13, _url.length);

        _gp_url = "https://pulipulichen.github.io/blogger" + _url;
        _lh_url = "http://localhost/blogger-page" + _url;
    }
    else if (_url.indexOf("/blogger-data/") > -1) {
        _url = _url.substring(_url.indexOf("/blogger-data/")+13, _url.length);

        _gp_url = "https://pulipulichen.github.io" + _url;
        _lh_url = "http://localhost/blogger-data" + _url;
    }

    $("#gitpage_20161127 .gitpage").val(_gp_url);
    $("#gitpage_20161127 .localhost").val(_lh_url);
});

$("#gitpage_20161127 .local").keyup();

$("#gitpage_20161127 button.gp").click(function () {
    var _url = $("#gitpage_20161127 .gitpage").val();
    _url = $.trim(_url);
    if (_url !== "") {
        window.open(_url, "_blank");
    }
});

$("#gitpage_20161127 button.lh").click(function () {
    var _url = $("#gitpage_20161127 .localhost").val();
    _url = $.trim(_url);
    if (_url !== "") {
        window.open(_url, "_blank");
    }
});