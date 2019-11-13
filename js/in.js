// We don't end up with Promise in V8 unless quite recent (and not on
// the version as installed on Ubuntu 18.04 without using a ppa) so we
// need to pull in an ES6 Promise implementation. This is liable to be
// lost from the package.json because npm sees it as extraneous.
global.Promise = require("promise");

global.i18next = require("i18next");
global.i18next_sprintf = require("i18next-sprintf-postprocessor");

global.init = function(resources, lng) {
    var options = {
        "lng": lng,
        "resources": resources,
        "initImmediate": true
    };
    global.i18next
        .use(i18next_sprintf)
        .init(options);
    return true;
}

global.t = function(x, options) {
    return i18next.t(x, options);
}

global.language = function() {
    return i18next.language;
}

global.languages = function() {
    return i18next.languages;
}
