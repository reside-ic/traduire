global.i18next = require("i18next");
// global.i18next_sprintf = require("i18next-sprintf-postprocessor");

global.init = function() {
    var lng = "en";
    var resources = {
        en: {
            translation: {
                "hello": "hello world",
                "interpolate": "{{what}} is {{how}}",
                "pluralex1": "nose",
                "pluralex1_plural": "noses",
                "pluralex2": "sheep",
                "pluralex2_plural": "sheep"
            }
        },
        fr: {
            translation: {
                "hello": "bonjour le monde",
                "interpolate": "{{what}} est {{how}}",
                "pluralex1": "nez",
                "pluralex1_plural": "nez",
                "pluralex2": "mouton",
                "pluralex2_plural": "moutons"
            }
        }
    };
    var options = {"lng": lng, "resources": resources};
    var callback = function(err, t) {
        if (err) {
            return console.log('something went wrong loading', err);
        }
    };
    //i18next.use(i18next_sprintf).init(options, callback);
    i18next.init(options, callback);
    return true;
}

global.t = function(x, options) {
    return i18next.t(x, options);
}
