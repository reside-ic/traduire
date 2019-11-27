global.setTimeout = function(callback, time) {
    callback();
}

// We don't end up with Promise in V8 unless quite recent (and not on
// the version as installed on Ubuntu 18.04 without using a ppa) so we
// need to pull in an ES6 Promise implementation. This is liable to be
// lost from the package.json because npm sees it as extraneous.
global.Promise = require("promise");

global.i18next = require("i18next");

global.init = function(resources, lng, defaultNS, debug, resourcePattern,
                       namespaces, languages, loggerName) {
    var options = {
        "lng": lng,
        // it's important that 'resources' comes through as null, not
        // as {}, if resources are not available or load won't be
        // triggered...that probably needs dealing with somewhere - I
        // think that the option partialBundledLanguages is important
        // here?
        "resources": JSON.parse(resources),
        "debug": debug,
        "initImmediate": true,
        "ns": namespaces,
        "preload": languages,
        "backend": {
            "resourcePattern": resourcePattern
        }
    };
    if (defaultNS) {
        options.defaultNS = defaultNS;
    }
    global.i18next
        .use(traduireLoader())
        .use(traduireLogger(loggerName))
        .init(options);
    return true;
}

global.t = function(x, options) {
    return i18next.t(x, options);
}

global.exists = function(x, options) {
    return i18next.exists(x, options);
}

global.language = function() {
    return i18next.language;
}

global.languages = function() {
    return i18next.languages;
}

global.default_namespace = function() {
    return i18next.options.defaultNS;
};

global.addResourceBundle = function(lng, ns, resources, deep, overwrite) {
    i18next.addResourceBundle(lng, ns, JSON.parse(resources), deep, overwrite);
}

global.traduireLoader = function() {
    return {
        type: 'backend',
        init: function(services, backendOptions, i18nextOptions) {
            this.resourcePattern = backendOptions.resourcePattern;
        },

        read: function(language, namespace, callback) {
            var args = [this.resourcePattern, language, namespace];
            var data = console.r.call("traduire:::i18n_backend_read", args);
            callback(null, JSON.parse(data));
        },

        // optional
        readMulti: function(languages, namespaces, callback) {
        },

        // only used in backends acting as cache layer
        save: function(language, namespace, data) {
            // store the translations
        },

        create: function(languages, namespace, key, fallbackValue) {
            // save the missing translation
        }
    }
};

// For some reason the logger does not get an init method, so we have
// to do the initialisation via an argument to the closure.
global.traduireLogger = function(name) {
    return {
        type: "logger",
        sendMessage: function(level, args) {
            var data = JSON.stringify(args.slice(1));
            var payload = [name, level, args[0], data];
            console.r.call("traduire:::traduire_logger_call", payload);
        },
        log: function(args) {
            this.sendMessage("info", args);
        },
        warn: function(args) {
            this.sendMessage("warn", args);
        },
        error: function(args) {
            this.sendMessage("error", args);
        }
    }
}
