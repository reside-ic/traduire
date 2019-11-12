var i18next = require("i18next");

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
}

i18next.init({
    lng: "en",
    debug: true,
    resources: resources});

i18next.t("hello");
i18next.t("interpolate", {what: 'i18next', how: 'great'});
i18next.t("pluralex1", {count: 0});
i18next.t("pluralex1", {count: 1});
i18next.t("pluralex1", {count: 2});
i18next.t("pluralex2", {count: 0});
i18next.t("pluralex2", {count: 1});
i18next.t("pluralex2", {count: 2});

i18next.changeLanguage("fr");

i18next.t("hello");
i18next.t("interpolate", {what: 'i18next', how: 'great'});
i18next.t("pluralex1", {count: 0});
i18next.t("pluralex1", {count: 1});
i18next.t("pluralex1", {count: 2});
i18next.t("pluralex2", {count: 0});
i18next.t("pluralex2", {count: 1});
i18next.t("pluralex2", {count: 2});
