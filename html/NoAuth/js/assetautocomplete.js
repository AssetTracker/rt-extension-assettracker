jQuery(function() {
    // inputs that accept multiple asset urls
    var multipleCompletion = new Array("([0-9]|new)+-RefersTo");

    // inputs with only a single email address allowed
    var singleCompletion = new Array();

    // inputs for only privileged users
    var privilegedCompletion = new Array();

    // build up the regexps we'll use to match
    var applyto = new RegExp('^(' + multipleCompletion.concat(singleCompletion, privilegedCompletion).join('|') + ')$');
    var acceptsMultiple = new RegExp('^(' + multipleCompletion.join('|') + ')$');
    var onlyPrivileged = new RegExp('^(' + privilegedCompletion.join('|') + ')$');

    var inputs = document.getElementsByTagName("input");

    for (var i = 0; i < inputs.length; i++) {
        var input = inputs[i];
        var inputName = input.getAttribute("name");

        if (!inputName || !inputName.match(applyto))
            continue;

        var options = {
            source: "<% RT->Config->Get('WebPath')%>/Helpers/Autocomplete/Assets"
        };

        var queryargs = [];

        if (inputName.match(onlyPrivileged)) {
            queryargs.push("privileged=1");
        }

        if (inputName.match(acceptsMultiple)) {
            queryargs.push("delim=,");

            options.focus = function () {
                // prevent value inserted on focus
                return false;
            }

            options.select = function(event, ui) {
                var terms = this.value.split(/,\s*/);
                terms.pop(); // remove current input
                terms.push( ui.item.value ); // add selected item
                this.value = terms.join(", ");
                return false;
            }
        }

        if (queryargs.length)
            options.source += "?" + queryargs.join("&");

        jQuery(input).autocomplete(options);
    }
});
