var kl = new function (){

    var kl = this;

    var loaded = false;
    var runAfterLoad;
    var LiftDataHolder;

    // Knockout works only when node to which binding will be applied is loaded.
    // We don't know which node will be binded, so delay initialization until document is ready
    if ( document.addEventListener ) {
        window.addEventListener( "load", ready, false );
    } else if ( document.attachEvent ) {
        window.attachEvent( "onload", ready );
    }

    function ready(){

        /*
        * LiftDataHolder object is based on DataHolder class by Antonio Salazar Cardozo
        * which he kindly provided for public at https://gist.github.com/Shadowfiend/2639589
        **/

        LiftDataHolder = (function() {

        function LiftDataHolder(attributes, defaults, defaultUndefineds) {
            var attribute, prop, value, _i, _len;
            defaults || (defaults = {});
            defaultUndefineds || (defaultUndefineds = []);
            for (_i = 0, _len = defaultUndefineds.length; _i < _len; _i++) {
                attribute = defaultUndefineds[_i];
                defaults[attribute] = void 0;
            }
            for (prop in defaults) {
                value = defaults[prop];
                if (!attributes[prop]) {
                    attributes[prop] = value;
                }
            }
            this.updateAttributesWith(attributes);
        }

        LiftDataHolder.prototype.setValue = function(attribute, value) {
            var self = this;
            var current;
            if (typeof value === 'undefined' && self[attribute]) {
                return;
            }
            current = self[attribute];
            if (ko.isObservable(value) && ko.isWriteableObservable(current)) {
                return current(value());
            } else if (ko.isWriteableObservable(current)) {
                return current(value);
            } else if (typeof value === 'function') {
                return self[attribute] = value;
            } else if (Array.isArray(value)) {
                var dataHolderArray = ko.utils.arrayMap(value, function(elem){
                   return new LiftDataHolder(elem)
                });
                return self[attribute] = ko.observableArray(dataHolderArray);
            } else {
                return self[attribute] = ko.observable(value);
            }
        };

        LiftDataHolder.prototype.preprocessAttribute = function(attribute, value) {
            if (attribute.match(/Fn$/) && typeof value !== 'function' && value) {
                return os.processFnString(value);
            } else {
                return value;
            }
        };

        LiftDataHolder.prototype.updateAttributesWith = function(attributes) {
            var self = this;
            var attribute, _results;
            _results = [];
            for (attribute in attributes) {
                _results.push(self.setValue(attribute, self.preprocessAttribute(attribute, attributes[attribute])));
            }
            return _results;
        };

        return LiftDataHolder;

        })();

        loaded = true;
        if (runAfterLoad && typeof runAfterLoad === 'function') runAfterLoad()
    }

    var internalSetData = function(data, klGetterName){
        var ldh = new LiftDataHolder({
            list: data
        });
        if (klGetterName)
            kl[klGetterName] = ldh
        else
            kl["data"] = ldh;
        ko.applyBindings(ldh);
    }

    kl.setData = function setData(data, klGetterName){
        if (loaded)
            internalSetData(data, klGetterName)
        else
            runAfterLoad = function (){internalSetData(data, klGetterName)};
    }
}
