// Encapsulated Ciao engine
var CIAOENGINE = {
  run: function(inmod) {
    var Module = inmod;
    // Make FS visible
    if (!Module['preRun']) Module['preRun'] = [];
    Module['preRun'].push(function() { Module['FS'] = FS; });
    // Make ENV visible
    if (!Module['getENV']) Module['getENV'] = function() { return ENV; };
    // Make LZ4 visible
    if (!Module['getLZ4']) Module['getLZ4'] = function() { return LZ4; };

