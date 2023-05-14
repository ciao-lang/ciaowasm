// Encapsulated Ciao engine (generated with emcc)
var CIAOENGINE = {
  run: function(inmod) {
    var Module = inmod;
    // Make FS visible
    if (!Module['preRun']) Module['preRun'] = [];
    Module['preRun'].push(function() { Module['FS'] = FS; });
    // Make ENV visible
    if (!Module['getENV']) Module['getENV'] = function() { return ENV; };
    // Disable image and audio decoding
    Module.noImageDecoding = true;
    Module.noAudioDecoding = true;
