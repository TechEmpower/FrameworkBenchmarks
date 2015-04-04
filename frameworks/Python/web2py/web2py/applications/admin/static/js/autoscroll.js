/*
 * autoscroll.js widget with jquery event(s)
 * Copyright 2011 Michael Whitford <michael@alwandassociates.com>
 * License: BSD
 */

// toggle - id of the toggle element for the click event
// textarea  - id of the textarea to autoscroll
var toggleAutoScroll = function (toggle, textarea) {
    var autoScrollDebug = false;
    var state = toggle + 'interval';
    var scrollOn = function() {
        if (autoScrollDebug) { console.log('on:', toggle, textarea); }
        var onAnimation = function(name, target) {
            jQuery('#' + name).unbind().click(function() {
                toggleAutoScroll(name, target);
            }).fadeTo(350, 1);
        };
        // uses obj attached to window for state
        window[state] = {};
        window[state].name = toggle;
        window[state].target = textarea;
        // get a handle to the textarea el
        var area = jQuery('#' + textarea);
        // set the interval
        window[state].interval = setInterval(function() {
            // ui hack?  needs more testing
            area[0].scrollTop = area[0].scrollHeight;
        }, 2500);  // 2.5 seconds
        onAnimation(toggle, textarea);
    };
    var scrollOff = function() {
        if (autoScrollDebug) { console.log('off:', toggle, textarea); }
        var offAnimation = function(name, target) {
            jQuery('#' + name).unbind().click(function() {
                    toggleAutoScroll(name, target);
                }).fadeTo(350, 0.55);
        };
        offAnimation(toggle, textarea);
        clearInterval(window[state].interval);
        window[state] = undefined;
    };
    if (arguments.length == 2) {
        if (typeof window[state] === 'undefined') {
            scrollOn();
        } else {
            scrollOff();
        }
    }
    return;
};

// jquery onready

jQuery(document).ready(function () {
    // turn it on by default
    toggleAutoScroll('autoscroll', 'output');
});

// todo: some key - toggle off
// todo: drag scrollbar up - toggle off
// todo: drag scrollbar to bottom - toggle off

