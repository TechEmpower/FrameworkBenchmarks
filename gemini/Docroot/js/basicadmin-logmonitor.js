/**
 * Gemini Basic Administration
 * Script for real-time log monitoring
 * Attempts to use WebSockets and falls back to AJAX.
 */

var logmonitor = (function() {

  var uid = 0,
      logBuffer = new Array(),
      dispCurr = 0,
      dispThreshold = 30,
      containReq = "",
      repaintTimeout = 0,
      capture = true,
      track = true,
      ignored = 0,
      $filter = $("#filter"),
      maxLines,
      channelID,
      channel,
      svUrl,
      ajaxProcessor,
      wsProcessor,
      activeProcessor,
      $logData = $("tbody#logdata"),
      $body = $("body"),
      $displayed = $("span#displayed"),
      $captured = $("span#captured");

  /** The WebSocket processor. */
  wsProcessor = (function() {
    function connect() {
      try {
        var url;

        if (svUrl.startsWith("http://")) {
          url = "ws://" + svUrl.substring(7);
        }
        else {
          url = "wss://" + svUrl.substring(8);
        }

        url = url + "?cmd=admin-log-monitor&act=ws&ch=" + channelID;
        var ws = new WebSocket(url, "logmonitor");
      }
      catch (failure) {
        return false;
      }

      // Receive data as a WebSocket event and decode from JSON before
      // calling the distributor.
      ws.onmessage = function(event) {
        process($.parseJSON(event.data));
      }

      // Send a keep-alive message to the server every 30 seconds.
      setInterval(function() {
        ws.send("keep-alive");
      }, 30000);

      return true;
    }

    function process(data) {
      for (var i = 0; i < data.updates.length; i++) {
        debug(data.updates[i].text, data.updates[i].level);
      }
    }

    function pause() {
    }

    function resume() {
    }
    
    return {
      connect: connect,
      pause: pause,
      resume: resume
    };

  })();

  /** The AJAX processor. */
  ajaxProcessor = (function() {
    var ajaxInterval = 0,
        since = 0;

    function processAjaxFetch(data) {
      if (data.uid) {
        since = data.uid;
      }
      if (data.items) {
        for (var i = 0; i < data.items.length; i++) {
          debug(data.items[i].text, data.items[i].level);
        }
      }
    }

    function fetchViaAjax() {
      $.ajax({
        url: svUrl + "?cmd=admin-log-monitor&act=ajax&ch=" + channelID + "&since=" + since,
        dataType: 'json',
        type: 'get',
        data: {},
        success: processAjaxFetch
        });
    }

    function connect() {
      resume();
      return true;
    }

    function pause() {
      window.clearInterval(ajaxInterval);
    }

    function resume() {
      window.clearInterval(ajaxInterval);
      ajaxInterval = window.setInterval(fetchViaAjax, 500);
    }

    return {
      connect: connect,
      pause: pause,
      resume: resume
    };

  })();

  /** Paint details; wire up the filter; and start fetching log items. */
  function init(sv, cid, channels) {
    svUrl = sv;
    channelID = cid;
    channel = channels[cid];
    maxLines = channel.size;

    createSeverityClasses();
    debug("Attaching listener to log file at " + standardDateString(new Date()) + ".", 50);
    displayChannelInfo();

    // Capture keypresses on the filter box.
    $filter.focus().keyup(function(event) {
      var curVal = containReq;
      containReq = this.value.toLowerCase();
      if (curVal != containReq) {
        if (repaintTimeout > 0) {
          window.clearTimeout(repaintTimeout);
        }
        repaintTimeout = window.setTimeout(repaintLog, 100);
      }
    });

    $("#togglecapture").click(toggleCapture);
    $("#toggletrack").click(toggleTrack);
    $("#levels").find(".level").each(function (idx, el) {
      el = $(el);
      el.click(function() {
        setThreshold(el.attr("level"));
      });
    });

    connect();
  }

  /** Connect to the server.  Try WebSockets first and then AJAX. */
  function connect() {
    var good;

    good = wsProcessor.connect();
    if (good) {
      activeProcessor = wsProcessor;
    }
    else {
      good = ajaxProcessor.connect();
      activeProcessor = ajaxProcessor;
    }
  }

  /** Render a summary of the current channel's filter settings. */
  function displayChannelInfo() {
    var toAdd;
    if (channel) {
      toAdd = channel.name
        + " (levels " + channel.min
        + " to " + channel.max
        + (channel.contains ? (" containing \"" + channel.contains + "\"") : "")
        + (channel .regex ? (" matching regex \"" + channel.regex + "\"") : "")
        + ")";
    }
    else {
      toAdd = "Unknown log channel";
    }
    $("span#channel").html(toAdd);
  }

  /** Toggle the pause/capture mode. */
  function toggleCapture() {
    if (capture) {
      capture = false;
      $("div#banner span#controls a#togglecapture").addClass("selected").blur();
      activeProcessor.pause();
    }
    else {
      repaintLog();
      capture = true;
      $("div#banner span#controls a#togglecapture").removeClass("selected").blur();
      activeProcessor.resume();
    }
  }

  /** Toggle the scroll tracking mode. */
  function toggleTrack() {
    if (track) {
      track = false;
      $("div#banner span#controls a#toggletrack").removeClass("selected").blur();
    }
    else {
      jumpToBottom();
      track = true;
      $("div#banner span#controls a#toggletrack").addClass("selected").blur();
    }
  }

  /** Capture a log item and display the item if it meets the client-side filters. */
  function debug(text, level) {
    var logItem = { 'uid': uid++, 'level': level, 'text': text };

    // Remove top item from the buffer if at maximum length.
    if (logBuffer.length >= maxLines) {
      logBuffer.shift();
      if (capture) {
        var toRemove = uid - maxLines - 1;
        $("tr#line" + toRemove).remove();
      }
    }

    // Add the item to our buffer.
    logBuffer.push(logItem);

    if (meetsCriteria(logItem)) {
      displaySingle(logItem);
    }

    showStats();
  }

  /** Turns a log item into HTML. */
  function renderSingleAsHtml(logItem) {
    return "<tr id='line" + logItem.uid + "'><td class='content l" + logItem.level + "'>" + logItem.text + "</td></tr>";
  }

  /** Add a log item into the view assuming we're capturing. */
  function displaySingle(logItem) {
    if (capture) {
      var toAdd = renderSingleAsHtml(logItem);
      dispCurr++;
      $logData.append(toAdd);
      jumpIfFollow();
    }
    else {
      ignored++;
    }
  }

  /** If we're following the log, scroll down. */
  function jumpIfFollow() {
    if (track) {
      jumpToBottom();
    }
  }

  /** Scroll to the bottom of the page. */
  function jumpToBottom() {
    $body.scrollTop(10000000);
  }

  /* Adjust the minimum threshold and repaint. */
  function setThreshold(newThreshold) {
    dispThreshold = newThreshold;
    $("div#banner span#levels a").removeClass("selected").blur();
    $("div#banner span#levels a#t" + dispThreshold).addClass("selected");
    repaintLog();
  }

  /** Clear the view and re-render it entirely given current client-side settings. */
  function repaintLog() {
    dispCurr = 0;
    var toAdd = "";
    for (var i = 0; i < logBuffer.length; i++) {
      if (meetsCriteria(logBuffer[i])) {
        toAdd += renderSingleAsHtml(logBuffer[i]);
        dispCurr++;
      }
    }

    $logData.html(toAdd);

    showStats();
    jumpIfFollow();
  }

  /** Determines if a log item meets the current client-side criteria. */
  function meetsCriteria(logItem) {
    if (logItem.level >= dispThreshold) {
      return (  (containReq == "")
             || (logItem.text.toLowerCase().indexOf(containReq) >= 0)
             );
    }
    return false;
  }

  /** Display how many lines are visible versus captured. */
  function showStats() {
    var displayed = $logData.children().length;
    $displayed.text(displayed);
    $captured.text(logBuffer.length);
  }

  /** Create a gradient of colors for log severity. */
  function createSeverityClasses() {
    var sr = 168, sg = 168, sb = 128, offset = 0,
        toAdd = "";

    function createGradient(dr, dg, db, num) {
      var res = "";
      for (i = 0; i < num; i++) {
        res += ".l" + (offset++) + " { color: rgb(" + sr + "," + sg + "," + sb + "); } ";
        sr += dr; if (sr > 255) sr = 255; if (sr < 0) sr = 0;
        sg += dg; if (sg > 255) sg = 255; if (sg < 0) sg = 0;
        sb += db; if (sb > 255) sb = 255; if (sb < 0) sb = 0;
      }

      return res;
    }

    toAdd += createGradient(-4, -4, 4, 15);  // Start with gray
    toAdd += createGradient(-1, 4, -6, 20);  // Blend to blue
    toAdd += createGradient(-6, -8, -4, 20); // Blend to green
    toAdd += createGradient(-6, -6, -6, 5);  // Blend to black
    toAdd += createGradient(12, 6, 0, 20);   // Blend to orange
    toAdd += createGradient(1, -2, 0, 10);   // Blend to red
    toAdd += createGradient(4, -10, 0, 11);  // Blend to bright red

    $("head").append("<style>" + toAdd + "</style>");
  }

  return {
    init: init
  };

})();
