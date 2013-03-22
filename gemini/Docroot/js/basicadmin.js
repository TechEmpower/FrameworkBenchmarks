/**
 * Gemini Basic Administration
 */

var gba = (function() {

  /** Pad a number to two digits. */
  function pad(n) {
    return (n < 10) ? '0' + n : n
  }

  /** Render a full date. */
  function standardDateString(d) {
    return d.getFullYear() + '-'
      + pad(d.getMonth() + 1)+'-'
      + pad(d.getDate()) + ' '
      + pad(d.getHours()) + ':'
      + pad(d.getMinutes()) + ':'
      + pad(d.getSeconds());
  }

  /** Render a date object as time only. */
  function standardTimeString(d) {
    return pad(d.getHours()) + ':'
      + pad(d.getMinutes()) + ':'
      + pad(d.getSeconds());
  }

  /** Truncate and add an abbr tag. */
  function abbrTruncate(inputText, desiredLength, abbrMaxLength) {
    if (inputText.length > desiredLength) {
      if (  (abbrMaxLength != undefined)
         && (inputText.length > abbrMaxLength)
         ) {
        return '<abbr title="' + inputText.substring(0, abbrMaxLength - 3) + '...">' + inputText.substring(0, desiredLength - 3) + '...</abbr>';
      } else {
        return '<abbr title="' + inputText + '">' + inputText.substring(0, desiredLength - 3) + '...</abbr>';
      }
    }
    else {
      return inputText;
    }
  }

  /** Render the list of monitored commands in the performance monitor. */
  function renderPerformanceMonitorList(commands, detail, exceptionalCoefficient,
    includeSpecialTime, concurrencyHighlight) {

    var st = false,             // Special time
        cp = false,             // Compact mode
        avgw = 8,               // Width in columns
        worw = 6,               // Width in columns
        totalRequests = 0,
        totalConcurrent = 0,
        lastHourRequests = 0,
        totalDispatches = 0,    // Rough totals for averages across all commands.
        totalQueries = 0,
        totalQueryErrors = 0,
        totalQueryTime = 0,
        totalLogicTime = 0,
        totalRenderTime = 0,
        totalSpecialTime = 0,
        totalTotalTime = 0,
        totalCpuTime = 0,
        worstQueries = 0,       // Worsts across all commands.
        worstQueryErrors = 0,
        worstQueryTime = 0,
        worstSpecialTime = 0,
        worstLogicTime = 0,
        worstRenderTime = 0,
        worstCpuTime = 0,
        content = '';           // The HTML to render.

    if (includeSpecialTime == 1) {
      // Standard mode, do include ST (special time).
      st = true;
      avgw = 9;
      worw = 7;
    }
    else if (includeSpecialTime == 2) {
      // Compact mode.
      cp = true;
      avgw = 6;
      worw = 5;
    }

    for (var i = 0; i < commands.length; i++) {
      content += '<tr><td class="admconfitem">'
        + (!cp ? '<a href="' + detail + commands[i].command + '">' : '')
        + abbrTruncate(commands[i].command, 30)
        + (!cp ? '</a>' : '')
        + '</td><td class="admconfintvalue'
        + ((commands[i].currentload >= concurrencyHighlight) ? ' exceptional' : (commands[i].currentload == 0 ? ' inactive' : '')) + '">'
        + commands[i].currentload + '</td>'
        + '<td class="admconfintvalue">' + commands[i].count + '</td>'
        + '<td class="admconfintvalue' + (commands[i].ci != null ? '">' + commands[i].ci.count : ' inactive">--') + '</td>'
        + '<td></td>';
  
      totalRequests += commands[i].count;
      totalConcurrent += commands[i].currentload;
  
      if (commands[i].ci != null) {
        content += (!cp ? '<td class="admconfintvalue">' + commands[i].ci.avgdisp + '</td>' : '')
          + '<td class="admconfintvalue">' + commands[i].ci.avgqr + '</td>'
          + (!cp ? '<td class="admconfintvalue">' + commands[i].ci.avgqe + '</td>' : '')
          + '<td class="admconfintvalue">' + commands[i].ci.avgqt + '</td>'
          + (st ? '<td class="admconfintvalue">' + commands[i].ci.avgsp + '</td>' : '')
          + '<td class="admconfintvalue">' + commands[i].ci.avglg + '</td>'
          + '<td class="admconfintvalue">' + commands[i].ci.avgrn + '</td>'
          + '<td class="admconfintvalue">' + commands[i].ci.avgcp + '</td>'
          + '<td class="admconfintvalue">' + commands[i].ci.avgto + '</td>'
          + '<td></td><td class="admconfintvalue'
            + ((commands[i].ci.worqr > commands[i].ci.avgqr * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worqr + '</td>'
          + (!cp ? '<td class="admconfintvalue' : '')
            + (!cp ? ((commands[i].ci.worqe > commands[i].ci.avgqe * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worqe + '</td>' : '')
          + '<td class="admconfintvalue'
            + ((commands[i].ci.worqt > commands[i].ci.avgqt * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worqt + '</td>'
          + (st ? '<td class="admconfintvalue' : '')
            + (st ? ((commands[i].ci.worsp > commands[i].ci.avgsp * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worsp + '</td>' : '')
          + '<td class="admconfintvalue'
            + ((commands[i].ci.worlg > commands[i].ci.avglg * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worlg + '</td>'
          + '<td class="admconfintvalue'
            + ((commands[i].ci.worrn > commands[i].ci.avgrn * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worrn + '</td>'
          + '<td class="admconfintvalue'
            + ((commands[i].ci.worcp > commands[i].ci.avgcp * exceptionalCoefficient) ? ' exceptional' : '') + '">' + commands[i].ci.worcp + '</td>';
  
        lastHourRequests += commands[i].ci.count;
        totalDispatches += (commands[i].ci.count * commands[i].ci.avgdisp);
        totalQueries += (commands[i].ci.count * commands[i].ci.avgqr);
        totalQueryErrors += (commands[i].ci.count * commands[i].ci.avgqe);
        totalQueryTime += (commands[i].ci.count * commands[i].ci.avgqt);
        if (st) {
          totalSpecialTime += (commands[i].ci.count * commands[i].ci.avgsp);
        }
        totalLogicTime += (commands[i].ci.count * commands[i].ci.avglg);
        totalRenderTime += (commands[i].ci.count * commands[i].ci.avgrn);
        totalCpuTime += (commands[i].ci.count * commands[i].ci.avgcp);
        totalTotalTime += (commands[i].ci.count * commands[i].ci.avgto);
  
        if (commands[i].ci.worqr > worstQueries) {
          worstQueries = commands[i].ci.worqr;
        }
        if (commands[i].ci.worqe > worstQueryErrors) {
          worstQueryErrors = commands[i].ci.worqe;
        }
        if (commands[i].ci.worqt > worstQueryTime) {
          worstQueryTime = commands[i].ci.worqt;
        }
        if (commands[i].ci.worsp > worstSpecialTime) {
          worstSpecialTime = commands[i].ci.worsp;
        }
        if (commands[i].ci.worlg > worstLogicTime) {
          worstLogicTime = commands[i].ci.worlg;
        }
        if (commands[i].ci.worrn > worstRenderTime) {
          worstRenderTime = commands[i].ci.worrn;
        }
        if (commands[i].ci.worcp > worstCpuTime) {
          worstCpuTime = commands[i].ci.worcp;
        }
      }
      else {
        for (var k = 0; k < avgw; k++) {
          content += '<td class="admconfintvalue inactive">--</td>';
        }
        content += '<td></td>';
        for (var k = 0; k < worw; k++) {
          content += '<td class="admconfintvalue inactive">--</td>';
        }
      }
  
      if (!cp) {
        content += '<td></td><td class="admconfvalue">' + standardTimeString(new Date(commands[i].last.time)) + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.disp + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.queries + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.queryexc + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.querytime + '</td>'
          + (includeSpecialTime == 1 ? '<td class="admconfintvalue">' + commands[i].last.special + '</td>' : '')
          + '<td class="admconfintvalue">' + commands[i].last.logic + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.render + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.cpu + '</td>'
          + '<td class="admconfintvalue">' + commands[i].last.total + '</td>'
          + '</tr>';
      }
    }
  
    content += '<tr class="admplainsubheader"><td colspan="4">Totals</td><td></td>'
      + '<td colspan="' + avgw + '">Averages across all commands</td><td></td>'
      + '<td colspan="' + worw + '">Worst across all commands</td></tr>'
      + '<tr><td class="admconfitem"><b>' + commands.length + ' command' + (commands.length == 1 ? '' : 's')
      + '</b></td><td class="admconfintvalue">' + totalConcurrent + '</td>'
      + '<td class="admconfintvalue">' + totalRequests + '</td>'
      + '<td class="admconfintvalue">' + lastHourRequests + '</td>'
      + '<td></td>';
  
    if (lastHourRequests > 0) {
      content += (!cp ? '<td class="admconfintvalue">' + Math.floor(totalDispatches / lastHourRequests) + '</td>' : '')
        + '<td class="admconfintvalue">' + Math.floor(totalQueries / lastHourRequests) + '</td>'
        + (!cp ? '<td class="admconfintvalue">' + Math.floor(totalQueryErrors / lastHourRequests) + '</td>' : '')
        + '<td class="admconfintvalue">' + Math.floor(totalQueryTime / lastHourRequests) + '</td>'
        + (includeSpecialTime == 1 ? '<td class="admconfintvalue">' + Math.floor(totalSpecialTime / lastHourRequests) + '</td>' : '')
        + '<td class="admconfintvalue">' + Math.floor(totalLogicTime / lastHourRequests) + '</td>'
        + '<td class="admconfintvalue">' + Math.floor(totalRenderTime / lastHourRequests) + '</td>'
        + '<td class="admconfintvalue">' + Math.floor(totalCpuTime / lastHourRequests) + '</td>'
        + '<td class="admconfintvalue">' + Math.floor(totalTotalTime / lastHourRequests) + '</td>'
        + '<td></td>'
        + '<td class="admconfintvalue">' + worstQueries + '</td>'
        + (!cp ? '<td class="admconfintvalue">' + worstQueryErrors + '</td>' : '')
        + '<td class="admconfintvalue">' + worstQueryTime + '</td>'
        + (includeSpecialTime == 1 ? '<td class="admconfintvalue">' + worstSpecialTime + '</td>' : '')
        + '<td class="admconfintvalue">' + worstLogicTime + '</td>'
        + '<td class="admconfintvalue">' + worstRenderTime + '</td>'
        + '<td class="admconfintvalue">' + worstCpuTime + '</td>'
        + '</tr>';
    }
    else {
      for (var k = 0; k < avgw; k++) {
        content += '<td class="admconfintvalue inactive">--</td>';
      }
      content += '<td></td>';
      for (var k = 0; k < worw; k++) {
        content += '<td class="admconfintvalue inactive">--</td>';
      }
    }
  
    return content;
  }
  
  /** Render the current requests in the performance monitor. */
  function renderCurrentRequestList(requests, detail, includeSpecialTime) {

    var st = false,             // Standard mode.
        cp = false,             // Compact mode.
        cols = 8,               // Width in columns.
        totalDispatches = 0,    // Rough totals for averages across all commands.
        totalQueries = 0,
        totalQueryErrors = 0,
        totalQueryTime = 0,
        totalLogicTime = 0,
        totalRenderTime = 0,
        totalSpecialTime = 0,
        totalTotalTime = 0,
        totalCpuTime = 0,
        worstQueries = 0,       // Worsts across all commands.
        worstQueryErrors = 0,
        worstQueryTime = 0,
        worstSpecialTime = 0,
        worstLogicTime = 0,
        worstRenderTime = 0,
        worstCpuTime = 0,
        content = '';           // The HTML to render.

    if (includeSpecialTime == 1) {
      // Standard mode, include ST.
      st = true;
      cols = 9;
    }
    else if (includeSpecialTime == 2) {
      // Compact mode.
      cp = true;
    }

    for (var i = 0; i < requests.length; i++) {
      content += '<tr><td class="admconfitem">'
        + (!cp ? '<a href="' + detail + requests[i].thread + '">' : '')
        + abbrTruncate(requests[i].command, 30)
        + (!cp ? '</a>' : '')
        + '</td><td class="admconfintvalue">' + requests[i].reqnum + '</td>'
        + '<td class="admconfintvalue">' + requests[i].thread + '</td>'
        + '<td class="admconfvalue">' + standardTimeString(new Date(requests[i].time)) + '</td>'
        + '<td></td>'
        + '<td class="admconfintvalue">' + requests[i].disp + '</td>'
        + '<td class="admconfintvalue">' + requests[i].queries + '</td>'
        + '<td class="admconfintvalue">' + requests[i].queryexc + '</td>'
        + '<td class="admconfintvalue">' + requests[i].querytime + ' ms</td>'
        + (st ? '<td class="admconfintvalue">' + requests[i].special + ' ms</td>' : '')
        + '<td class="admconfintvalue">' + requests[i].logic + ' ms</td>'
        + '<td class="admconfintvalue">' + requests[i].render + ' ms</td>'
        + '<td class="admconfintvalue">' + requests[i].cpu + ' ms</td>'
        + '<td class="admconfintvalue">' + requests[i].total + ' ms</td>'
        + '</tr>';

      totalDispatches += requests[i].disp;
      totalQueries += requests[i].queries;
      totalQueryErrors += requests[i].queryexc;
      totalQueryTime += requests[i].querytime;
      totalLogicTime += requests[i].logic;
      totalSpecialTime += requests[i].special;
      totalRenderTime += requests[i].render;
      totalCpuTime += requests[i].cpu;
      totalTotalTime += requests[i].total;
    }

    content += '<tr class="admplainsubheader"><td colspan="4">Totals</td><td></td>'
      + '<td colspan="' + cols + '">Total across all commands</td></tr>'
      + '<tr><td colspan="4" class="admconfitem"><b>' + requests.length + ' command' + (requests.length == 1 ? '' : 's')
      + '</b></td><td></td>';

    content += '<td class="admconfintvalue">' + totalDispatches + '</td>'
      + '<td class="admconfintvalue">' + totalQueries + '</td>'
      + '<td class="admconfintvalue">' + totalQueryErrors + '</td>'
      + '<td class="admconfintvalue">' + totalQueryTime + ' ms</td>'
      + (st ? '<td class="admconfintvalue">' + totalSpecialTime + ' ms</td>' : '')
      + '<td class="admconfintvalue">' + totalLogicTime + ' ms</td>'
      + '<td class="admconfintvalue">' + totalRenderTime + ' ms</td>'
      + '<td class="admconfintvalue">' + totalCpuTime + ' ms</td>'
      + '<td class="admconfintvalue">' + totalTotalTime + ' ms</td>';
      + '</tr>';

    return content;
  }

  /** Use Flot to render a chart of health over time. */
  function renderHealthMonitorFlot(h, timezoneShift, interval, targetElement) {
    var disps = new Array(),
        dispc = new Array(),
        memto = new Array(),
        memfr = new Array(),
        memus = new Array(),
        pages = new Array(),
        pagec = new Array(),
        quers = new Array(),
        querc = new Array(),
        threa = new Array(),
        block = new Array(),
        waits = new Array(),
        blcwa = new Array(),
        lastTime,
        healthOptions,
        healthData;

    // Build the series arrays.
    for (var i = 0; i < h.length; i++) {
      if (h[i] != null) {
        lastTime = h[i].start + timezoneShift;
        disps[i] = [lastTime, h[i].disps];
        dispc[i] = [lastTime, h[i].dispcon];
        if (h[i].totalmem > 0) {
          memto[i] = [lastTime, h[i].totalmem];
          memfr[i] = [lastTime, h[i].freemem];
          memus[i] = [lastTime, h[i].totalmem - h[i].freemem];
        }
        pages[i] = [lastTime, h[i].pages];
        pagec[i] = [lastTime, h[i].pagecon];
        quers[i] = [lastTime, h[i].queries];
        querc[i] = [lastTime, h[i].querycon];
        if (h[i].threads > 0) {
          threa[i] = [lastTime, h[i].threads];
          block[i] = [lastTime, h[i].blocked];
          waits[i] = [lastTime, h[i].waiting];
          blcwa[i] = [lastTime, h[i].waiting + h[i].blocked];
        }
      }
    }
  
    // Flot rendering options.
    healthOptions = {
      xaxis: { mode: "time", timeformat: "%h:%M", minTickSize: [1, "minute"] },
      legend: { position: "nw" },
      yaxis: { labelWidth: 25, min: 0, minTickSize: 1, tickDecimals: 0 },
      y2axis: { labelWidth: 50, min: 0, minTickSize: 1, tickDecimals: 0, tickFormatter: function(number) {
          return Math.floor(number / 1024 / 1024) + " M";
        } },
      selection: { mode: "x" },
      colors: [ "#5060F0" ],
      grid: { hoverable: true, backgroundColor: { colors: ["#FFF", "#FFF", "#FFF", "#F0F4F8"] },
      series: { stack: false } }
    };

    // Data for Flot to render.
    healthData =
      [
       { label: 'Used heap &gt;', color: '#B88810', data: memus, yaxis: 2 },
       { label: '&lt; Pages', color: '#90A0FF', data: pages },
       { label: '&lt; Dispatches', color: '#0000E0', data: disps },
       { label: '&lt; Queries', color: '#C00000', data: quers },
       { label: '&lt; Blocked/Waiting', color: '#009000', data: blcwa },
       { label: 'Total heap &gt;', color: '#C0C0C2', data: memto, yaxis: 2 }
      ];
  
    $.plot($(targetElement), healthData, healthOptions);
  }

  return {
    standardDateString: standardDateString,
    standardTimeString: standardTimeString,
    abbrTruncate: abbrTruncate,
    renderPerformanceMonitorList: renderPerformanceMonitorList,
    renderCurrentRequestList: renderCurrentRequestList,
    renderHealthMonitorFlot: renderHealthMonitorFlot
  };

})();



