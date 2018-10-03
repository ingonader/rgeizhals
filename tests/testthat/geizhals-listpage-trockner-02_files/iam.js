/*DO NOT HOST THIS SCRIPT ON YOUR OWN SERVER*/
var szmvars = "";
var iom = iom || (function () {
  var dummySite = "dummy",
      baseUrlDE = "de.ioam.de/tx.io",
      baseUrlLSO = "de.ioam.de/aid.io",
      optinUrl = "de.ioam.de/optin.php?re=",
      qdsUrl = "irqs.ioam.de",
      cntBaseUrl = ".iocnt.net/tx.io",
      cntBaseUrlLSO = ".iocnt.net/aid.io",
      cntOptinUrl = ".iocnt.net/optin.php?re=",
      cntQdsUrl = "irqs.iocnt.net",
      cntSubdomain = ["at", "imarex"],
      eventList = ["", "inst", "init","open", "clse", "play", "resm", "stop", "fowa", "bakw", "recd", "paus", "forg", "bakg", "dele", "refr", "kill", "view", "alve", "fini", "mute", "aforg", "abakg", "aclse", "sple", "scvl", "serr", "spyr", "smdr", "sfpl", "sfqt", "ssqt", "stqt", "soqt", "sofc", "scfc", "scqt", "splr", "spli", "sprs", "spre", "smrs", "smre", "sors", "sore", "sack", "sapl", "sapa", "snsp"],
      LSOBlacklist = [],
      checkEvents = 1,
      tb = 0,
      sv = 1,
      lastEvent = "",
      emptyCode = "Leercode_nichtzuordnungsfaehig",
      autoEvents = {
        onfocus:"aforg",
        onblur:"abakg",
        onclose:"aclse"
      },
      nt = 2,
      cookiewhitelist = ["infonlin", "housectr"],
      socioToken = "8226b5d5bb4a9412939ba335a7e9d339",
      frequency = 60000,
      hbiAdShort = 5000,
      hbiAdMedium = 10000,
      hbiAdLong = 30000,
      hbiShort = 10000,
      hbiMedium = 30000,
      hbiLong = 60000,
      hbiExtraLong = 300000,
      heart;

  var IAMPageElement = null,
      IAMQSElement = null,
      qdsParameter = {},
      qdsPopupBlockDuration = 86400000,
      result = {},
      mode,
      eventsEnabled = 0,
      surveyCalled = 0,
      inited = 0;

  var lsottl = 86400000,
      lsottlmin = 180000,
      ioplusurl = "me.ioam.de";

  function enableEvents() {
    if ((tb == 1 || result.tb == "on") && result.tb != "off" && !eventsEnabled) {
      eventsEnabled = 1;
      mode = 1;
      for(var e in autoEvents) {
        (function(e) {
          var oldEvent = window[e];
          window[e] = function() {
            if (lastEvent != autoEvents[e]) {
              lastEvent = autoEvents[e];
              event(autoEvents[e]);
            }
            if (typeof oldEvent == "function") oldEvent();
          };
        })(e);
      }
    }
  }

  function isDoNotTrack() {
    if ((nt & 2) ? ((typeof result.nt == "undefined") ? (nt & 1) : result.nt) : nt & 1) {
      if (window.navigator.msDoNotTrack && window.navigator.msDoNotTrack == "1") return true;
      if (window.navigator.doNotTrack && (window.navigator.doNotTrack == "yes" || window.navigator.doNotTrack == "1")) return true;
    }
    return false;
  }

  var getInvitation = function (response) {
    if (response && response.hasOwnProperty("block-status")){
      var isEligibleForInvitation = ( "NONE" === response['block-status'].toUpperCase() );
      if (isEligibleForInvitation) {
        if (IAMQSElement) {
          IAMQSElement.parentNode.removeChild(IAMQSElement);
        }
        IAMQSElement = createScriptTag(response['invite-url']);
      }
    }
  };

  function loadSurvey() {
    szmvars = result.st + "//" + result.pt + "//" + result.cp + "//VIA_SZMNG";
    var sampleType = (result.sv == "i2") ? "in" : result.sv;
    var qdsHost = qdsUrl;
    if (result.cn) {
      sampleType += "_"+result.cn;
      if (result.cn == "at") {
        qdsHost = cntQdsUrl;
      }
    }

    qdsParameter = {
      siteIdentifier: result.cp,
      offerIdentifier: result.st,
      sampleType: sampleType,
      pixelType: result.pt,
      contentType: result.cp,
      host: qdsHost,
      port: "",
      isFadeoutFlash: true,
      isFadeoutFrame: true,
      isFadeoutForm: true,
      positionTop: 10,
      positionLeft: 100,
      zIndex: 1100000,
      popupBlockDuration: qdsPopupBlockDuration,
      keysForQueryParam : [
          "offerIdentifier",
          "siteIdentifier",
          "sampleType",
          "pixelType",
          "isFadeoutFlash",
          "isFadeoutFrame",
          "isFadeoutForm",
          "positionTop",
          "positionLeft",
          "zIndex"]
    };

    if(typeof window.iam_zindex !== 'undefined') {
      qdsParameter.zIndex = window.iam_zindex;
    }

    if(typeof window.iam_fadeout_flash !== 'undefined') {
      qdsParameter.isFadeoutFlash = window.iam_fadeout_flash;
    }

    if(typeof window.iam_fadeout_iframe !== 'undefined') {
      qdsParameter.isFadeoutFrame = window.iam_fadeout_iframe;
    }

    if(typeof window.iam_fadeout_form !== 'undefined') {
      qdsParameter.isFadeoutForm = window.iam_fadeout_form;
    }

    if(typeof window.iam_position_top !== 'undefined') {
      qdsParameter.positionTop = window.iam_position_top;
    }

    if(typeof window.iam_position_left !== 'undefined') {
      qdsParameter.positionLeft = window.iam_position_left;
    }

    var filterObjectByKeys = function (obj, keysToFilter) {
        var result = {}, key;
        var arrayLength = keysToFilter.length;
        for (var i = 0; i < arrayLength; i++) {
            key = keysToFilter[i];
            if (obj.hasOwnProperty(key)) {
                result[key] = obj[key];
            }
        }
        return result;
    };

    var serializeToQueryString = function (obj) {
      var str = [];
      for (var key in obj)
        if (obj.hasOwnProperty(key)) {
        str.push(encodeURIComponent(key) + "=" + encodeURIComponent(obj[key]));
      }
      return str.join("&");
    };

    var createPopupcheckCookie = function (blockDuration) {
      var blockedUntilDate = new Date();
      blockedUntilDate.setTime(blockedUntilDate.getTime() + blockDuration);
      var expires = "expires=" + blockedUntilDate.toUTCString();
      document.cookie = "POPUPCHECK=" + blockedUntilDate.getTime().toString() + ";" + expires + ";path=/";
    };

    var hasPopupcheckCookie = function () {
      var cookie = document.cookie.split(";");
      for (var i = 0; i < cookie.length; i++) {
        if (cookie[i].match("POPUPCHECK=.*")) {
          var currentDate = new Date();
          var now = currentDate.getTime();
          currentDate.setTime(cookie[i].split("=")[1]);
          var blockedUntilTime = currentDate.getTime();
          if (now <= blockedUntilTime) {
            return true;
          }
        }
      }
      return false;
    };

    if (hasPopupcheckCookie()) {
      return;
    }

    if (sv && !surveyCalled && result.sv !== "ke" && result.sv === "dz") {
      surveyCalled = 1;
      iam_ng_nxss();
    }

    if (sv && !surveyCalled && result.sv !== "ke" && (result.sv === "in" || result.sv === "mo" || result.sv === "i2" )) {
      surveyCalled = 1;
      createPopupcheckCookie(qdsParameter.popupBlockDuration);
      var protocol = window.location.protocol;
      var pathOfCheckInvitation = "identitystatus";
      var queryParameter = filterObjectByKeys(qdsParameter, qdsParameter.keysForQueryParam);
      var queryParameterString = "?" + serializeToQueryString(queryParameter);
      if (window.XDomainRequest && document.documentMode === 9) {
        var checkForInvitationUrl = protocol + '//' + qdsParameter.host + '/' + pathOfCheckInvitation + '/identity.js' + queryParameterString+'&callback=iom.gi&c='+Math.random();
        createScriptTag(checkForInvitationUrl);
      } else {
        var checkForInvitationUrl = protocol + '//' + qdsParameter.host + '/' + pathOfCheckInvitation + queryParameterString+'&c='+Math.random();
        var httpRequest = new XMLHttpRequest();
        httpRequest.onreadystatechange = function () {
          if (httpRequest.readyState === XMLHttpRequest.DONE && 200 === httpRequest.status) {
            var response = JSON.parse(httpRequest.responseText);
            getInvitation(response);
          }
        };
        httpRequest.open('GET', checkForInvitationUrl, true);
        httpRequest.withCredentials = true;
        httpRequest.send(null);
      }

    }
  }

  function hash(key) {
    var hash = 0;
    for (var i=0; i<key.length; ++i) {
      hash += key.charCodeAt(i);
      hash += (hash << 10);
      hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    hash = Math.abs(hash & hash);
    return hash.toString(36);
  }

  function activeXDetect() {
    var result = "",
        componentVersion,
        components =[
                     "7790769C-0471-11D2-AF11-00C04FA35D02", "89820200-ECBD-11CF-8B85-00AA005B4340",
                     "283807B5-2C60-11D0-A31D-00AA00B92C03", "4F216970-C90C-11D1-B5C7-0000F8051515",
                     "44BBA848-CC51-11CF-AAFA-00AA00B6015C", "9381D8F2-0288-11D0-9501-00AA00B911A5",
                     "4F216970-C90C-11D1-B5C7-0000F8051515", "5A8D6EE0-3E18-11D0-821E-444553540000",
                     "89820200-ECBD-11CF-8B85-00AA005B4383", "08B0E5C0-4FCB-11CF-AAA5-00401C608555",
                     "45EA75A0-A269-11D1-B5BF-0000F8051515", "DE5AED00-A4BF-11D1-9948-00C04F98BBC9",
                     "22D6F312-B0F6-11D0-94AB-0080C74C7E95", "44BBA842-CC51-11CF-AAFA-00AA00B6015B",
                     "3AF36230-A269-11D1-B5BF-0000F8051515", "44BBA840-CC51-11CF-AAFA-00AA00B6015C",
                     "CC2A9BA0-3BDD-11D0-821E-444553540000", "08B0E5C0-4FCB-11CF-AAA5-00401C608500",
                     "D27CDB6E-AE6D-11CF-96B8-444553540000", "2A202491-F00D-11CF-87CC-0020AFEECF20"
                    ];
    document.body.addBehavior( "#default#clientCaps" );
    for (var i = 0; i < components.length; i++) {
      componentVersion = document.body.getComponentVersion('{' + components[i] + '}', 'ComponentID');
      if ( componentVersion != null ) {
        result += componentVersion;
      } else {
        result += "null";
      }
    }
    return result;
  }

  function fingerprint() {
    var nav = window.navigator, t = nav.userAgent;
    t += getScreen();
    if (nav.plugins.length > 0 ) {
      for (var i = 0; i < nav.plugins.length; i++ ) {
        t += nav.plugins[i].filename + nav.plugins[i].version + nav.plugins[i].description;
      }
    }
    if (nav.mimeTypes.length > 0 ) {
      for (var i = 0; i < nav.mimeTypes.length; i++ ) {
        t += nav.mimeTypes[i].type;
      }
    }
    if ( /MSIE (\d+\.\d+);/.test(nav.userAgent) ) {
      try {
        t += activeXDetect();
      }
      catch(e) {
        //ignore
      }
    }
    return hash(t);
  }

  function createScriptTag(url){
    var el = document.createElement("script");
    el.type = "text/javascript";
    el.src = url;
    var head = document.getElementsByTagName("head")[0];
    if(head) {
      head.appendChild(el);
      return el;
    }
    else return false;
  }

  function createScriptTagAsync(url, cb){
    var el = document.createElement("script");
    el.type = "text/javascript";
    el.src = url;
    el.onload = cb;
    el.async = true;
    var head = document.getElementsByTagName("head")[0];
    if(head) {
      head.appendChild(el);
      return el;
    }
    else return false;
  }

  function transmitData(url, mode) {
    if (url.split("/")[2].slice(url.split("/")[2].length-8) == ".ioam.de" || url.split("/")[2].slice(url.split("/")[2].length-10) == ".iocnt.net") {
      switch (mode) {
        case 1:
          if (IAMPageElement) {
            IAMPageElement.parentNode.removeChild(IAMPageElement);
          }
          IAMPageElement = createScriptTag(url+'&mo=1');
          if (!IAMPageElement) (new Image()).src = url+'&mo=0';
          break;
        case 2:
          (new Image()).src = url+'&mo=0';
          break;
        case 3:
          var IAMsendBox = document.getElementById('iamsendbox'), sendBoxStyle;
          if (IAMsendBox) {
            document.body.removeChild(IAMsendBox);
          }
          IAMsendBox = document.createElement("iframe");
          IAMsendBox.id = "iamsendbox";
          sendBoxStyle = IAMsendBox.style;
          sendBoxStyle.position = "absolute";
          sendBoxStyle.left = sendBoxStyle.top = "-999px";
          IAMsendBox.src = url + "&mo=1";
          document.body.appendChild(IAMsendBox);
          break;
        case 0:
        default:
          document.write('<script src="'+url+'&mo=1"></script>');
      }
    }
  }

  function getScreen() {
    return screen.width + "x" + screen.height + "x" + screen.colorDepth;
  }

  function arrayContains(arr, obj) {
    var i;
    for (i=0;i<arr.length;i++) {
      if (arr[i]==obj) return true;
    }
    return false;
  }

  function transformVar(value) {
    if (!value) value = "";
    value = value.replace(/[?#].*/g, "");
    value = value.replace(/[^a-zA-Z0-9,_\/-]+/g, ".");
    if (value.length > 255) value = value.substr(0,254) + '+';
    return value;
  }

  function getRefHost() {
    var url = document.referrer.split("/");
    return (url.length >= 3) ? url[2] : "";
  }

  function buildResult(params) {
    result = {};
    var i;
    for (i in params) {
      if (params.hasOwnProperty(i)) {
        if (i != "cn" || (i == "cn" && arrayContains(cntSubdomain, params[i]))) {
          result[i] = params[i];
        }
      }
    }
    if (result.hasOwnProperty("fp")) {
      result.fp = (result.fp != "" && typeof result.fp != "undefined") ? result.fp : emptyCode;
      result.fp = transformVar(result.fp);
      result.pt = "FP";
    }
    if (result.hasOwnProperty("np")) {
      result.np = (result.np != "" && typeof result.np != "undefined") ? result.np : emptyCode;
      result.np = transformVar(result.np);
      result.pt = "NP";
    }
    if (result.hasOwnProperty("xp")) {
      result.xp = (result.xp != "" && typeof result.xp != "undefined") ? result.xp : emptyCode;
      result.xp = transformVar(result.xp);
      result.pt = "XP";
    }
    if (result.hasOwnProperty("cp")) {
      result.cp = (result.cp != "" && typeof result.cp != "undefined") ? result.cp : emptyCode;
      result.cp = transformVar(result.cp);
      result.pt = "CP";
    }
    if (!result.pt) {
      result.cp = emptyCode;
      result.pt = "CP";
      result.er = "N13";
    }
    if (!result.hasOwnProperty("ps")) {
      result.ps = "lin";
      result.er = "N22";
    } else {
      if (!(arrayContains(['ack', 'lin', 'pio', 'out'], result.ps))) {
        result.ps = "lin";
        result.er = "N23";
      }
    }
    result.rf = getRefHost();
    result.r2 = document.referrer;
    result.ur = document.location.host;
    result.xy = getScreen();
    result.lo = "AT/Niederosterreich";
    result.cb = "0003";
    result.i2 = "00039e376aee209b65bb4a941";
    result.ep = 1561054893;
    result.vr = "410";
    result.id = fingerprint();
    result.st = result.st ? result.st : dummySite;
    if ((arrayContains(cookiewhitelist, result.st)) || (result.hasOwnProperty("sc") && result.sc == "yes")) {
      result.i3 = FirstPartyCookie();
    }

    if (!result.hasOwnProperty("cn") && result.st.charAt(2) == "_") {
      var cn = result.st.substr(0,2);
      if (arrayContains(cntSubdomain, cn)) {
        result.cn = cn;
      } else {
        result.er = "E12";
      }
    }
  }

  function event(event) {
    var payLoad = "";
    var i;
    event = event || "";
    stopHeart();
    if (inited && !isDoNotTrack() && (!checkEvents || (checkEvents && arrayContains(eventList, event))) && result.ps !== "out") {
      result.lt = (new Date()).getTime();
      result.ev = event;
      var proto = ( window.location.protocol.slice(0,4) === 'http' ) ? window.location.protocol : "https:";
      var baseUrl = baseUrlDE;
      if (result.cn) {
        baseUrl = result.cn + cntBaseUrl;
      }
      if ( !(arrayContains(LSOBlacklist, result.st)) && (((/iPhone/.test(window.navigator.userAgent) || /iPad/.test(window.navigator.userAgent)) && /Safari/.test(window.navigator.userAgent) && !(/Chrome/.test(window.navigator.userAgent))  && !(/CriOS/.test(window.navigator.userAgent))) || (/Maple_2011/.test(window.navigator.userAgent))) ) {
        if (result.cn) {
          baseUrl = result.cn + cntBaseUrlLSO;
        } else {
          baseUrl = baseUrlLSO;
        }
        mode = 3;
        result.u2 = document.URL;
      }
      for (i in result) {
        if (result.hasOwnProperty(i) && i!="cs" && i!="url") {
          payLoad = payLoad + encodeURIComponent(i).slice(0,8) + "=" + encodeURIComponent(result[i]).slice(0,2048) + "&";
        }
      }
      payLoad = payLoad.slice(0,4096);
      result.cs = hash(payLoad);
      result.url = proto + "//" + baseUrl + "?" + payLoad + "cs=" + result.cs;
      transmitData(result.url, mode);
      if (arrayContains(['play', 'resm', 'alve', 'mute', 'sfqt', 'ssqt', 'stqt', 'sapl', 'snsp'], event) && mode === 1 && result.hasOwnProperty('hb')) {
        startHeart();
      }
      return result;
    }
    return {};
  }

  function forwardToOldSZM() {
    if (result.oer === "yes" && !window.IVW && !document.IVW) {
      var SZMProtocol = (window.location.protocol.slice(0,4) === 'http') ? window.location.protocol : "https:";
      var SZMComment = (result.co) ? result.co + "_SENT_VIA_MIGRATION_TAG" : "SENT_VIA_MIGRATION_TAG";
      var SZMCode = (result.oc) ? result.oc : ((result.cp) ? ((result.cp == emptyCode) ? "" : result.cp) : "");
      var SZMContType = (result.pt !== null) ? result.pt : "CP";
      (new Image()).src = SZMProtocol + "//" + result.st + ".ivwbox.de/cgi-bin/ivw/" + SZMContType.toUpperCase() + "/" + SZMCode + ";" + SZMComment + "?r=" + escape(document.referrer) + "&d=" + (Math.random()*100000);
    }
  }

  function count(params, m) {
    init(params,m);
    return event(result.ev);
  }

  function init(params,m) {
    mode = m;
    buildResult(params);
    if (result.sv) {
      result.sv = (result.sv == "in" && mode == 1) ? "i2" : result.sv;
    }
    enableEvents();
    loadSurvey();
    inited = 1;
    forwardToOldSZM();
    return {};
  }

  function hybrid(params,m) {
    init(params,m);
    var ioam_smi = (typeof localStorage === 'object' && typeof localStorage.getItem === 'function') ? localStorage.getItem("ioam_smi") : null;
    var ioam_site = (typeof localStorage === 'object' && typeof localStorage.getItem === 'function') ? localStorage.getItem("ioam_site") : null;
    var ioam_bo = (typeof localStorage === 'object' && typeof localStorage.getItem === 'function') ? localStorage.getItem("ioam_bo") : null;
    if ( ioam_smi !== null && ioam_site !== null && ioam_bo !== null ) {
      result.mi = ioam_smi;
      result.fs = result.st;
      result.st = ioam_site;
      result.bo = ioam_bo;
      if (result.fs == result.st) {
        result.cp = (result.cp.slice(0,10) !== "___hyb2___") ? "___hyb2___"+result.fs+"___"+result.cp : result.cp;
      } else {
        result.cp = (result.cp.slice(0,9) !== "___hyb___") ? "___hyb___"+result.fs+"___"+result.cp : result.cp;
      }
      return event(result.ev);
    } else if ( ioam_smi !== null && ioam_bo !== null ) {
      return {};
    } else {
      if ( window.location.protocol.slice(0,4) !== 'http' || /IOAM\/\d+\.\d+/.test(window.navigator.userAgent) ) {
        return {};
      } else {
        return event(result.ev);
      }
    }
  }

  function setMultiIdentifier(midentifier) {
    if ( localStorage.getItem("ioam_smi") === null || localStorage.getItem("ioam_site") === null || localStorage.getItem("ioam_bo") === null || localStorage.getItem("ioam_smi") !== midentifier ) {
      result.fs = result.st;
      var JsonMIndetifier = null;
      var NewSite = null;
      if ( typeof midentifier === 'string' && typeof JSON === 'object' && typeof JSON.parse === 'function' ) {
        try {
          JsonMIndetifier = JSON.parse(midentifier);
          if (JsonMIndetifier.hasOwnProperty( 'library' )) {
            if (JsonMIndetifier.library.hasOwnProperty( 'offerIdentifier' )) {
              if ( JsonMIndetifier.library.offerIdentifier ) {
                NewSite = JsonMIndetifier.library.offerIdentifier;
              } else {
                result.er = "JSON(E10): offerIdentifier not valid";
              }
            } else {
              result.er = "JSON(E10): no key offerIdentifier";
            }
          } else {
            result.er = "JSON(E10): no key library";
          }
        } catch(err) {
          result.er = "JSON(E10): "+err;
        }
      }
      if ( NewSite !== null ) {
        localStorage.setItem("ioam_site", NewSite);
      }
      result.st = NewSite;
      result.mi = midentifier;
      result.bo = (new Date()).getTime();
      localStorage.setItem("ioam_smi", result.mi);
      localStorage.setItem("ioam_bo", result.bo);
      if (result.fs == result.st) {
        result.cp = (result.cp.slice(0,10) !== "___hyb2___") ? "___hyb2___"+result.fs+"___"+result.cp : result.cp;
      } else {
        result.cp = (result.cp.slice(0,9) !== "___hyb___") ? "___hyb___"+result.fs+"___"+result.cp : result.cp;
      }
      return event(result.ev);
    }
    return {};
  }

  if (typeof window.postMessage != "undefined" && typeof JSON === 'object' && typeof JSON.parse === 'function' && typeof JSON.stringify === 'function') {
    var listener = function(msg) {
      try {
        var msgdata = JSON.parse(msg.data);
      } catch(e) {
        msgdata = { type:false };
      }
      if (typeof msgdata === "object" && msgdata.type === "iam_data") {
        var respObj = {
                       seq : msgdata.seq,
                       iam_data : {
                                   st: result.st,
                                   cp: result.cp
                                  }
                      };
        msg.source.postMessage(JSON.stringify(respObj),msg.origin);
      }
    };
    if (window.addEventListener) {
      window.addEventListener("message", listener);
    } else {
      window.attachEvent("onmessage", listener);
    }
  }


  function optin() {
    var oiurl = ( window.location.protocol.slice(0,4) === 'http' ) ? window.location.protocol : "https:" + "//" + optinUrl;
    var win = window.open(oiurl, '_blank');
    win.focus();
  }

  function startHeart() {
    //IE 9 Compatible
    function heartbeat() {
      return event("alve");
    }
    switch (result.hb) {
        case "adshort":
          frequency = hbiAdShort;
          break;
        case "admedium":
          frequency = hbiAdMedium;
          break;
       case "adlong":
          frequency = hbiAdLong;
          break;
        case "short":
          frequency = hbiShort;
          break;
        case "medium":
          frequency = hbiMedium;
          break;
        case "long":
          frequency = hbiLong;
          break;
        case "extralong":
          frequency = hbiExtraLong;
          break;
        default:
          frequency = 0;
    }
    if (frequency != 0) {
      try {
        heart = setInterval(heartbeat, frequency);
      } catch(e) {
        //pass
      }
    }
  }

  function stopHeart() {
    try {
      clearInterval(heart);
    } catch(e) {
      //pass
    }
  }

  function getUniqueID() {
    var max = 100000000000;
    var min = 999999999999;
    return result.vr + result.cb + String(Math.floor(Math.random() * (max - min + 1)) + min) + fingerprint();
  }

  function FirstPartyCookie() {
    var cookie = document.cookie.split(";");
    for (var i = 0; i < cookie.length; i++) {
      if (cookie[i].match("ioam2017=.*")) {
        var cook = cookie[i].split("=")[1];
        if (hash(cook.split("!")[0]) == cook.split("!")[1]) {
          return cook;
        } else {
          result.er = "N20";
        }
      } else {
        result.er = "N19";
      }
    }
    var blockedUntilDate = new Date();
    blockedUntilDate.setTime(result.ep * 1000);
    var currentDate = new Date();
    var expires = "expires=" + blockedUntilDate.toUTCString();
    if (result.hasOwnProperty("i2")) {
      var cookval = result.i2 + ":" + blockedUntilDate.getTime().toString() + ":" + currentDate.getTime().toString();
    } else {
      var cookval = getUniqueID() + ":" + blockedUntilDate.getTime().toString() + ":" + currentDate.getTime().toString();
    }
    cookval = cookval + "!" + hash(cookval);
    document.cookie = "ioam2017=" + cookval + ";" + expires + ";path=/";
    return cookval;
  }

  function createCORSRequest(method, url) {
    var xdhreq = new XMLHttpRequest();
    if ("withCredentials" in xdhreq) {
      xdhreq.open(method, url, true);
      xdhreq.withCredentials = true;
    } else if (typeof XDomainRequest != "undefined") {
      xdhreq = new XDomainRequest();
      xdhreq.open(method, url);
    } else {
      xdhreq = null;
    }
    return xdhreq;
  }

  function getPlus() {
    if (typeof localStorage === 'object' && typeof localStorage.getItem === 'function') {
      if (localStorage.getItem("ioamplusdata") !== null && localStorage.getItem("ioamplusttl") !== null) {
        var currentDate = new Date();
        var now = currentDate.getTime();
        currentDate.setTime(localStorage.getItem("ioamplusttl"));
        if (now <= currentDate.getTime()) {
          return true;
        }
      }
      var checkForSocio = 'https:' + '//' + ioplusurl + '/soziodata2.php?sc=' + socioToken + '&st=' + result.st + '&id=' + result.id;
      var XHR = createCORSRequest('GET', checkForSocio);
      if (XHR) {
        XHR.onload = function() {
          var response = XHR.responseText;
          var blockedUntilDate = new Date();
          try {
            if ((response.split(":")[1].split(",")[0]) == "0") {
              blockedUntilDate.setTime(blockedUntilDate.getTime() + lsottlmin);
              localStorage.setItem("ioamplusttl", blockedUntilDate.getTime().toString());
              if (localStorage.getItem("ioamplusdata") == null) {
                localStorage.setItem("ioamplusdata", response);
              }
            } else {
              blockedUntilDate.setTime(blockedUntilDate.getTime() + lsottl);
              localStorage.setItem("ioamplusdata", response);
              localStorage.setItem("ioamplusttl", blockedUntilDate.getTime().toString());
            }
          } catch(e) {
            //pass
          }
        };
        XHR.send();
        return true;
      }
    }
    return false;
  }

  return {
    count: count,
    c: count,
    i: init,
    init: init,
    e: event,
    event: event,
    h: hybrid,
    hybrid: hybrid,
    setMultiIdentifier: setMultiIdentifier,
    smi: setMultiIdentifier,
    oi: optin,
    optin: optin,
    getInvitation: getInvitation,
    gi: getInvitation,
    getPlus: getPlus,
    gp: getPlus
  };

})();
