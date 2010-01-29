Ext.BLANK_IMAGE_URL = document.location.href.replace(/\/[^\/]*$/, "/resources/images/s.gif");

function generateRandomName(length)
{
  var charset = "0123456789abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
  var name = "";
  for (var i = 0; i < length; ++i)
    name += charset.charAt(Math.floor(Math.random() * charset.length));

  return name;
}

function setClasses(ui, oldCls, newCls) {
  var clsToAdd = [];

  for (var cls in oldCls)
    if (!(cls in newCls))
      ui.removeClass(cls);

  for (cls in newCls)
    if (!(cls in oldCls))
      ui.addClass(cls);

}

(function() {
  var parts = document.location.toString().replace(/.*?\?/, "").split(/&/);

  for (var i = 0; i < parts.length; i++) {
    var part = parts[i];
    if (!part)
      continue;
    part = part.split("=", 2);
    var val = part.length == 1 ? null : decodeURIComponent(part[1])
    parameters[decodeURIComponent(part[0])] = val == "false" ? false : val;
  }

  parts = document.cookie.split(/\s*;\s*/);
  for (var i = 0; i < parts.length; i++) {
    part = parts[i];
    if (!part)
      continue;
    part = part.split("=", 2);
    parameters[part[0]] = part[1] == "false" ? false : val;
  }
})();

function readableTimestamp(date)
{
    var dayMap = [_("Sunday"), _("Monday"), _("Tuesday"), _("Wednesday"),
                  _("Thursday"), _("Friday"), _("Saturday")];

    var now = new Date();
    var d1 = new Date(now), d2 = new Date(date);

    d1.setHours(0); d1.setMinutes(0); d1.setSeconds(0); d1.setMilliseconds(0);
    d2.setHours(0); d2.setMinutes(0); d2.setSeconds(0); d2.setMilliseconds(0);

    var days = (d1-d2)/24/60/60/1000;
    if (days == 0)
        return date.format("H:i:s");
    if (days == 1)
        return _("Yesterday")+" "+date.format("H:i:s");
    if (days > 1 && days < 6)
        return dayMap[date.getDay()]+" "+date.format("H:i:s");

    return date.format("Y-m-d H:i:s");
}

var _msgCt
function message(msg) {
  if (!_msgCt)
    _msgCt = Ext.DomHelper.insertFirst(document.body, {id:'msg-div'}, true);

  _msgCt.alignTo(document, 'tr-tr', [-5, 0]);
  var m = Ext.DomHelper.append(_msgCt, {html:"<div class='msg'>"+msg+"</div>"}, true);
  m.slideIn('t').pause(10).ghost("t", {remove:true});
}

Ext.DomQuery.pseudos.nodeValueLC = function(c, v) {
  var r = [], ri = -1;

  for(var i = 0, ci; ci = c[i]; i++)
    if(ci.firstChild && ci.firstChild.nodeValue.toLowerCase() == v)
      r[++ri] = ci;

  return r;
}
