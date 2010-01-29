var Model = {}

Model.Connection = function() {
  this.connected = false;

  this.xmppServices = {};

  this.addEvents({
    connected: true,
    disconnect: true,
    errorCatched: true,
    groupsChanged: true,
    currentPresenceChanged: true,
    resourcesChanged: true
  });
}
Model.Connection = new (Ext.extend(Model.Connection, Ext.util.Observable, {
  connect: function(domain, user, pass) {
    if (!domain)
      domain = parameters.domain ||
        document.location.toString().replace(/(?:jar:)?\w+:\/\/([^:\/]+).*$/, "$1");

    var httpbase = document.location.toString().
      replace(/(?:jar:)?(\w+:\/\/[^:\/]+(?::\d+)?\/).*$/, "$1")+
      parameters.base+"/";

    var args = {
      httpbase: httpbase,
      __oDbg: { log: function(a){flashJingleService.log(a)}},
      _oDbg: {log: function(a) {
          window.console ? console.info(a) : dump(a+"\n")
      }},
      timerval: 2000
    };

    this._conObj = parameters.polling ? new JSJaCHttpPollingConnection(args) :
      new JSJaCHttpBindingConnection(args);

    var me = this;

    this._conObj.registerHandler("message", function(p){me._onMessage(p)});
    this._conObj.registerHandler("presence", function(p){me._onPresence(p)});
    this._conObj.registerHandler("iq", function(p){me._onIQ(p)});
    this._conObj.registerHandler("onconnect", function(p){me._onConnect(p)});
    this._conObj.registerHandler("onresume", function(p){me._onConnect(p)});
    this._conObj.registerHandler("ondisconnect", function(p){me._onDisconnect(p)});
    this._conObj.registerHandler("onerror", function(p){me._onError(p)});
    this._conObj.registerHandler("status_changed", function(p){me._onStatusChanged(p)});
    this._conObj.registerHandler("onexception", function(e){debugger});

    args = {
      domain: domain,
      username: user,
      pass: pass,
      resource: parameters.resource || "MUCek",
      xmllang: parameters.lang
    }

    if (!user)
      args.authtype = "saslanon";

    this.xmppObservers = {};
    this.contacts = {};
    this.allContacts = {};
    this.allGroups = {};
    this.groups = {};
    this.resources = {};
    this.defaultGroup = new Model.Group(null, "Contacts", true, -1);
    this.allGroups[""] = this.defaultGroup;

    this.resumed = true;
    if (!(this.resumed = this._conObj.resume()))
      this._conObj.connect(args);
  },

  disconnect: function() {
    if (this._conObj)
      this._conObj.disconnect();
  },

  suspend: function() {
    if (this._conObj)
      this._conObj.suspend();
  },

  setPresence: function(show, status, priority)
  {
      var presence;
      if (show instanceof Object) {
          presence = show;
      } else
          presence = new Model.Presence(show, status, priority);

      if (this.currentPresence && this.currentPresence.show == "unavailable" &&
          presence.show == "unavailable")
          return;

      this._conObj.send(presence.generatePacket());

      this.currentPresence = presence;
      this.myResource.presence = presence;

      this.fireEvent("currentPresenceChanged");
  },

  getOrCreateGroup: function(name) {
    if (!this.allGroups[name||""])
      this.allGroups[name||""] = new Model.Group(name);

    return this.allGroups[name];
  },

  _onMessage: function(pkt) {
    var jid = pkt.getFromJID();

    do {
      var obs = this.xmppObservers[jid];
      if (obs)
        if (obs.onMessage)
          return obs.onMessage(pkt);
        else if (obs.onPacket)
          return obs.onPacket(pkt);
    } while (jid.getResource() && jid.removeResource())
    return 0;
  },

  _onPresence: function(pkt) {
    var jid = pkt.getFromJID();
    do {
      var obs = this.xmppObservers[jid];
      if (obs)
        if (obs.onPresence)
          return obs.onPresence(pkt);
        else if (obs.onPacket)
          return obs.onPacket(pkt);
    } while (jid.getResource() && jid.removeResource())
    return 0;
  },

  _onIQ: function(pkt) {
    var jid = pkt.getFromJID();
    var query = pkt.getNode().childNodes;

    for (var i = 0; i < query.length; i++)
      if (query[i].nodeType == 1) {
        query = query[i];
        break;
      }
    if (i == query.length)
      query = null;

    if (query && this.xmppServices[query.namespaceURI])
        return this.xmppServices[query.namespaceURI](pkt, query);

    do {
      var obs = this.xmppObservers[jid];
      if (obs)
        if (obs.onIQ)
          return obs.onIQ(pkt);
        else if (obs.onPacket)
          return obs.onPacket(pkt);
    } while (jid.getResource() && jid.removeResource())

    if (!query)
      return 0;

    if (query.namespaceURI == "jabber:iq:roster" &&
        pkt.getFrom() == this.myResource.jid.clone().removeResource().toString())
    {
      var items = query.getElementsByTagName("item");
      for (var i = 0; i < items.length; i++) {
          var jid = items[i].getAttribute("jid");
          if (this.allContacts[jid]) {
              var contact = this.allContacts[jid];
              contact._updateFromServer(items[i]);
          } else
              new Model.Contact(items[i]);
      }

      if (!this.currentPresence) {
        this.setPresence("available");
        this.fireEvent("connected");
      }
      return 1;
    } else if (query.namespaceURI == "http://jabber.org/protocol/disco#info" &&
               pkt.getType() == "get")
    {
      var iq = new JSJaCIQ();
      iq.setIQ(pkt.getFrom(), "result", pkt.getID());
      iq.appendNode("query", {xmlns: "http://jabber.org/protocol/disco#info"}, [
        ["feature", {"var": "http://jabber.org/protocol/disco#info"}],
        ["feature", {"var": "urn:xmpp:jingle:1"}],
        ["feature", {"var": "p1:jingle:transports:rtmp:0"}],
      ]);
      this._conObj.send(iq);
      return 1;
    }
    return 0;
  },

  _onGroupAdded: function(group) {
    this.groups[group.name] = group;
    this.fireEvent("groupsChanged", [group], []);
  },

  _onGroupRemoved: function(group) {
    delete this.groups[group.name];
    this.fireEvent("groupsChanged", [], [group]);

  },

  _onContactAdded: function(contact) {
    this.contacts[contact.jid] = contact;
  },

  _onContactRemoved: function(contact) {
    delete this.contacts[contact.jid];
  },

  _onConnect: function() {
    this.connected = true;
    this.myResource = new Model.MyResource(this._conObj.fulljid);

    this._pingTimer = setInterval(this._ping, 29*1000);

    if (parameters.fullClient || parameters.jingleTest) {
        var pkt = new JSJaCIQ();
        pkt.setIQ(null, 'get');
        pkt.setQuery('jabber:iq:roster');
        this._conObj.send(pkt);

        return;
    }
    this.setPresence("available");

    this.fireEvent("connected");
  },

  _onDisconnect: function() {
    if (this._pingTimer)
      clearInterval(this._pingTimer);
    this._pingTimer = null;
    this.connected = false;
    this.fireEvent("disconnected");
  },

  _ping: function() {
    Model.Connection._conObj._sendRaw(" ");
  },

  _onError: function(pkt) {
    this.fireEvent("errorCatched", pkt);
  },

  _onStatusChanged: function() {
  }
}))();

Model.Conference = function(jid) {
  this.jid = jid;
  this.members = {};
  this.allMembers = {};
  this.membersByBareRealJID = {};
  this.membersByID = {};
  this.messages = [];
  this.invitationCallbacks = {};
  this.blockedWords = {};
  this.mutedUsers = {};

  this.addEvents({
    membersChanged: true,
    memberRenamed: true,
    messageAdded: true,
    joinStatusChanged: true,
    muteStateChanged: true,
    wordBlockingStateChanged: true
  });
}
Ext.extend(Model.Conference, Ext.util.Observable, {
  join: function(nick, password) {
    if (this.joined || this.joinInProgress)
      return;

    Model.Connection.xmppObservers[this.jid] = this;

    this.nick = nick;
    this.joinInProgress = true;
    var pkt = new JSJaCPresence();
    pkt.setTo(this.jid + "/" + nick);

    if (parameters.lang)
      pkt.getNode().setAttribute("xml:lang", parameters.lang)

    pkt.appendNode("x", {xmlns: "http://jabber.org/protocol/muc"},
                   password ? [["password", password]] : []);

    Model.Connection._conObj.send(pkt);
  },

  part: function() {
    if (!this.joined && !this.joinInProgress)
      return;

    this.joined = this.joinInProgress = false;

    var pkt = new JSJaCPresence();
    pkt.setTo(this.jid + "/" + nick);
    pkt.setShow("unavailable");
    Model.Connection._conObj.send(pkt);

    this.fireEvent("membersChanged", this, this._cleanup([]), []);
  },

  onMessage: function(pkt) {
    var x = pkt.getChild("x", "http://jabber.org/protocol/muc#user");

    if (x) {
      var decline = x.getElementsByTagName("decline").item(0);
      if (decline) {
        var callback = this.invitationCallbacks[decline.getAttribute("from")];
        delete this.invitationCallbacks[decline.getAttribute("from")];
        if (callback) {
          var reason = decline.getElementsByTagName("reason").item(0);
          callback[0].call(callback[1], this, "decline", reason && reason.firstChild.nodeValue);
        }
        return;
      }

      var invite = x.getElementsByTagName("invite").item(0);
      if (invite && pkt.getType() == "error") {
        var callback = this.invitationCallbacks[invite.getAttribute("to")];
        delete this.invitationCallbacks[invite.getAttribute("to")];
        if (callback)
          callback[0].call(callback[1], this, "error");
        return;
      }
    }

    if (!pkt.getBody())
      return;

    var msg = new Message(pkt, this);

    this.deliverMessage(msg);
  },

  sendMessage: function(body) {
    var pkt = new JSJaCMessage();
    pkt.setBody(body);
    pkt.setTo(this.jid)
    pkt.setType("groupchat");
    Model.Connection._conObj.send(pkt);
  },

  invite: function(who, reason, fun, scope) {
    var pkt = new JSJaCMessage();
    var ns = "http://jabber.org/protocol/muc#user";

    pkt.setTo(this.jid)

    var x = pkt.appendNode("x", {xmlns: ns});
    var invite = x.appendChild(pkt.buildNode("invite", {xmlns: ns, to: who}));

    if (reason)
      invite.appendChild(pkt.buildNode("reason", {xmlns: ns}, reason));

    if (fun)
      this.invitationCallbacks[who] = [fun, scope];

    Model.Connection._conObj.send(pkt);
  },

  kick: function(nick, reason) {
    this.setRole("none", null, nick, reason);
  },

  ban: function(jid, reason) {
    this.setAffiliation("outcast", jid, null, reason);
  },

  setRole: function(role, jid, nick, reason) {
    var ns = "http://jabber.org/protocol/muc#admin";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "set");

    var itemAttrs = {xmlns: ns, role: role};

    if (jid)
      itemAttrs.jid = jid;
    if (nick)
      itemAttrs.nick = nick;

    var item = iq.appendNode("query", {xmlns: ns},
                             [["item", itemAttrs,
                               [reason ? ["reason", {xmlns: ns}, [reason]] : []]]]);

    Model.Connection._conObj.send(iq);
  },

  setAffiliation: function(affiliation, jid, nick, reason) {
    this.changeAffiliationList({affiliation: affiliation, jid: jid,
                               nick: nick, reason: reason});
  },

  changeAffiliationList: function(list) {
    var ns = "http://jabber.org/protocol/muc#admin";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "set");

    var items = [];
    for (var i = 0; i < list.length; i++) {
      var attrs = {xmlns: ns, affiliation: list[i].affiliation};
      if (list[i].jid)
        attrs.jid = list[i].jid;
      if (list[i].nick)
        attrs.nick = list[i].nick;
      items.push(["item", attrs,
                  [list[i].reason ? ["reason", {xmlns: ns}, [list[i].reason]] : []]]);
    }

    if (items.length)
      iq.appendNode("query", {xmlns: ns}, items);

    Model.Connection._conObj.send(iq);
  },

  retrieveAffiliationList: function(affiliation, callback) {
    var ns = "http://jabber.org/protocol/muc#admin";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "get");

    var itemAttrs = {xmlns: ns};

    if (affiliation)
      itemAttrs.affiliation = affiliation;

    var item = iq.appendNode("query", {xmlns: ns},
                             [["item", itemAttrs, []]]);

    Model.Connection._conObj.send(iq,
      this._processAffiliaitonList.createDelegate(this), callback);
  },

  _processAffiliaitonList: function(pkt, callback) {
    var items = [];
    var itemEls = pkt.getNode().getElementsByTagName("item");

    for (var i = 0; i < itemEls.length; i++) {
      var reason = itemEls[i].getElementsByTagName("reason")[0];
      items.push({
        affiliation: itemEls[i].getAttribute("affiliation"),
        jid: itemEls[i].getAttribute("jid"),
        nick: itemEls[i].getAttribute("nick"),
        reason: reason && reason.firstChild && reason.firstChild.nodeValue
      });
    }

    callback(items);
  },

  retrieveBlockedWords: function() {
    var ns = "http://process-one.net/af83/words";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "get");

    var item = iq.appendNode("query", {xmlns: ns});

    Model.Connection._conObj.send(iq,
      this._processBlockedWords.createDelegate(this));
  },

  _processBlockedWords: function(pkt) {
    if (pkt.getType() != "result")
      return;

    var itemEls = pkt.getNode().getElementsByTagName("item");

    for (var i = 0; i < itemEls.length; i++)
      this.blockedWords[itemEls[i].getAttribute("word").toLowerCase()] = 1;
  },

  retrieveMutedUsers: function() {
    var ns = "http://process-one.net/af83/mute";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "get");

    var item = iq.appendNode("query", {xmlns: ns});

    Model.Connection._conObj.send(iq,
      this._processMutedUsers.createDelegate(this));
  },

  _processMutedUsers: function(pkt) {
    if (pkt.getType() != "result")
      return;

    var itemEls = pkt.getNode().getElementsByTagName("item");

    for (var i = 0; i < itemEls.length; i++)
      this._pushMuteChange(itemEls[i].getAttribute("jid"), true)
  },

  onIQ: function(pkt) {
    var el;

    if (pkt.getFrom() == this.jid && pkt.getType() == "set") {
      if ((el = pkt.getChild("blocked", "http://process-one.net/af83/words"))) {
        var word = el.getAttribute("word").toLowerCase();
        this.blockedWords[word] = 1;
        this.fireEvent("wordBlockingStateChanged", word, true);
      } else if ((el = pkt.getChild("unblocked", "http://process-one.net/af83/words"))) {
        var word = el.getAttribute("word").toLowerCase();
        delete this.blockedWords[word];
        this.fireEvent("wordBlockingStateChanged", word, false);
      } else if ((el = pkt.getChild("muted", "http://process-one.net/af83/mute"))) {
        this._pushMuteChange(el.getAttribute("jid"), true);
      } else if ((el = pkt.getChild("unmuted", "http://process-one.net/af83/mute"))) {
        this._pushMuteChange(el.getAttribute("jid"), false);
      }
    }
  },

  _pushMuteChange: function(jid, state) {
    if (state)
      this.mutedUsers[jid] = 1;
    else
      delete this.mutedUsers[jid];

    if (this.membersByBareRealJID[jid])
      for (m in this.membersByBareRealJID[jid])
        this.membersByBareRealJID[jid][m].muteStateChange(state);
  },

  wordIsBlocked: function(word) {
    return word.toLowerCase() in this.blockedWords;
  },

  blockWord: function(word) {
    var ns = "http://process-one.net/af83/words";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "set");

    word = word.toLowerCase();
    this.fireEvent("wordBlockingStateChanged", word, true);

    var item = iq.appendNode("block", {xmlns: ns, word: word});
    this.blockedWords[word] = 1;

    Model.Connection._conObj.send(iq);
  },

  unblockWord: function(word) {
    var ns = "http://process-one.net/af83/words";

    var iq = new JSJaCIQ();
    iq.setIQ(this.jid, "set");

    word = word.toLowerCase();
    this.fireEvent("wordBlockingStateChanged", word, false);

    var item = iq.appendNode("unblock", {xmlns: ns, word: word});
    delete this.blockedWords[word];

    Model.Connection._conObj.send(iq);
  },

  deliverMessage: function(msg) {
    this.messages.push(msg);
    this.fireEvent("messageAdded", this, msg);
  },

  clearMessages: function() {
    this.messages = [];
  },

  _cleanup: function(res)
  {
    for (var i in this.members) {
      this.members[i]._cleanup();
      if (res)
        res.push(this.members[i]);
    }
    this.members = {};
    this.allMembers = {};
    this.invitationCallbacks = {};
    this.nick = null;

    delete Model.Connection.xmppObservers[this.jid];

    return res;
  },

  _memberEnters: function(member) {
    this.members[member.nick] = member;
    this.membersByID[member.id] = member;

    if (member.bareRealJID) {
      if (!this.membersByBareRealJID[member.bareRealJID])
        this.membersByBareRealJID[member.bareRealJID] = {};
      this.membersByBareRealJID[member.bareRealJID][member.realJID.getResource()] = member;
    }
    this.fireEvent("membersChanged", this, [member], []);
    if (!this.joinInProgress)
      this.deliverMessage(new Message(null, this, member.nick,
        _("{0} has joined this room", member.nick), null, false, true));
  },

  _memberLeaves: function(member, partMsg, partial) {
    delete this.members[member.nick];
    delete this.membersByID[member.id];

    if (member.bareRealJID)
      delete this.membersByBareRealJID[member.bareRealJID][member.realJID.getResource()];

    this.fireEvent("membersChanged", this, [], [member.nick]);

    var msg = partMsg ? partial ?
        _("{0} has left this room: {1}", member.nick, partMsg) : partMsg :
      _("{0} has left this room", member.nick);

    this.deliverMessage(new Message(null, this, member.nick,
      msg, null, false, true));
  },

  _memberRename: function(member, oldNick) {
    if (oldNick == this.nick)
      this.nick = member.nick;

    delete this.members[oldNick];
    delete this.allMembers[oldNick];

    this.members[member.nick] = member;
    this.allMembers[member.nick] = member;

    this.fireEvent("memberRenamed", this, member, oldNick);

    this.deliverMessage(new Message(null, this, member.nick,
      _("{0} changed his nick to {1}", oldNick, member.nick), null, false, true));
  },

  onPresence: function(pkt) {
    if (!this.joined && !this.joinInProgress)
      return;

    var resource = pkt.getFromJID().getResource();
    var joinInProgress = this.joinInProgress;

    if (this.joinInProgress && resource == this.nick) {
      joinInProgress = false;
      if (pkt.getType() == "error") {
        var error = pkt.getNode().getElementsByTagName('error')[0];
        this.fireEvent("joinStatusChanged", "error", error && error.getAttribute("code") || 0);
      } else {
        this.joined = true;
        this.fireEvent("joinStatusChanged", "joined");
        this.retrieveBlockedWords();
        this.retrieveMutedUsers();
      }
    }

    if (!this.allMembers[resource]) {
      if (pkt.getType() == "error")
        return;

      if (resource != this.nick) {
        for (var c in this.invitationCallbacks) {
          var callback = this.invitationCallbacks[c];
          delete this.invitationCallbacks[c];

          callback[0].call(callback[1], this, "accepted");
          break;
        }
      }

      this.allMembers[resource] = new Model.ConferenceMember(this, resource);
    }

    this.allMembers[resource].onPresence(pkt);
    this.joinInProgress = joinInProgress;
  }
});

Model.ConferenceMember = function(conference, nick) {
  this.conference = conference;
  this.nick = nick;
  this.show = "unavailable";
  this.status = "";
  this.role = "none";
  this.affiliation = "none";
  this.messages = [];
  this.member = true;
  this.id = ++Model.ConferenceMember.prototype._id;

  this.addEvents({
    presenceInfoUpdated: true,
    messageAdded: true,
    extraActionsUpdated: true,
    muteStateChanged: true
  });
}
Ext.extend(Model.ConferenceMember, Ext.util.Observable, {
  _id: 0,
  onPresence: function(pkt) {
    if (pkt.getType() == "error")
      return;

    var partMsg;
    var x = pkt.getChild("x", "http://jabber.org/protocol/muc#user");
    if (x) {
      var item = x.getElementsByTagName("item").item(0);
      var statusCodes = x.getElementsByTagName("status");

      for (var i = 0; i < statusCodes.length; i++) {
        var code = statusCodes[i].getAttribute("code");
        if (code == 303) {
          var oldNick = this.nick;
          this.nick = item.getAttribute("nick");

          delete Model.Connection.xmppObservers[this.conference.jid+"/"+oldNick];
          Model.Connection.xmppObservers[this.conference.jid+"/"+this.nick] = this;

          this.conference._memberRename(this, oldNick);
          this.fireEvent("presenceInfoUpdated", this);

          return;
        } else if (code == 307 || code == 301) {
          var byWho = item.getElementsByTagName("actor")[0];
          byWho = byWho && byWho.getAttribute("jid");
          var why = item.getElementsByTagName("reason")[0];
          why = why && why.firstChild.nodeValue;

          var str, idx = 0;
          if (this.nick == this.conference.nick)
            str = "You has been ";
          else {
            str = "{0} has been ";
            idx++;
          }
          str += (code == 307 ? "kicked": "banned")+" from the room";

          if (byWho) {
            str += " by {"+idx+"}"
            idx++;
          }
          if (why)
            str += ": {"+idx+"}";

          partMsg = this.nick == this.conference.nick ?
            byWho ?
              String.format(str, byWho, why) :
              String.format(str, why) :
            byWho ?
              String.format(str, this.nick, byWho, why):
              String.format(str, this.nick, why);
        }
      }

      if (item && pkt.getType() != "unavailable") {
        this.role = item.getAttribute("role");
        this.affiliation = item.getAttribute("affiliation");
        this.realJID = item.getAttribute("jid");
        this.realJID = this.realJID && new JSJaCJID(this.realJID);
        this.bareRealJID = this.realJID && this.realJID.clone().removeResource();

        if (this.conference.mutedUsers[this.bareRealJID])
          this.muteStateChange(true);
      }
    }

    var show = pkt.getShow();
    var status = pkt.getStatus();

    if (pkt.getType() == "unavailable") {
      show = "unavailable";
      delete Model.Connection.xmppObservers[this.conference.jid+"/"+this.nick];

      if (partMsg)
        this.conference._memberLeaves(this, partMsg);
      else
        this.conference._memberLeaves(this, status, true);

      if (this.nick == this.conference.nick)
        conferenceClosed(partMsg);
    } else if (this.show == "unavailable") {
      Model.Connection.xmppObservers[this.conference.jid+"/"+this.nick] = this;
      this.conference._memberEnters(this);
    }

    this.show = show;
    this.status = status;

    this.fireEvent("presenceInfoUpdated", this);
    if (this.nick == this.conference.nick)
      for (var m in this.conference.members)
        this.conference.members[m].fireEvent("extraActionsUpdated");
  },

  onMessage: function(pkt) {
    if (!pkt.getBody())
      return;

    var msg = new Message(pkt, this.conference);

    if (pkt.getType() == "groupchat") {
      this.conference.deliverMessage(msg);
      return;
    }
    this.deliverMessage(msg);
  },

  canBeKicked: function() {
    var me = this.conference.members[this.conference.nick];

    if (!me || this == me || me.role != "moderator")
      return false;

    var weight = {owner: 5, admin: 4, member: 3, none: 2, outcast: 1};

    return weight[me.affiliation] > weight[this.affiliation];
  },

  canBeBanned: function() {
    var me = this.conference.members[this.conference.nick];

    if (!me || this == me || (me.affiliation != "owner" && me.affiliation != "admin"))
      return false;

    var weight = {owner: 5, admin: 4, member: 3, none: 2, outcast: 1};

    return weight[me.affiliation] > weight[this.affiliation];
  },

  kick: function(reason) {
    this.conference.kick(this.nick, reason);
  },

  ban: function(reason) {
    this.conference.ban(this.realJID, reason);
  },

  mute: function() {
    this.conference._pushMuteChange(this.bareRealJID, true);
    var ns = "http://process-one.net/af83/mute";

    var iq = new JSJaCIQ();
    iq.setIQ(this.conference.jid, "set");

    var item = iq.appendNode("mute", {xmlns: ns, jid: ""+this.bareRealJID});

    Model.Connection._conObj.send(iq);
  },

  unmute: function() {
    this.conference._pushMuteChange(this.bareRealJID, false);
    var ns = "http://process-one.net/af83/mute";

    var iq = new JSJaCIQ();
    iq.setIQ(this.conference.jid, "set");

    var item = iq.appendNode("unmute", {xmlns: ns, jid: ""+this.bareRealJID});

    Model.Connection._conObj.send(iq);
  },

  muteStateChange: function(muted) {
    if (!this.muted == !muted)
      return;

    this.muted = muted;
    this.fireEvent("muteStateChanged", this, muted);
    this.conference.fireEvent("muteStateChanged", this, muted);
  },

  sendMessage: function(body) {
    var pkt = new JSJaCMessage();
    pkt.setBody(body);
    pkt.setTo(this.conference.jid+"/"+this.nick)
    pkt.setType("chat");
    Model.Connection._conObj.send(pkt);

    this.deliverMessage(new Message(null, this.conference, this.conference.nick, body))
  },

  deliverMessage: function(msg) {
    this.messages.push(msg);
    this.fireEvent("messageAdded", this, msg);
  },

  clearMessages: function() {
    this.messages = [];
  },

  _cleanup: function()
  {
    if (this.show != "unavailable")
      delete Model.Connection.xmppObservers[this.conference.jid+"/"+this.nick];

    this.show = "unavailable";
    this.status = "";
    this.role = "none";
    this.affiliation = "none";
  }
});

function Message(pkt, conf, nick, body, time, offlineMsg, systemMsg) {
  if (pkt) {
    nick = nick || pkt.getFromJID().getResource();
    body = pkt.getBody();

    var blockedWords = pkt.getChild("blockedby", "http://process-one.net/af83/words");
    blockedWords = blockedWords && blockedWords.getAttribute("words");
    if (blockedWords) {
      blockedWords = blockedWords.split(/\s+/);
      this.blockedWords = {};
      for (var i = 0; i < blockedWords.length; i++)
        this.blockedWords[blockedWords[i]] = 1;
    }

    var stamp = pkt.getChild("delay", "urn:xmpp:delay") ||
      pkt.getChild("x", "jabber:x:delay");
    if (stamp) {
      stamp = stamp.getAttribute("stamp").match(/(\d\d\d\d)-?(\d\d)-?(\d\d)T(\d\d):(\d\d):(\d\d)/);
      time = new Date(stamp[1], stamp[2]-1, stamp[3], stamp[4], stamp[5], stamp[6]);
      offlineMsg = true;
    }

    if (!nick || typeof(nick) != "string" && !nick.jid._resource && pkt.getType() == "groupchat")
      systemMsg = true;
  }
  if (typeof(nick) != "string") {
    this.nick = nick.visibleName;
    this.myMsg = nick.representsMe;
  } else {
    this.nick = nick;
    this.myMsg = nick == conf.nick;
  }
  this.body = body;
  this.time = time || new Date();
  this.offlineMsg = offlineMsg;
  this.systemMsg = systemMsg;
}

Model.Group = function(name, visibleName, builtinGroup, sortPriority)
{
  this.name = name;
  this.visibleName = visibleName || name || "XXXunnamed";
  this.contacts = [];
  this.availContacts = 0;
  this.builtinGroup = builtinGroup;
  this.sortPriority = sortPriority || 0;

  if (!builtinGroup)
    Model.Connection.allGroups[name] = this;

  this.addEvents({
    contactsChanged: true,
    availContacts: true
  });
}

Ext.extend(Model.Group, Ext.util.Observable, {
    rename: function(newName)
    {
        this._name = name;
        for (var c = 0; c < this.contacts.length; c++)
            this.contacts[c]._updateRoster();
        delete this._name;
    },

    _clean: function()
    {
        this.contacts = [];
        this.availContacts = 0;
        this.init();
    },

    _onContactUpdated: function(contact, dontNotifyViews)
    {
        var oldAvailCount = this.availContacts;
        this.availContacts = 0;

        for (var c = 0; c < this.contacts.length; c++)
            if (this.contacts[c].activeResource)
                this.availContacts++;

        if (!dontNotifyViews && oldAvailCount != this.availContacts)
            this.fireEvent("availContacts");

        return oldAvailCount != this.availContacts;
    },

    _onContactAdded: function(contact)
    {
        this.contacts.push(contact);
        if (contact.activeResource) {
            this.availContacts++;
            this.fireEvent("contactsChanged", [contact], [])
            this.fireEvent("availContacts")
        } else
            this.fireEvent("contactsChanged", [contact], [])

        if (this.contacts.length == 1)
            Model.Connection._onGroupAdded(this);
    },

    _onContactRemoved: function(contact)
    {
        this.contacts.splice(this.contacts.indexOf(contact), 1);
        if (this._onContactUpdated(contact, true)) {
            this.fireEvent("contactsChanged", [], [contact])
            this.fireEvent("availContacts")
        } else
            this.fireEvent("contactsChanged", [], [contact])

        if (this.contacts.length == 0) {
            Model.Connection._onGroupRemoved(this);
            if (!this.builtinGroup)
                delete Model.Connection.allGroups[this.name];
        }
    }
});

Model.Presence = function(show, status, priority, profile)
{
    if (show instanceof JSJaCPresence) {
        var pkt = show, type = show.getType();
        if (this._showValues[type] === 0)
            this.show = type;
        else
            this.show = pkt.getShow();

        if (!this.show || !(this.show in this._showValues))
            this.show = "available";
        this.status = pkt.getStatus()
        this.priority = pkt.getPriority();
    } else {
        this.show = show;
        if (!this.show || !(this.show in this._showValues))
            this.show = "available";

        this.status = status;
        this.priority = priority == null || isNaN(+priority) ?
            this._priorityMap[this.show] : +priority;
    }

    this.profile = profile;
}

Ext.extend(Model.Presence, Ext.util.Observable, {
    _showValues: {
        available: 1,
        chat: 1,
        dnd: 1,
        away: 1,
        xa: 1,
        unavailable: 0,
        invisible: 0,
        subscribe: 0,
        subscribed: 0,
        unsubscribe: 0,
        unsubscribed: 0
    },

    generatePacket: function(contact)
    {
        var pkt = new JSJaCPresence();
        if (contact)
            pkt.setTo(contact.jid || contact);

        var presence = (this.profile && contact &&
                        this.profile.getPresenceFor(contact)) || this;

        if (this._showValues[presence.show] === 0)
            pkt.setType(presence.show);
        else {
            if (presence.show && presence.show != "available")
                pkt.setShow(presence.show);

            pkt.setPriority(presence.priority == null ?
                            prefManager.getPref("chat.connection.priority") :
                            presence.priority);

            if (0 && account.avatarRetrieved) {
                var photo = pkt.getNode().
                    appendChild(pkt.getDoc().createElementNS("vcard-temp:x:update", "x")).
                    appendChild(pkt.getDoc().createElementNS("vcard-temp:x:update", "photo"));

                if (account.avatarHash)
                    photo.appendChild(pkt.getDoc().createTextNode(account.avatarHash));
            }
            //servicesManager.appendCapsToPresence(pkt.getNode());
        }

        if (presence.status)
            pkt.setStatus(presence.status);

        return pkt;
    },

    equal: function(p)
    {
        return this.show == p.show && this.status == p.status &&
            this.priority == p.priority && this.profile == p.profile;
    },

    weight: function() {
        const show2num = {chat: 0, available: 1, dnd: 2, away:3, xa: 4,
                          unavailable: 5};

        return show2num[this.show||"available"];
    },

    cmp: function(p, comparePriority)
    {

        if (comparePriority)
            if (this.priority != p.priority)
                return p.priority - this.priority;

        return this.weight() - p.weight();
    },

    statusToString: {
        available: _("Available"),
        chat: _("Available for chat"),
        dnd: _("Busy"),
        away: _("Away"),
        xa: _("Not available"),
        unavailable: _("Offline"),
        invisible: _("Invisible")
    },

    toString: function(showStatus, lowerCase)
    {
        var showStr = this.statusToString[this.show];
        if (lowerCase)
            showStr = showStr.toLowerCase();

        return showStr+(showStatus && this.status ? " ("+this.status+")" : "");
    },

    _priorityMap: {
        available: 50,
        chat: 50,
        dnd: 40,
        away: 30,
        xa: 20,
        unavailable: 0
    }
});

Model.Contact = function(jid, name, groups, subscription, subscriptionAsk, newItem)
{
    this.addEvents({
        presenceInfoUpdated: true,
        messageAdded: true,
        resourcesChanged: true
    });

    if (jid instanceof Node) {
        var vals = this._parseNode(jid);
        jid = vals[0];
        name = vals[1];
        subscription = vals[2];
        subscriptionAsk = vals[3];
        groups = vals[4];
    }

    this.jid = new JSJaCJID(jid);
    this.resources = [];
    if (newItem) {
        this._name = name;
        this._groups = groups || [];
        this.newItem = true;
        this.groups = [];
        this.visibleName = name || this.jid;
    } else {
        this.name = name || this.jid;
        this.visibleName = name || this.jid;
        this.subscription = subscription || "none";
        this.subscriptionAsk = !!subscriptionAsk;

        groups = groups || [Model.Connection.defaultGroup];
        this.groups = [];
        for (var i = 0; i < groups.length; i++) {
            var group = typeof(groups[i]) == "string" ?
                Model.Connection.getOrCreateGroup(groups[i]) : groups[i];
            this.groups.push(group);
            group._onContactAdded(this);
        }

        this.newItem = false;
        Model.Connection._onContactAdded(this);
    }

    this.messages = [];

    Model.Connection.xmppObservers[this.jid] = this;
    Model.Connection.allContacts[this.jid] = this;
}

Ext.extend(Model.Contact, Ext.util.Observable, {
    presence: new Model.Presence("unavailable"),

    _updateRoster: function(callback)
    {
        var iq = new JSJaCIQ();
        iq.setType('set');
        var query = iq.setQuery('jabber:iq:roster');
        var item = query.appendChild(iq.getDoc().createElement('item'));
        item.setAttribute('jid', this.jid);

        if (this._subscription != "remove") {
            if (this._name || this.name)
                item.setAttribute('name', this._name || this.name);
            var groups = this._groups || this.groups;
            for (var i = 0; i < groups.length; i++) {
                var groupName = typeof(groups[i]) == "string" ? groups[i] : groups[i]._name || groups[i].name;
                if (!groupName) continue;
                var group = item.appendChild(iq.getDoc().createElement('group'));
                group.appendChild(iq.getDoc().createTextNode(groupName));
            }
            this._inRoster = true;
        } else
            this._inRoster = false;

        if (this._subscription || this.subscription)
            item.setAttribute('subscription', this._subscription || this.subscription);

        delete this._name;
        delete this._subscription;
        delete this._subscriptionAsk;
        delete this._groups;

        Model.Connection._conObj.send(iq, callback);
    },

    _updateFromServer: function(node)
    {
        var groups, groupsHash;
        var canSeeHim = this.canSeeHim;

        var vals = this._parseNode(node, true);
        this.name = vals[1];
        this.subscription = vals[2];
        this.subscriptionAsk = vals[3];
        groups = vals[4];
        groupsHash = vals[5];

        this.name = this.name || this.jid;
        this.visibleName = this.name;
        delete this._inRoster;

        for (var i = 0; i < this.groups.length; i++) {
            if (!(this.groups[i].name in groupsHash)) {
                if (!this._notVisibleInRoster)
                    this.groups[i]._onContactRemoved(this);
            }
            delete groupsHash[this.groups[i].name];
        }

        for (i in groupsHash) {
            if (!this._notVisibleInRoster)
                groupsHash[i]._onContactAdded(this);
        }

        this.groups = groups;

        if (this.subscription == "remove") {
            Model.Connection._onContactRemoved(this);
            delete Model.Connection.allContacts[this.jid]

            this.newItem = true;
            this.fireEvent("newItem");
        } else if (this.newItem) {
            Model.Connection._onContactAdded(this);
            this.newItem = false;
            this.fireEvent("newItem");
        }

        if (this.subscription == "remove") {
            for (i = 0; i < this.resources.length; i++)
                this.resources[i]._remove();
        }

        for (i = 0; i < this.resources.length; i++) {
            this.resources[i].visibleName = this.visibleName+
              (parameters.chatWith ? "" : " ("+this.resources[i].jid.getResource()+")");
            this.resources[i].fireEvent("presenceInfoUpdated");
        }
        this.fireEvent("presenceInfoUpdated");
    },

    _parseNode: function(node, wantGroupsHash)
    {
        jid = node.getAttribute("jid");
        name = node.getAttribute("name");
        subscription = node.getAttribute("subscription") || "none"
        subscriptionAsk = node.getAttribute("ask") == "susbscribe";

        groups = [];
        groupsHash = {};
        var groupTags = node.getElementsByTagName("group");
        for (var i = 0; i < groupTags.length; i++) {
            var groupName = groupTags[i].textContent;
            var group = Model.Connection.getOrCreateGroup(groupName);
            groups.push(group);
            groupsHash[groupName] = group;
        }

        if (groups.length == 0 && subscription != "remove") {
            groups.push(Model.Connection.defaultGroup);
            groupsHash[""] = Model.Connection.defaultGroup;
        }
        return [jid, name, subscription, subscriptionAsk, groups, groupsHash];
    },

    _sendPresence: function(presence)
    {
        Model.Connection._conObj.send(presence.generatePacket(this));
    },

    sendMessage: function(msg, dontDisplay)
    {
        if (!dontDisplay)
          this.deliverMessage(new Message(null, null, Model.Connection.myResource, msg))

        var message = new JSJaCMessage();
        message.setTo(this.jid);
        message.setBody(msg);
        message.setType("chat");

        Model.Connection._conObj.send(message);
    },

    onMessage: function(packet)
    {
        if (packet.getType() == "error")
            return;

        this.deliverMessage(new Message(packet, null, this));
    },

    onPresence: function(packet) {
      var resource = new Model.Resource(packet.getFromJID(), this);

      return resource.onPresence(packet);
    },

    deliverMessage: function(msg) {
      this.messages.push(msg);

      this.fireEvent("messageAdded", this, msg);
    },

    clearMessages: function() {
      this.messages = [];
    },

    subscribe: function(reason, allowToSeeMe)
    {
        if (this.newItem)
            this._updateRoster(this._subscribeStep.
                               createDelegate(this, [reason, allowToSeeMe]));
        else
            this._subscribeStep(null, reason, allowToSeeMe);
    },

    _subscribeStep: function(reason, allowToSeeMe)
    {
        this.askForSubscription(reason);
        if (allowToSeeMe)
            this.allowToSeeMe();
    },

    addToRoster: function()
    {
        if (this.newItem)
            this._updateRoster();
    },

    allowToSeeMe: function()
    {
        this._subscribed = true;
        this._sendPresence(new Model.Presence("subscribed"));
    },

    disallowToSeeMe: function()
    {
        this._sendPresence(new Model.Presence("unsubscribed"));
    },

    askForSubscription: function(reason)
    {
        // TODO use string bundle.
        this._sendPresence(new Model.Presence("subscribe",
            reason || "I would like to add you in my contacts list"));
    },

    rename: function(newName)
    {
        this._name = newName;
        this._updateRoster();
    },

    remove: function()
    {
        this._subscription = "remove";
        this._updateRoster();
    },

    editContact: function(newName, newGroups)
    {
        this._name = newName;
        this._groups = newGroups;
        this._updateRoster();
    },

    _onResourceUpdated: function(resource, dontNotifyViews)
    {
        if (!this.resources.length)
            return false;

        var res = this.activeResource;

        if (resource == this.activeResource) {
            res = this.resources[0];

            for (var r = 1; r < this.resources.length; r++)
                if (res.isLt(this.resources[r]))
                    res = this.resources[r];
        } else if (!this.activeResource || this.activeResource.isLt(resource))
            res = resource;

        if (res != this.activeResource) {
            this.activeResource = res;
            this.presence = res.presence;

            if (!dontNotifyViews)
                this.fireEvent("presenceInfoUpdated");
            return true;
        } else if (!dontNotifyViews && resource == this.activeResource) {
            this.presence = resource.presence;
            this.fireEvent("presenceInfoUpdated");
        }

        return false;
    },

    _onResourceAdded: function(resource)
    {
        var notifyGroups = !this.activeResource;

        this.resources.push(resource);
        if (!this.activeResource || this.activeResource.isLt(resource)) {
            this.activeResource = resource;
            this.presence = resource.presence;

            this.fireEvent("resourcesChanged", [resource], []);
            this.fireEvent("presenceInfoUpdated");
        } else
            this.fireEvent("resourcesChanged", [resource], []);
        if (notifyGroups && !this._notVisibleInRoster)
            for (var g = 0; g < this.groups.length; g++)
                this.groups[g]._onContactUpdated(this);
    },

    _onResourceRemoved: function(resource)
    {
        this.resources.splice(this.resources.indexOf(resource), 1);
        if (!this.resources.length) {
            this.activeResource = null;
            this.presence = new Model.Presence("unavailable");

            this.fireEvent("resourcesChanged", [], [resource]);
            this.fireEvent("presenceInfoUpdated");
            if (!this._notVisibleInRoster)
              for (var g = 0; g < this.groups.length; g++)
                  this.groups[g]._onContactUpdated(this);
            return;
        }
        if (this.activeResource == resource && this._onResourceUpdated(resource, true)) {
            this.presence = new Model.Presence("unavailable");
            this.fireEvent("resourcesChanged", [], [resource]);
            this.fireEvent("presenceInfoUpdated");
        } else
            this.fireEvent("resourcesChanged", [], [resource]);
    }
});

Model.Resource = function(jid, contact)
{
    this.addEvents({
        presenceInfoUpdated: true,
        messageAdded: true
    });

    this.jid = new JSJaCJID(jid);
    this.contact = contact || Model.Connection.allContacts[this.clone().removeResource()];
    this.visibleName = this.contact.visibleName +
      (parameters.chatWith ? "" : " ("+this.jid.getResource()+")");

    this.messages = [];

    Model.Connection.xmppObservers[this.jid] = this;
    Model.Connection.resources[this.jid] = this;
    Model.Connection.fireEvent("resourcesChanged", [this], [])
}

Ext.extend(Model.Resource, Ext.util.Observable, {
    _registered: false,
    presence: new Model.Presence("unavailable"),
    representsMe: false,

    onPresence: function(packet, dontNotifyViews)
    {
        if (packet.getType() == "error") {
            var errorTag = packet.getNode().getElementsByTagName('error')[0];
            if (errorTag) {
                // XXX: I don't think it is ideal solution, maybe show it it roster somehow?
                // XXX: Disabled for now
                var text = 0 && errorTag.getElementsByTagName('text');
                if (text)
                    openDialogUniq("ot:error", "chrome://oneteam/content/error.xul",
                                   "chrome,modal", text.textContent);
                return true;
            }
        }

        var oldPresence = this.presence;
        this.presence = new Model.Presence(packet);
        var equal = this.presence.equal(oldPresence);

        if (packet.getType() == "unavailable")
            this._remove();
        else {
            if (!this._registered)
                this.contact._onResourceAdded(this);
            else
                this.contact._onResourceUpdated(this);
/*
            var avatarHash = packet.getNode().
                getElementsByTagNameNS("vcard-temp:x:update", "photo")[0];
            if (avatarHash)
                this.onAvatarChange(avatarHash.textContent);

            var caps = packet.getNode().
                getElementsByTagNameNS("http://jabber.org/protocol/caps", "c")[0];
            if (caps)
                this.updateCapsInfo(caps);
*/
        }

        if (!dontNotifyViews && !equal)
            this.fireEvent("presenceInfoUpdated");

        this._registered = true;

        return true;
    },

    onAvatarChange: function(avatarHash)
    {
        this.contact.onAvatarChange(avatarHash);
    },

    _remove: function()
    {
        if (this._registered)
            this.contact._onResourceRemoved(this);
        delete Model.Connection.xmppObservers[this.jid];
        delete Model.Connection.resources[this.jid];
        Model.Connection.fireEvent("resourcesChanged", [], [this]);
    },

    sendMessage: function(msg)
    {
        var message = new JSJaCMessage();
        message.setTo(this.jid);
        message.setType("chat");
        if (msg)
            msg.fillPacket(message);

        Model.Connection.connection.send(message);
        this.contact.deliverMessage(new Message(null, null, this.myResource, msg))
    },

    onMessage: function(packet)
    {
        if (packet.getType() == "error")
            return;

        this.contact.deliverMessage(new Message(packet, null, this));
    },

    onAdHocCommand: function()
    {
        openDialogUniq("ot:adhoc", "chrome://oneteam/content/adhoc.xul",
                       "resizable=no,chrome,dialog", this);
    },

    onShowHistory: function()
    {
        Model.Connection.showHistoryManager(this.contact);
    },

    createCompletionEngine: function()
    {
        return Contact.prototype.createCompletionEngine();
    },

    isLt: function(c) {
      return this.cmp(c) < 0;
    },

    cmp: function(c)
    {
        return this.presence.cmp(c.presence, true);
    },

    getStatusIcon: function()
    {
        return Model.Connection.style.getStatusIcon(this);
    }
});

Model.MyResource = function(jid)
{
    this.jid = new JSJaCJID(jid);
    this.visibleName = parameters.chatWith ? "Me" : this.jid.getNode();
}

Ext.extend(Model.MyResource, Ext.util.Observable, {
    presence: new Model.Presence("available"),
    representsMe: true
});

Model.DiscoItem = function(jid)
{
  this.jid = jid;
}

Ext.extend(Model.DiscoItem, Ext.util.Observable, {
    hasDiscoFeature: function(name, forceUpdate, callback)
    {
      return this._requestDiscoInfo(forceUpdate, this._hasDiscoFeature.
                                    createDelegate(this, [name, callback]));
    },

    _hasDiscoFeature: function(name, callback) {
      callback(this, name in this.discoInfo.features);
    },

    _requestDiscoInfo: function(forceUpdate, callback)
    {
      if (!this.discoInfo || forceUpdate) {
        if (!this.discoInfoCallbacks) {
          var iq = new JSJaCIQ();
          iq.setIQ(this.jid, "get");
          iq.setQuery("http://jabber.org/protocol/disco#info");
          Model.Connection._conObj.send(iq, function(pkt, _this) { _this._gotDiscoInfo(pkt) }, this);
          this.discoInfoCallbacks = [callback];
        } else
          this.discoInfoCallbacks.push(callback);

        return;
      }
      callback(discoItem);
    },

    _gotDiscoInfo: function(pkt)
    {
      try{
        var features = pkt.getQuery().getElementsByTagName("feature");
        var identities = pkt.getQuery().getElementsByTagName("identity");
        var cacheVal = "";

        this.discoInfo = { identities: [], features: {} };

        if (identities.length)
            for (var i = 0; i < identities.length; i++) {
                var ident = {
                    name: identities[i].getAttribute("name") || "",
                    type: identities[i].getAttribute("type") || "",
                    category: identities[i].getAttribute("category") || ""
                }
                this.discoInfo.identities.push(ident);
            }

        for (i = 0; i < features.length; i++) {
            var feature = features[i].getAttribute("var");
            this.discoInfo.features[feature] = 1;
        }

        for (i = 0; i < this.discoInfoCallbacks.length; i++)
            this.discoInfoCallbacks[i](this);

        delete this.discoInfoCallbacks;
      }catch(ex){alert(ex+"\n"+ex.stack)}
    }
})

Ext.EventManager.on(window, 'unload', function(){
  if (parameters.chatWith)
    Model.Connection.suspend();
  else
    Model.Connection.disconnect()
});
