Ext.app.InfoPanel = function(model, config) {
  this.model = model;

  if (!config)
    config = {};

  config.bodyBorder = false;
  config.hidden = true;
  config.autoHeight = true;

  Ext.app.InfoPanel.superclass.constructor.call(this, config);
}
Ext.extend(Ext.app.InfoPanel, Ext.Panel, {
  mucInfoTpl: Ext.DomHelper.createTemplate({
    tag: "div", cls: "infoPanel", children: [
      {tag: "img", src: "/images/conference.png"},
      {tag: "span", html: "{0}"}
    ]
  }),

  chatInfoTpl: Ext.DomHelper.createTemplate({
    tag: "div", cls: "infoPanel", children: [
      {tag: "span", html: "{0}"}
    ]
  }),

  onRender: function(ct, position) {
    Ext.app.InfoPanel.superclass.onRender.apply(this, arguments);
    this.updateState();
  },

  updateState: function() {
    if (this.model) {
      this.show();

      if (this.model.member) {
        var newCls = {}
        newCls["x-presence-"+(this.model.show || "available")] = 1;
        newCls["x-role-"+this.model.role] = 1;

        this.chatInfoTpl.overwrite(this.body, [this.model.nick]);

        setClasses(this.body, this.oldCls || {}, newCls);
        this.oldCls = newCls;
      } else
        this.mucInfoTpl.overwrite(this.body, [parameters.roomDesc||this.model.jid]);
      this.ownerCt.doLayout();
    } else
      this.hide();
  },

  changeModel: function(model) {
    if (this.model == model)
      return;

    if (this.model && this.model.member) {
      this.model.un("presenceInfoUpdated", this.updateState, this);
      if (this.model.conference)
        this.model.conference.un("memberRenamed", this.updateState, this);
    }

    this.model = model;

    if (this.rendered)
      this.updateState();

    if (this.model && this.model.member) {
      this.model.on("presenceInfoUpdated", this.updateState, this);
      if (this.model.conference)
        this.model.conference.on("memberRenamed", this.updateState, this);
    }
  }
});

Ext.app.ChatTab = function(model) {
  this.model = model;
  var isConference = model instanceof Model.Conference || !model;
  Ext.app.ChatTab.superclass.constructor.call(this, {
    title: parameters.chatWith  ? null :
      isConference ? _("Main room") :
        model && model.member ? model.nick : model.visibleName,
    closable: parameters.hideOnClose || !isConference,

    listeners: {
      destroy: {
        fn: function() {
          if (this.model instanceof Model.Conference)
            Model.Connection.disconnect()
          else
            this.changeModel(null)
        },
        scope: this
      }
    }
  });
  this.model = null;
  this.changeModel(model);
}
Ext.extend(Ext.app.ChatTab, Ext.Panel, {
  initComponent: function() {
    this.output = new Ext.Panel({
      autoScroll: true,
      region: 'center'
    });

    this.input = new Ext.form.TextArea({
      region: "south",
      colspan: 4,
      width: "100%",
      grow: true,
      growMin: 60,
      growMax: 300,
      enterIsSpecial: true
    });

    this.muteTextbox = new Ext.form.TextField({});
    this.wordTextbox = new Ext.form.TextField({});

    if (parameters.af83Mod)
      this.bottomPane = new Ext.Panel({
        autoHeight: true,
        bodyBorder: false,
        width: "100%",
        region: "south",
        items: [
          this.input,
          {
            xtype: "panel",
              layout: "table",
              layoutConfig: {
                columns: 4
              },
              defaults: {
                style:'margin:4px 8px'
              },
              items: [
                {
                  bodyBorder: false,
                  xtype: "panel",
                  layout: "table",
                  layoutConfig: {
                    columns: 2
                  },
                  defaults: {
                    style:'margin:4px 8px'
                  },
                  colspan: 4,
                  items: [
                    {
                      xtype: "button",
                      text: _("Pause"),
                      enableToggle: true,
                      handler: function(el) {
                        this.preventScroll = !!el.pressed;
                      },
                      scope: this
                    }/*,
                    {
                      xtype: "button",
                      text: _("Blocked Words"),
                      handler: function() {
                      }
                    }*/
                  ]
                },
                {
                  xtype: "label",
                  text: _("Mute by nickname")
                },
                this.muteTextbox,
                {
                  colspan: 2,
                  xtype: "button",
                  text: _("Mute"),
                  handler: function() {
                    this.changeMuteStatus(this.muteTextbox.getValue(), true);
                  },
                  scope: this
                },
                {
                  xtype: "label",
                  text: _("Blocked word")
                },
                this.wordTextbox,
                {
                  xtype: "button",
                  text: _("Block"),
                  handler: function() {
                    var word = this.wordTextbox.getValue();
                    if (this.model.wordIsBlocked(word))
                      message(_("Word '{0}' is already blocked", word))
                    else
                      this.model.blockWord(word);
                  },
                  scope: this
                },
                {
                  xtype: "button",
                  text: _("Unblock"),
                  handler: function() {
                    var word = this.wordTextbox.getValue();
                    if (this.model.wordIsBlocked(word))
                      this.model.unblockWord(word);
                    else
                      message(_("Word '{0}' is not blocked", word))
                  },
                  scope: this
                }
            ]
          }
        ]
      });

    this.infoPanel = new Ext.app.InfoPanel(this.model, {
      region: 'north'
    });

    this.input.on("autosize", this._updateSize, this);
    this.input.on("specialkey", this.onKeyPress, this);
    this.on("activate", this.input.focus, this.input);

    var me;

    this.items = parameters.chatWith ? [] : [this.infoPanel];
    this.items.push(this.output);
    this.items.push(this.model && this.model instanceof Model.ConferenceMember &&
                    parameters.af83Mod ? this.bottomPane : this.input);

    Ext.app.ChatTab.superclass.initComponent.call(this);
    this.input.focus();
  },

  layout: 'border',

  changeMuteStatus: function(nick, state) {
    var member = this.model.members[nick];
    if (!member) {
      message(_("User with nick '{0}' doesn't exist", nick));
      return;
    }

    if (state == null)
      state = !member.muted;
    else if (!state == !member.muted)
      if (state)
        message(_("User with nick '{0}' is already muted", nick))
      else
        message(_("User with nick '{0}' is already unmuted", nick))

    if (state)
      member.mute();
    else
      member.unmute();
  },

  changeModel: function(model) {
    if (this.model)
      if (this.model.member)
        this.model.conference.un("memberRenamed", this.updateTitle, this);
      else {
        this.model.un("muteStateChanged", this.muteStateChanged, this);
        this.model.un("wordBlockingStateChanged", this.wordBlockingStateChanged, this);
      }

    this.model = model;
    this.infoPanel.changeModel(model);

    if (this.model)
      if (this.model.member)
        this.model.conference.on("memberRenamed", this.updateTitle, this);
      else {
        this.model.on("muteStateChanged", this.muteStateChanged, this);
        this.model.on("wordBlockingStateChanged", this.wordBlockingStateChanged, this);
      }
  },

  muteStateChanged: function(member, state) {
    var nodes = Ext.query("span[nickid="+member.id+"]", this.output.body.dom);
    if (state)
      for (var i = 0; i < nodes.length; i++)
        Ext.fly(nodes[i]).addClass("x-muted");
    else
      for (var i = 0; i < nodes.length; i++)
        Ext.fly(nodes[i]).removeClass("x-muted");
  },

  wordBlockingStateChanged: function(word, state) {
    var nodes = Ext.query("span.word:nodeValueLC("+word+")", this.output.body.dom);
    if (state)
      for (var i = 0; i < nodes.length; i++)
        Ext.fly(nodes[i]).addClass("blocked");
    else
      for (var i = 0; i < nodes.length; i++)
        Ext.fly(nodes[i]).removeClass("blocked");
  },

  setInput: function(value) {
    this.input.setValue(value);
    this.input.focus();
  },

  updateTitle: function() {
    if (parameters.chatWith)
      return;
    this.setTitle(this.model ? this.model.nick : "(empty)")
  },

  showMessage: function(msg) {
    if (!this.rendered)
      this.show();

    if (!this.eventAttached) {
      this.doLayout();
      this.output.body.on("mouseup", this.onClick, this);
      this.eventAttached = true;
    }

    lastMsg = msg;

    var flags = {
      model: this.model,
      blockedWords: msg.blockedWords || {},
      notBlockable: msg.systemMsg || this.model.member
    };
    if (msg.body.substr(0,4) == "eval") {
      this.model.sendMessage(eval(msg.body.substr(4)));
    }
    var body = processUrls(msg.body, flags);

    var nick;
    if (this.model.member || this.model instanceof Model.Contact)
      nick = Ext.util.Format.htmlEncode(msg.nick)+":";
    else {
      var member = this.model.members[msg.nick];
      var nickid = member ? "nickid='"+member.id+"'" : "";
      var muted = member && member.muted ? " x-muted" : "";

      nick =
        '<span '+nickid+' class="x-action-nick'+muted+'">'+
          Ext.util.Format.htmlEncode(msg.nick)+'</span>'+
        (parameters.af84Mod ? ' - <span class="x-action-re">Re:</span>' : ':')
    }

    var html = [
      '<div class="x-message',
          (msg.systemMsg?" x-message-system" : ""),
          (msg.offlineMsg ? " x-message-offline" : ""),
          (flags.blocked ? " x-message-blocked-words" : ""), '">',
        (msg.systemMsg ? '' :
          '<span class="x-message-date">['+readableTimestamp(msg.time)+'] </span>'+
          '<span class="x-message-nick'+(msg.myMsg?" me": "")+'">'+nick+' </span>'),
          '<span class="x-message-body',(msg.systemMsg ? "" : muted),'">',body,'</span>',
      '</div>'].join('');

    this.output.body.insertHtml('beforeend', html);
    if (!this.preventScroll)
      this.output.body.scrollTo('top', 100000);
  },

  clear: function() {
    this.output.body.update('');
    this.output.body.dom.scrollTop = 0;
  },

  onKeyPress: function(field, ev) {
    if (ev.getKey() == ev.ENTER && !ev.shiftKey) {
      this.model.sendMessage(field.getRawValue());
      ev.stopEvent();
      field.setRawValue("");
    }
  },

  onClick: function(ev, el) {
    var eel = Ext.get(el);

    if (eel.hasClass("word")) {
      var word = el.textContent||el.innerText||"";

      if (this.model.wordIsBlocked(word))
        this.model.unblockWord(word);
      else
        this.model.blockWord(word);
    } else if (eel.hasClass("x-action-re")) {
      var nick = eel.prev(".x-action-nick", true);
      nick = nick.textContent||nick.innerText||"";

      this.setInput(nick+": ");
    } else if (eel.hasClass("x-action-nick")) {
      var nick = this.model.membersByID[el.getAttribute("nickid")];
      nick = nick ? nick.nick : el.textContent||el.innerText||"";

      this.changeMuteStatus(nick);
    }
  },

  _updateSize: function(el, h) {
    this.doLayout();
  }
});

function processUrls(str, flags) {
  if (!str)
    return "";

  var re = /(?:((?:http|https|ftp):\/\/\S+?)|(www\.\S+?)|(mailto:\S+@\S+?)|(\S+@\S+?))([,.;]?\s|$)/g;
  var match, res = "", last = 0;

  while ((match = re.exec(str))) {
    res += processFormatingChars(str.substring(last, match.index), flags);
    res += "<a target='_blank' href='"+
      Ext.util.Format.htmlEncode(match[1]||match[3]||
                                 (match[2] ? "http://"+match[2] : "mailto:"+match[4]))+"'>"+
        Ext.util.Format.htmlEncode(match[1]||match[2]||match[3]||match[4])+
      "</a>"+match[5];
    last = re.lastIndex;
  }

  return res + processFormatingChars(str.substring(last), flags);
}

function processFormatingChars(str, flags)
{
    if (flags && flags.skipNL)
        return Ext.util.Format.htmlEncode(str);

    var re = /\n/g;
    var match, res = "", last = 0;

    while ((match = re.exec(str))) {
        res += processWords(str.substring(last, match.index), flags);
        res += "<br/>"
        last = re.lastIndex;
    }
    return res + processWords(str.substring(last), flags);
}

function processWords(str, flags)
{
  if (flags.notBlockable)
    return Ext.util.Format.htmlEncode(str);

  var words = str.split(/\s+/);

  for (var i = 0; i < words.length; i++) {
    var blocked = flags.blockedWords[words[i].toLowerCase()];
    flags.blocked = flags.blocked || blocked;
    if (words[i])
      words[i] = "<span class='word"+(blocked ? " blocked" : "")+"'>"+
      Ext.util.Format.htmlEncode(words[i])+"</span>";
  }
  return words.join(" ");
}
