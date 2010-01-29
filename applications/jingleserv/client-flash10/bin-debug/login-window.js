Ext.app.LoginManager = {
  showLoginWindow: function(message) {
    if (!this._window)
      this._init();

    if ((parameters.fullClient || parameters.jingleTest) &&
        (parameters.user && parameters.password || parameters.allowAnonymous)) {
      this._elementToMask = viewport.items.first().body;
      this._login();
    } else if (message == null &&
        (parameters.user && parameters.password || parameters.allowAnonymous) &&
        parameters.nick && parameters.room) {
      this._elementToMask = viewport.items.first().body;
      this._login();
    } else if (message) {
      this._showError(message);
    } else if (parameters.chatWith) {
      if (!parameters.room) {
        this._elementToMask = viewport.items.first().body;
        this._login();
        return;
      }
      Ext.Msg.show({
        fn: this._startChat,
        scope: this,
        icon: Ext.Msg.QUESTION,
        msg: "Who you want to talk?",
        buttons: {
          ok: "Consultant",
          cancel: "Other users"
        }
      });
    } else {
      this._window.show();
      this._elementToMask = this._window.body;
    }
  },

  _startChat: function(button) {
    if (button == "ok") {
      delete parameters.room;
    } else {
      delete parameters.chatWith;
      if (!parameters.nick) {
        this._window.show();
        this._elementToMask = this._window.body;
        return;
      }
    }

    this._elementToMask = viewport.items.first().body;
    this._login();
  },

  _init: function() {
    this._errorPanel = new Ext.Panel({
      cls: 'errorMessage',
      border: false,
      html: _("Invalid password"),
      hidden: true,
      autoWidth: true
    });
    this._window = new Ext.Window({
      title: _('Login informations'),
      layout: 'table',
      width: 400,
      autoHeight: true,
      items: [
        new Ext.FormPanel({
          labelWidth: 120,
          defaults: {width: 230},
          bodyStyle: 'padding: 10px',
          defaultType: 'textfield',
          items: [
            {
              fieldLabel: _('User name'),
              name: 'user',
              value: parameters.user || "",
              itemCls: (parameters.allowAnonymous || parameters.user) &&
                parameters.userFieldHidden ? "hidden-field" : "",
              allowBlank: false
            },
            {
              fieldLabel: _('Password'),
              inputType: 'password',
              value: parameters.password || "",
              itemCls: (parameters.allowAnonymous || parameters.password) &&
                parameters.passwordFieldHidden ? "hidden-field" : "",
              name: 'password',
              allowBlank: false
            },
            {
              fieldLabel: _('Room'),
              value: parameters.room || "",
              itemCls: parameters.room && parameters.roomFieldHidden ? "hidden-field" : "",
              name: 'room',
              allowBlank: false
            },
            {
              fieldLabel: _('Nick'),
              value: parameters.nick || "",
              itemCls: parameters.nick && parameters.nickFieldHidden ? "hidden-field" : "",
              name: 'nick',
              allowBlank: false
            },
            {
              fieldLabel: _('Room Password'),
              value: parameters.roomPassword || "",
              itemCls: parameters.roomPasswordFieldHidden ? "hidden-field" : "",
              name: 'roomPassword',
              allowBlank: true
            },
            this._errorPanel
          ],
          buttons: [
            {
              text: _("Login"),
              handler: function() {
                Ext.app.LoginManager._login();
              }
            }
          ]
        })
      ],
      listeners: {
        close: function() {
          hideMucek();
        }
      }
    });
    Model.Connection.on("connected", this._loggedIn, this);
    Model.Connection.on("disconnected", this._loggedOut, this);
  },

  _showError: function(text) {
    this._elementToMask.unmask();

    this._window.show();
    this._elementToMask = this._window.body;

    if (parameters.nickFieldHidden)
      this._toggleField("nick", false);
    if (parameters.roomFieldHidden)
      this._toggleField("room", false);
    if (parameters.userFieldHidden)
      this._toggleField("user", false);
    if (parameters.passwordFieldHidden)
      this._toggleField("password", false);

    this._errorPanel.body.update(text);
    this._errorPanel.show();

    this._conference = null;

    Model.Connection.disconnect();
  },

  _showMessage: function(text) {
    this._elementToMask.mask(text, "x-mask-loading");
  },

  _login: function() {
    var user = this._window.find('name', "user")[0].getRawValue();
    var pass = this._window.find('name', "password")[0].getRawValue();

    this._user = user;

    if (this._conference) {
      this._conference._cleanup();
      this._conference = null;
    }

    this._showMessage(_("Authorisation..."));

    Model.Connection.on("errorCatched", this._onError, this);

    Model.Connection.connect(null, user, pass);
  },

  _loggedIn: function() {
    if (parameters.fullClient || parameters.chatWith || parameters.jingleTest) {
      this._elementToMask.unmask();
      if (this._window.rendered && !this._window.hidden)
        this._window.hide();

      if (parameters.fullClient)
        createRoster();
      else if (parameters.chatWith) {
        welcomeMsg = Ext.DomHelper.append(document.body, {
          id: "welcome-msg", tag: "div", cls: "welcome-msg",
          children: ["Hello ! You are in talking to one of our operators. Please type your message."]
        }, true);

        welcomeMsg.timeout = (function() {
          welcomeMsg.fadeOut({duration: 2})
        }).defer(4000);

        startChat();
      } else {
        startJingleTest();
      }
      return;
    }

    Model.Connection.un("errorCatched", this._onError, this);

    this._showMessage(_("Joining Room..."));
    this._conference = new Model.Conference(this._window.find('name', "room")[0].getRawValue());
    this._conference.on("joinStatusChanged", this._joinStatusChanged, this, {single: true})
    this._conference.join(this._window.find('name', "nick")[0].getRawValue(),
                          this._window.find('name', "roomPassword")[0].getRawValue());
  },

  _loggedOut: function() {
    if (parameters.fullClient) {
      rosterClosed();
      return;
    }

    if (!this._conference)
      return;

    this._conference = null;
    conferenceClosed(_("You has been disconnected"));
  },

  _joinStatusChanged: function(status, errorCode) {
    if (status == "joined") {
      this._elementToMask.unmask();

      if (this._window.rendered && !this._window.hidden)
        this._window.hide();

      conferenceCreated(this._conference);
    } else {
      errorCodesMap = {
        401: _("This room requires password"),
        403: _("You are banned from this room"),
        404: _("This room doesn't exist"),
        405: _("This room doesn't exist, and can be created only by administrator"),
        406: _("This room can be accessed only by registered persons"),
        407: _("You are not member of this room"),
        409: _("You nick name is already used, try another nick"),
        503: _("Conference server can't be contacted or room reached maximum number of users")
      };

      this._showError(errorCodesMap[errorCode] || _("Can't join conference room"));

      if (errorCode == 409)
        this._toggleField("nick", true);
    }
  },

  _toggleField: function(name, show) {
    var el = this._window.find("name", name)[0].getEl().parent().parent();
    if (show)
      el.removeClass("hidden-field");
    else
      el.addClass("hidden-field");
  },

  _onError: function(pkt) {
    Model.Connection.un("errorCatched", this._onError, this);
    this._showError(_("Invalid login information"));

    this._toggleField("user", true);
    this._toggleField("password", true);
  }
}
