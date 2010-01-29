Ext.app.ConferenceMemberTreeItem = function(model) {
  Ext.app.ConferenceMemberTreeItem.superclass.constructor.call(this, {
    cls: "x-roster-item",
    uiProvider: Ext.app.ConferenceMemberTreeItemUI
  });

  this._model = model;
  this._model.on('presenceInfoUpdated', this._sync, this);
  this._model.on("messageAdded", this.onMessageAdded, this);
  this._model.on("muteStateChanged", this._sync, this);
  this._model.on("extraActionsUpdated", this._sync, this);
  this._sync();

  this.on("dblclick", function(_this, ev) {
    if (this.ui._muteTimeout) {
      clearTimeout(this.ui._muteTimeout)
      this.ui._muteTimeout = null;
    }
    _this.openTab(true);
  });
}
Ext.extend(Ext.app.ConferenceMemberTreeItem, Ext.tree.TreeNode, {
  openTab: function(activate) {
    if (!this.tab) {
      this.tab = new Ext.app.ChatTab(this._model);
      tabsPanel.add(this.tab);
      this.tab.on("destroy", this.onTabClose, this);
    }
    if (activate)
      tabsPanel.activate(this.tab);
  },

  onTabClose: function()
  {
    this.tab = null;
  },

  onMessageAdded: function(conference, msg) {
    if (!this.tab)
      this.openTab();

    this.tab.showMessage(msg);
    conference.clearMessages();
  },

  _cleanup: function() {
    if (this.tab)
      this.tab.remove();

    this._model.un('presenceInfoUpdated', this._sync, this);
    this._model.un("messageAdded", this.onMessageAdded, this);
    this._model.un("extraActionsUpdated", this._sync, this);
    this._model.on("muteStateChanged", this._sync, this);
  },

  _sync: function() {
    var newCls = {};

    newCls["x-presence-"+(this._model.show || "available")] = 1;
    newCls["x-role-"+this._model.role] = 1;
    if (this._model.canBeKicked && !this._model.canBeKicked() && !this._model.canBeBanned())
      newCls["x-hide-extra-actions"] = 1;
    if (this._model.muted)
      newCls["x-muted"] = 1;

    setClasses(this.ui, this.uiCls||{}, newCls);

    if (this.oldRole != this._model.role) {
      var tree = this.getOwnerTree();
      if (tree)
        tree.changeGroup(this, this.oldRole || "participant",
                         this._model.role || "participant")
      this.oldRole = this._model.role;
    }

    this.uiCls = newCls;

    this.setText(this._model.nick);
  }
});

Ext.app.ConferenceMemberTreeItemUI = function() {
  Ext.app.RosterTreeItemUI.superclass.constructor.apply(this, arguments);
}
Ext.extend(Ext.app.ConferenceMemberTreeItemUI, Ext.tree.TreeNodeUI, {
  render: function() {
    var wasRendered = this.rendered;

    Ext.app.ConferenceMemberTreeItemUI.superclass.render.apply(this, arguments);

    if (!wasRendered) {
      Ext.DomHelper.insertHtml("afterEnd", this.ecNode,
                               ['<img src="', this.emptyIcon,
                                '" class="x-tree-extra-actions" />'].join(""));
      if (parameters.af83Mod)
        Ext.DomHelper.insertHtml("beforeEnd", this.elNode, " - <a href='#' class='x-tree-re-action'>Re:</span>");
    }
  },

  addClass: function(cls) {
    if (this.rendered)
      Ext.app.ConferenceMemberTreeItemUI.superclass.addClass.call(this, cls);
    else {
      clss = this.node.attributes.cls.split(/\s+/);
      for (var i = 0; i < clss.length; i++)
        if (clss[i] == cls)
          return;
      this.node.attributes.cls += " "+cls;
    }
  },

  removeClass: function(cls) {
    if (this.rendered)
      Ext.app.ConferenceMemberTreeItemUI.superclass.removeClass.call(this, cls);
    else {
      clss = this.node.attributes.cls.split(/\s+/);
      for (var i = 0; i < clss.length; i++)
        if (clss[i] == cls) {
          clss.splice(i, 1);
          i--;
        }
      this.node.attributes.cls = clss.join(" ");
    }
  },

  onClick: function(e) {
    if (e.getTarget(".x-tree-re-action", 1)) {
      tabsPanel.getActiveTab().setInput(this.node._model.nick+": ");
      return;
    }
    if (!e.getTarget(".x-tree-extra-actions", 1)) {
      Ext.app.ConferenceMemberTreeItemUI.superclass.onClick.apply(this, arguments);
      if (!this._muteTimeout) {
        var _this = this, model = this.node._model;
        this._muteTimeout = setTimeout(function() {
          _this._muteTimeout = null;
          if (model.muted)
            model.unmute();
          else
            model.mute();
        }, 250)
      }
      return;
    }
    var model = this.node._model;
    win = new Ext.Window({
      title: _("Actions"),
      activeItem: 0,
      layout: "card",
      defaults: { bodyStyle: "padding: 10px" },
      width: 400,
      height: 160,
      modal: true,
      items: [
        {
          xtype: "panel",
          items: [
            {
              xtype: "button",
              text: _("Ban User"),
              disabled: !model.canBeBanned(),
              handler: function() {
                win.buttons[0].show();
                win.getLayout().setActiveItem(1);
              }
            }, {
              xtype: "button",
              text: _("Kick User"),
              disabled: !model.canBeKicked(),
              handler: function() {
                win.buttons[1].show();
                win.getLayout().setActiveItem(2);
              }
            }
          ]
        }, {
          xtype: "form",
          items: [
            {
              id: "banJID",
              xtype: "textfield",
              value: model.realJID.clone().removeResource(),
              fieldLabel: _("Jabber ID")
            }, {
              id: "banReason",
              xtype: "textfield",
              fieldLabel: _("Reason")
            }
          ]
        }, {
          xtype: "form",
          items: [
            {
              id: "kickReason",
              xtype: "textfield",
              fieldLabel: _("Reason")
            }
          ]
        }
      ],
      buttons: [
        {
          text: _("Ban User"),
          hidden: true,
          handler: function() {
            var jid = win.findById("banJID").getRawValue();
            var reason = win.findById("banReason").getRawValue();
            model.conference.ban(jid, reason);
            win.close();
          }
        }, {
          text: _("Kick User"),
          hidden: true,
          handler: function() {
            var reason = win.findById("kickReason").getRawValue();
            model.conference.kick(model.nick, reason);
            win.close();
          }
        }, {
          text: _("Cancel"),
          handler: function() {
            win.close();
          }
        }
      ]
    });
    win.show();
  }
});

var roleWeights = {
  "moderator": 1,
  "participant": 2,
  "visitor": 3,
  "none": 4
};

Ext.app.ConferenceTree = function(tab, conf) {
  this.root = new Ext.tree.TreeNode({
    expanded: true,
    children: []
  });
  conf.bbar = [{
      xtype: "button",
          text: _("Edit Bans List"),
          hidden: true,
          handler: function() {
              Ext.app.BanWindow.showWindow(Ext.app.LoginManager._conference)
          }
  }
  ]

  Ext.app.ConferenceTree.superclass.constructor.call(this, conf)
  this.tab = tab;

  this.sorter = new Ext.tree.TreeSorter(this, {
      sortType: function(node) {
        return node._model ? node._model.nick : roleWeights[node.role];
      }
    });
}
Ext.extend(Ext.app.ConferenceTree, Ext.tree.TreePanel, {
  groupNames: {
    "moderator": _("Moderators"),
    "participant": _("Participants"),
    "visitor": _("Visitors"),
    "none": _("No role assigned")
  },

  initComponent: function() {
    Ext.app.ConferenceTree.superclass.initComponent.call(this);

    this.groups = {};
    this.resources = {};
  },

  setConference: function(conference)
  {
    if (this.conference) {
      this.conference.un("membersChanged", this.onMembersChanged, this);
      this.conference.un("memberRenamed", this.onMemberRenamed, this);
      this.conference.un("messageAdded", this.onMessageAdded, this);
      this.tab.clear();

      var toRemove = [];
      for (var m in this.resources)
        toRemove.push(m);

      this.onMembersChanged(conference, [], toRemove);
      this.resources = {};
    }

    this.conference = conference;

    var members;
    if (conference) {
      this.conference.on("membersChanged", this.onMembersChanged, this);
      this.conference.on("memberRenamed", this.onMemberRenamed, this);
      this.conference.on("messageAdded", this.onMessageAdded, this);
      members = this.conference.members;
    } else {
      members = {};
      this.purgeListeners();
    }

    var toAdd = [];
    for (var m in members) {
      if (!this.resources[members[m].nick])
        toAdd.push(members[m]);
    }

    this.onMembersChanged(conference, toAdd, []);
  },

  changeGroup: function(node, oldGroup, newGroup) {
    if (oldGroup == newGroup)
      return;

    var oldGroupEl = oldGroup ? this.groups[oldGroup] : null;

    if (newGroup)
      if (this.groups[newGroup])
        newGroup = this.groups[newGroup];
      else {
        var role = newGroup;
        newGroup = this.groups[newGroup] = new Ext.tree.TreeNode({
          expanded: true,
          text: this.groupNames[newGroup],
          cls: 'x-roster-group'
        });
        newGroup.role = role;
      }

    if (newGroup) {
      if (!newGroup.hasChildNodes())
        this.root.appendChild(newGroup);
      newGroup.appendChild(node);
    } else if (oldGroup)
      node.remove();

    if (oldGroupEl && !oldGroupEl.hasChildNodes()) {
      oldGroupEl.remove();
      delete this.groups[oldGroup];
    }
  },

  onMembersChanged: function(conference, added, removed) {
    for (var i = 0; i < added.length; i++) {
      var item = new Ext.app.ConferenceMemberTreeItem(added[i]);

      this.changeGroup(item, null, added[i].role || "participant")
      this.resources[added[i].nick] = item;
    }

    for (i = 0; i < removed.length; i++) {
      var node = this.resources[removed[i]];

      this.changeGroup(node, node._model.role || "participant");
      delete this.resources[removed[i]];
      node._cleanup();
    }
  },

  onMemberRenamed: function(conference, member, oldNick) {
    var item = this.resources[member.nick] = this.resources[oldNick];
    delete this.resources[oldNick];

    item._sync();
  },

  onMessageAdded: function(conference, msg) {
    this.tab.showMessage(msg);
    conference.clearMessages();
  },

  rootVisible: false,
  lines: false,
  loader: new Ext.tree.TreeLoader()
});






Ext.app.RosterTree = function(conf) {
  this.root = new Ext.tree.TreeNode({
    expanded: true,
    children: []
  });

  conf.autoScroll = true;

  Ext.app.RosterTree.superclass.constructor.call(this, conf)

  this.sorter = new Ext.tree.TreeSorter(this, {
      sortType: function(node) {
        return node._model instanceof Model.Group ?
          node._model.visibleName :
          node._model.presence.weight()+node._model.visibleName;
      }
    });
}
Ext.extend(Ext.app.RosterTree, Ext.tree.TreePanel, {
  initComponent: function() {
    Ext.app.RosterTree.superclass.initComponent.call(this);

    this.groups = {};
    Model.Connection.on("groupsChanged", this.onGroupsChanged, this);
    for (var i in Model.Connection.groups) {
      this.onGroupsChanged([Model.Connection.groups[i]], []);
    }
  },

  onGroupsChanged: function(added, removed) {
    for (var i = 0; i < added.length; i++) {
      var item = new Ext.app.RosterGroupItem(added[i]);

      this.groups[added[i].visibleName] = item;
      this.root.appendChild(item);
    }

    for (i = 0; i < removed.length; i++) {
      var node = this.groups[removed[i]];

      delete this.groups[removed[i]];
      node._cleanup();
    }
  },

  rootVisible: false,
  lines: false,
  loader: new Ext.tree.TreeLoader()
});




Ext.app.RosterGroupItem = function(model) {
  this._model = model;
  model.on("contactsChanged", this._onContactsChanged, this)

  Ext.app.RosterGroupItem.superclass.constructor.call(this, {
    expanded: true,
    text: model.visibleName,
    cls: 'x-roster-group'
  });

  this.contacts = {};

  if (this._model.contacts.length)
    this._onContactsChanged(this._model.contacts, [])
}
Ext.extend(Ext.app.RosterGroupItem, Ext.tree.TreeNode, {
  _cleanup: function() {
    model.un("contactsChanged", this._onContactsChanged, this)
  },

  _onContactsChanged: function(added, removed) {
    for (var i = 0; i < added.length; i++) {
      var item = new Ext.app.RosterTreeItem(added[i]);

      this.contacts[added[i].jid] = item;
      this.appendChild(item);
    }

    for (i = 0; i < removed.length; i++) {
      var node = this.contacts[removed[i].jid];

      delete this.contacts[removed[i].jid];
      node._cleanup();
    }
  }
});





Ext.app.RosterTreeItem = function(model) {
  Ext.app.RosterTreeItem.superclass.constructor.call(this, {
    cls: "x-roster-item",
    uiProvider: Ext.app.RosterTreeItemUI
  });

  this._model = model;
  this._model.on("presenceInfoUpdated", this._sync, this);
  if (parameters.jingleTest)
    this._model.on("resourcesChanged", this.onResourcesChanged, this);
  this._model.on("messageAdded", this.onMessageAdded, this);
  this._model.on("muteStateChanged", this._sync, this);
  this._model.on("extraActionsUpdated", this._sync, this);
  this._sync();
  this._jingleResources = {};
  this._jingleResourcesCount = 0;

  this.on("dblclick", function(_this, ev) {
    if (this.ui._muteTimeout) {
      clearTimeout(this.ui._muteTimeout)
      this.ui._muteTimeout = null;
    }
    _this.openTab(true);
  });
}
Ext.extend(Ext.app.RosterTreeItem, Ext.tree.TreeNode, {
  openTab: function(activate) {
    if (!this.tab) {
      this.tab = new Ext.app.ChatTab(this._model);
      tabsPanel.add(this.tab);
      this.tab.on("destroy", this.onTabClose, this);
      setTimeout(function(){tabsPanel.doLayout()}, 0)
    }
    if (activate)
      tabsPanel.activate(this.tab);
  },

  onTabClose: function()
  {
    this.tab = null;
  },

  onResourcesChanged: function(a, r) {
    for (var i = 0; i < a.length; i++) {
      var di = new Model.DiscoItem(a[i].jid);
      di.resource = a[i];
      di.rtItem = this;
      di.hasDiscoFeature("p1:jingle:transports:rtmp:0", false, function(di, value) {
        if (!value)
          return;
        di.rtItem._jingleResources[di.resource.jid] = 1;
        if (++di.rtItem._jingleResourcesCount == 1)
          di.rtItem._sync();
      })
    }

    for (var i = 0; i < r.length; i++)
      if (this._jingleResources[r[i].jid]) {
        delete this._jingleResources[r[i].jid];
        if ((--this._jingleResourcesCount) == 0)
          this._sync();
      }
  },

  onMessageAdded: function(conference, msg) {
    if (!this.tab)
      this.openTab();

    this.tab.showMessage(msg);
    conference.clearMessages();
  },

  _cleanup: function() {
    if (this.tab)
      this.tab.remove();

    this._model.un('presenceInfoUpdated', this._sync, this);
    this._model.un("messageAdded", this.onMessageAdded, this);
  },

  _sync: function() {
    var newCls = {};

    newCls["x-presence-"+(this._model.presence.show || "available")] = 1;
    if (this._jingleResourcesCount)
      newCls["x-call-available"] = 1;

    setClasses(this.ui, this.uiCls||{}, newCls);

    if (this.oldRole != this._model.role) {
      var tree = this.getOwnerTree();
      if (tree)
        tree.changeGroup(this, this.oldRole || "participant",
                         this._model.role || "participant")
      this.oldRole = this._model.role;
    }

    this.uiCls = newCls;

    this.setText(this._model.visibleName);
  }
});

Ext.app.RosterTreeItemUI = function() {
  Ext.app.RosterTreeItemUI.superclass.constructor.apply(this, arguments);
}
Ext.extend(Ext.app.RosterTreeItemUI, Ext.tree.TreeNodeUI, {
  render: function() {
    var wasRendered = this.rendered;

    Ext.app.RosterTreeItemUI.superclass.render.apply(this, arguments);

    if (!wasRendered) {
      if (parameters.jingleTest)
        Ext.DomHelper.insertHtml("afterEnd", this.ecNode,
                                 ['<img src="', this.emptyIcon,
                                  '" class="x-tree-call-action" />'].join(""));
      Ext.DomHelper.insertHtml("afterEnd", this.ecNode,
                               ['<img src="', this.emptyIcon,
                                '" class="x-tree-extra-actions" />'].join(""));
    }
  },

  addClass: function(cls) {
    if (this.rendered)
      Ext.app.RosterTreeItemUI.superclass.addClass.call(this, cls);
    else {
      clss = this.node.attributes.cls.split(/\s+/);
      for (var i = 0; i < clss.length; i++)
        if (clss[i] == cls)
          return;
      this.node.attributes.cls += " "+cls;
    }
  },

  removeClass: function(cls) {
    if (this.rendered)
      Ext.app.RosterTreeItemUI.superclass.removeClass.call(this, cls);
    else {
      clss = this.node.attributes.cls.split(/\s+/);
      for (var i = 0; i < clss.length; i++)
        if (clss[i] == cls) {
          clss.splice(i, 1);
          i--;
        }
      this.node.attributes.cls = clss.join(" ");
    }
  },

  onClick: function(e) {
    if (e.getTarget(".x-tree-re-action", 1)) {
      tabsPanel.getActiveTab().setInput(this.node._model.nick+": ");
      return;
    }
    if (e.getTarget(".x-tree-call-action", 1)) {
      var resource;
      for (var i in this.node._jingleResources) {
        requestCall(i);
        break;
      }
      return;
    }
    if (!e.getTarget(".x-tree-extra-actions", 1)) {
      Ext.app.RosterTreeItemUI.superclass.onClick.apply(this, arguments);
      if (!this._muteTimeout) {
        var _this = this, model = this.node._model;
        this._muteTimeout = setTimeout(function() {
          _this._muteTimeout = null;
          if (model.muted)
            model.unmute();
          else
            model.mute();
        }, 250)
      }
      return;
    }
    var model = this.node._model;
    win = new Ext.Window({
      title: _("Actions"),
      activeItem: 0,
      layout: "card",
      defaults: { bodyStyle: "padding: 10px" },
      width: 400,
      height: 160,
      modal: true,
      items: [
        {
          xtype: "panel",
          items: [
            {
              xtype: "button",
              text: _("Change Nick"),
              handler: function() {
                win.buttons[0].show();
                win.buttons[2].show();
                win.getLayout().setActiveItem(1);
              }
            }, {
              xtype: "button",
              text: _("View vCard"),
              handler: function() {
                win.buttons[1].show();
                win.getLayout().setActiveItem(2);
              }
            }
          ]
        }, {
          xtype: "form",
          items: [
            {
              id: "nickName",
              xtype: "textfield",
              value: model.visibleName,
              fieldLabel: _("Nickname")
            }
          ]
        }, {
          xtype: "panel",
          items: [
            {
              id: "kickReason",
              xtype: "textfield",
              fieldLabel: _("Reason")
            }
          ]
        }
      ],
      buttons: [
        {
          text: _("Change Nick"),
          hidden: true,
          handler: function() {
            var nick = win.findById("nickName").getRawValue();
            model.rename(nick);
            win.close();
          }
        }, {
          text: _("OK"),
          hidden: true,
          handler: function() {
            win.close();
          }
        }, {
          hidden: true,
          text: _("Cancel"),
          handler: function() {
            win.close();
          }
        }
      ]
    });
    win.show();
  }
});
