var conference;

var mainTab, tabsPanel, rosterView;

function conferenceCreated(conference) {
  _cleanup();

  mainTab = new Ext.app.ChatTab();
  tabsPanel = new Ext.TabPanel({
    region: 'center',
    autoScroll: true,
    activeTab: 0,
    split: true,
    items:[
      mainTab
    ]
  });
  rosterView = new Ext.app.ConferenceTree(mainTab, {region: "center"});

  viewport.add(new Ext.Panel({
    region: 'west',
    collapsible: true,
    title: 'Roster',
    layout: 'border',
    width: 150,
    split: true,
    items: [
      rosterView
    ]
  }), tabsPanel);
  viewport.setLayout(new Ext.layout.BorderLayout());
  viewport.doLayout();

  rosterView.setConference(conference);
  mainTab.changeModel(conference);
  viewport.doLayout();
}

function _cleanup() {
  if (rosterView && rosterView.setConference)
    rosterView.setConference(null);
  if (mainTab)
    mainTab.changeModel(null);

  while (viewport.items.first())
    viewport.remove(viewport.items.first(), true);
}

function conferenceClosed(message) {
  _cleanup();
  viewport.doLayout();

  if (parameters.hideOnClose)
    hideMucek();
  else
    Ext.app.LoginManager.showLoginWindow(message || "");
}

function hideMucek() {
  if (window == window.top)
    return;

  var el = window.frameElement;
  if (el)
    el.parentNode.removeChild(el);
}

Ext.app.SearchField = Ext.extend(Ext.form.TriggerField, {
  validationEvent: false,
  validateOnBlur: false,
  triggerClass: 'x-form-clear-trigger',

  onTriggerClick: function() {
  }
});

function createRoster() {
  _cleanup();

  tabsPanel = new Ext.TabPanel({
    region: 'center',
    autoScroll: true,
    activeTab: 0,
    split: true
  });
  rosterView = new Ext.app.RosterTree({region: "center"});

  Model.Connection.on("currentPresenceChanged", function() {
    viewport.find('id', "presence-type")[0].
      setValue(Model.Connection.currentPresence.show);
  })

  viewport.add(new Ext.Panel({
    region: 'west',
    collapsible: true,
    title: 'Contacts',
    layout: 'border',
    width: 250,
    split: true,
    items: [
      rosterView,
      {
        xtype: "panel",
        region: "north",
        height: 35,
        autoWidth: true,
        items: [
          {
            id: "presence-type",
            autoWidth: true,
            xtype: "combo",
            style: "margin: 5px 0px",
            displayField: "label",
            valueField: "value",
            editable: false,
            mode: "local",
            typeAhead: true,
            selectOnFocs: true,
            triggerAction: "all",
            value: Model.Connection.currentPresence && Model.Connection.currentPresence.show,
            store: new Ext.data.SimpleStore({
              fields: ["value", "label"],
              data: [["available", "Available"],
                     ["chat", "Available for chat"],
                     ["dnd", "Busy"],
                     ["away", "Away"],
                     ["xa", "Not available"],
                     ["unavailable", "Offline"],
                     ["custom", "Custom"]]
            }),
            listeners: {
              select: function(combo, record, index) {
                if (record.data.value == "unavailable")
                  Model.Connection.disconnect();
                else if (record.data.value == "custom") {
                  combo.setValue(Model.Connection.currentPresence.show);

                  win = new Ext.Window({
                    title: _("Custom Presence"),
                    layout: "form",
                    defaults: { bodyStyle: "padding: 10px" },
                    width: 390,
                    height: 190,
                    modal: true,
                    items: [
                      {
                        id: "presence-show",
                        xtype: "combo",
                        width: 250,
                        fieldLabel: "Status",
                        displayField: "label",
                        valueField: "value",
                        editable: false,
                        mode: "local",
                        typeAhead: true,
                        selectOnFocs: true,
                        triggerAction: "all",
                        value: Model.Connection.currentPresence.show,
                        store: new Ext.data.SimpleStore({
                          fields: ["value", "label"],
                          data: [["available", "Available"],
                                 ["chat", "Available for chat"],
                                 ["dnd", "Busy"],
                                 ["away", "Away"],
                                 ["xa", "Not available"]]
                        })
                      },
                      {
                        id: "presence-msg",
                        xtype: "textarea",
                        width: 250,
                        value: "",
                        fieldLabel: _("Message")
                      }
                    ],
                    buttons: [
                      {
                        text: _("OK"),
                        handler: function() {
                          Model.Connection.setPresence(
                            win.findById("presence-show").getValue(),
                            win.findById("presence-msg").getRawValue());
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
                } else
                  Model.Connection.setPresence(record.data.value);
              }
            }
          }
        ]
      }
    ]
  }),
  tabsPanel);
  if (parameters.jingleTest)
    viewport.add(new Ext.Panel({
      xtype: 'panel',
      region: 'south',
      contentEl: "FlashyJingle"
    }));
  viewport.setLayout(new Ext.layout.BorderLayout());
  viewport.doLayout();
}

function rosterClosed(message) {
  _cleanup();
  viewport.doLayout();

  if (parameters.hideOnClose)
    hideMucek();
  else
    Ext.app.LoginManager.showLoginWindow(message || "");
}

function startChat() {
  _cleanup();

  if (Model.Connection.resumed) {
    clearTimeout(welcomeMsg.timeout);
    welcomeMsg.remove();
  }

  var contact = new Model.Contact(parameters.chatWith, "Peer", [], false, false, true);

  mainTab = new Ext.app.ChatTab(contact);
  contact.on("messageAdded", function (c, m) {
    if (!mainTab.notFirstMsg) {
      mainTab.notFirstMsg = 1;
      contact.sendMessage("Chat initiated on page: "+parameters.parentUrl, true);
    }
    mainTab.showMessage(m)
  });

  Ext.EventManager.on(window, 'unload', function(){
    sessionStorage.mucekMessages = uneval(contact.messages);
  });

  viewport.add(mainTab);
  viewport.setLayout(new Ext.layout.FitLayout());
  viewport.doLayout();

  viewport.doLayout();

  contact.messages = eval(sessionStorage.mucekMessages||"[]");
  for (var i = 0; i < contact.messages.length; i++)
    mainTab.showMessage(contact.messages[i]);
}

function startJingleTest() {
  var callButtons = {};

  mainTab = new Ext.Panel({
  });
  Model.Connection.on("resourcesChanged", function (a, r) {
    try{
    for (var i = 0; i < a.length; i++) {
      var di = new Model.DiscoItem(a[i].jid);
      di.resource = a[i];
      di.hasDiscoFeature("p1:jingle:transports:rtmp:0", false, function(di, value) {
        if (!value)
          return;
        callButtons[di.jid] = new Ext.Button({
          text: "Call "+di.resource.visibleName,
          handler: function() {
            requestCall(di.jid);
          }
        })
        mainTab.add(callButtons[di.jid]);
        mainTab.doLayout();
      })
    }

    for (var i = 0; i < r.length; i++) {
      if (callButtons[r[i].jid]) {
        mainTab.remove(callButtons[r[i].jid]);
        delete callButtons[r[i].jid];
      }
    }
    }catch(ex){alert(ex+"\n"+ex.stack)}
  });

  viewport.add(mainTab);
  viewport.setLayout(new Ext.layout.FitLayout());
  viewport.doLayout();
  viewport.doLayout();

  connectionStutusChanged(true);
  setOurJid(Model.Connection.myResource.jid);
}

var statusWindow;
var jingleSession;

function requestCall(jid) {
  if (statusWindow)
    statusWindow.close();

  var name = Model.Connection.resources[jid];
  name = name ? name.visibleName : jid;

  statusWindow = new Ext.Window({
    title: "Requesting new call",
    modal: false,
    closable: false,
    items: [
      {
        xtype: "panel",
        html: "You requested call from "+name
      }, {
        xtype: "button",
        text: "Stop calling",
        handler: function() {
          jingleSession.endCall();
        }
      }
    ]
  });
  statusWindow.show();

  this.jingleSession = flashJingleService.initiateCall(jid);
}

function callStarted(jid) {
  if (statusWindow)
    statusWindow.close();

  var name = Model.Connection.resources[jid];
  name = name ? name.visibleName : jid;

  statusWindow = new Ext.Window({
    title: "Call in progress",
    modal: false,
    closable: false,
    items: [
      {
        xtype: "panel",
        html: "You talk with "+name
      }, {
        xtype: "button",
        text: "Stop call",
        handler: function() {
          jingleSession.endCall();
        }
      }
    ]
  });
  statusWindow.show();
}

function newCallRequested(jid, session) {
  if (statusWindow)
    statusWindow.close();

  jingleSession = session;

  var name = Model.Connection.resources[jid];
  name = name ? name.visibleName : jid;

  statusWindow = new Ext.Window({
    title: "Call request",
    modal: false,
    closable: false,
    items: [
      {
        xtype: "panel",
        html: "You got call request from "+name
      },{
        xtype: "button",
        text: "Accept call",
        handler: function() {
          jingleSession.acceptCall();
        }
      }, {
        xtype: "button",
        text: "Deny call",
        handler: function() {
          jingleSession.endCall();
        }
      }
    ]
  });
  statusWindow.show();
}

function callEnded(jid) {
  if (statusWindow)
    statusWindow.close();
  jingleSession = null;
  statusWindow = null;
}

var viewport;
var welcomeMsg;

Ext.onReady(function(){
  var items = [ {
    xtype: 'panel',
    region: 'center'
  }];

  if (parameters.jingleTest && !parameters.fullClient)
    items.push({
      xtype: 'panel',
      region: 'south',
      contentEl: "FlashyJingle"
    });

  viewport = new Ext.Viewport({
    layout: 'border',
    items: items
  });

  Ext.app.LoginManager.showLoginWindow();
});
