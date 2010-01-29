Ext.app.BanWindow = {
  showWindow: function(model) {
    model.retrieveAffiliationList("outcast", this._processBanList.createDelegate(this));

    this._model = model;
    var sm = new Ext.grid.CheckboxSelectionModel({width: 20});
    this._grid = new Ext.grid.GridPanel({
      trackMouseOver: true,
      viewConfig: {
        forceFit: true,
        autoFill: true
      },

      sm: sm,
      columns: [
        sm,
        {
          header: "JabberID", dataIndex: "jid", sortable: true
        }, {
          header: _("Reason"), dataIndex: "reason", sortable: true
        }
      ],
      store: new Ext.data.SimpleStore({
        id: "jid",
        fields: ["jid", "reason"]
      }),
      tbar: [
        {
          xtype: "button",
          text: _("Remove Bans"),
          iconCls: "deleteIcon",
          handler: function() {
            var records = this._grid.getSelectionModel().getSelections();
            for (var i = 0; i < records.length; i++)
              this._grid.store.remove(records[i]);
          },
          scope: this
        }, {
          xtype: "button",
          text: _("Add Ban"),
          iconCls: "addIcon",
          handler: function() {
            var store = this._grid.store;

            var win = new Ext.Window({
              title: _("Add Ban"),
              layout: "form",
              bodyStyle: "padding: 10px",
              width: 400,
              height: 160,
              modal: true,
              items: [
                {
                  id: "banJID",
                  xtype: "textfield",
                  fieldLabel: _("Jabber ID")
                }, {
                  id: "banReason",
                  xtype: "textfield",
                  fieldLabel: _("Reason")
                }
              ],
              buttons: [
                {
                  text: _("Add ban"),
                  handler: function() {
                    store.add(new store.recordType({
                      jid: win.findById("banJID").getRawValue(),
                      reason: win.findById("banReason").getRawValue() || null
                    }));
                    win.close();
                  }
                },
                {
                  text: _("Cancel"),
                  handler: function() {
                    win.close();
                  }
                }
              ]
            });
            win.show();
          },
          scope: this
        }
      ],
      buttons: [{
        text: _("Save"),
        iconCls: "saveIcon",
        handler: function() {
          var affs = [];
          var items = this._grid.store.data.items;
          for (var i = 0; i < items.length; i++)
            affs.push({affiliation: "outcast", jid: items[i].data.jid, reason: items[i].data.reason});

alert(uneval(affs));
          //this._model.changeAffiliationList(affs);

          this._win.close();
        },
        scope: this
      }]
    });

    this._win = new Ext.Window({
      title: _("Ban List Editor"),
      activeItem: 0,
      layout: "card",
      width: 500,
      height: 400,
      modal: true,
      items: this._grid
    });

    this._win.show();
  },

  _saveChanges: function() {
  },

  _processBanList: function(items) {
    var records = [];
    for (var i = 0; i < items.length; i++)
      records.push(new this._grid.store.recordType(items[i]));
    this._grid.store.add(records);
  }
};
