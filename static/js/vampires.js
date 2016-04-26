(function () {
  var blankVampireHtml = "" +
    "<tr>" +
      "<td><span class='name name-normal'></span></td>" +
      "<td>" + 
        "<div class='bloodContainer row'></div>" +
        "<div class='willpowerContainer row'></div>" +
        "<div class='row'><div class='col-md-3'><strong>Day: </strong></div><div class='col-md-8'><input type='date' class='vampireDay' /></div></div>" +
        "<div class='row'><div class='col-md-3'><strong>Notes: </strong></div><div class='col-md-8'><textarea class='vampireNotes' /></div></div>" + 
      "</td>" + 
      "<td class='text-center'>" +
          "<span class='saving glyphicon glyphicon-ok' /> " + 
      "</td><td class='text-center'>" +
          "<span class='glyphicon glyphicon-remove delete'></span>" + 
      "</td>" +
    "</td></tr>";

  var rootNode = $("#vampireList tbody");

  var vampires = {};

  var Vampire = function(data, node) {
    var vn = $(blankVampireHtml);
    node.replaceWith(vn);
    node = vn;

    vampires[data.id] = this;
    this.data = data;
    this.node = node;
    this.savingNode = node.find(".saving");
    this.needsUpdate = false;
    this.updating = false;

    //Set up the DOM

    node.attr('vampireId', data.id);
    node.find(".name").html(data.name);
    makeClickableCounterElements(node.find(".bloodContainer"), 'Blood', 'blood', 'blood', 'maxBlood');
    makeClickableCounterElements(node.find(".willpowerContainer"), 'Willpower', 'willpower', 'willpower', 'maxWillpower');
    node.find('.vampireDay').val(data.day);
    node.find('.vampireNotes').val(data.notes);
    node.find('.vampireNotes').trigger("input"); //Resize

    node.updatePolyfill(); //remove this whenever firefox frickin' supports the date element

    //End setting up the DOM

    this.deleteRemote = function() {
      var v = this;
      v.needsUpdate = false;
      $.ajax({
        type: "DELETE",
        url: "vampire/" + v.data.id, 
        contentType: 'application/json; charset=utf-8',
        error: function() {
          setTimeout(function() { v.deleteRemote(); }, 1000);
        }
      });
    }
    this.updateRemote = function() {
      var v = this;
      if (!this.updating) {
        //Needs to do an ajax call
        this.updating = true;
        this.savingNode.addClass("glyphicon-refresh spinning").removeClass("glyphicon-ok");
        $.ajax({
          type: "PUT",
          url: "vampire/" + v.data.id, 
          contentType: 'application/json; charset=utf-8',
          data: JSON.stringify(v.data),
          dataType: "json",
          success: function() { 
            v.updating = false;
            if (v.needsUpdate) { 
              v.needsUpdate = false;
              v.updateRemote(); //Update again with new data.
            } else {
              v.savingNode.addClass("glyphicon-ok").removeClass("glyphicon-refresh spinning");
            }
          },
          error: function() {
            setTimeout(function() { v.updating = false; v.updateRemote(); }, 1000);
          }
        });
      } else {
        this.needsUpdate = true;
      }
    }
  };
  
  var getVampire = function(element) {
    return vampires[$(element).closest('tr').attr('vampireId')];
  }

  var makeClickableCounterElements = function(container, label, className, dataKey, dataMaxKey) {
    container.html("<div class='col-md-3'><strong>" + label + ": </strong></div><div class='col-md-8'><span class='" + className + "-filled-container'></span><span class='" + className + "-unfilled-container'></span><span class='counter-max-container'><span class='" + className + "-plus glyphicon glyphicon-plus counter-plus' /> <span class='" + className + "-minus counter-minus glyphicon glyphicon-minus' /></span></div>");
    var filledContainer = container.find("." + className + "-filled-container");
    var unfilledContainer = container.find("." + className + "-unfilled-container");
    var vamp = getVampire(container);
    for (i = 1; i <= vamp.data[dataKey]; i++) {
      filledContainer.append("<span class='" + className + " " + className + "-filled' num='" + i + "'><span /><span /></span>");
    }
    for (i = vamp.data[dataKey] + 1; i <= vamp.data[dataMaxKey]; i++) {
      unfilledContainer.append("<span class='" + className + " " + className + "-unfilled' num='" + i + "'><span /><span /></span>");
    }
  };

  var makeClickableCounterEvents = function(className, dataKey, maxDataKey) {
    rootNode.on("click", "." + className + "-plus", function(event) {
      var vamp = getVampire(event.currentTarget);
      vamp.data[maxDataKey]++;

      var container = $(event.currentTarget).parent().parent();
      var unfilledContainer = container.find("." + className + "-unfilled-container");
      unfilledContainer.append("<span class='" + className + " " + className + "-unfilled' num='" + vamp.data[maxDataKey] + "'><span /><span /></span>");
      
      vamp.updateRemote();      
    });

    rootNode.on("click", "." + className + "-minus", function(event) {
      var vamp = getVampire(event.currentTarget);

      var container = $(event.currentTarget).parent().parent();
      container.find("." + className).each(function() { 
        if ($(this).attr('num') == vamp.data[maxDataKey]) {
          $(this).remove();
        }
      });
      
      vamp.data[maxDataKey]--;
      vamp.data[dataKey] = Math.min(vamp.data[dataKey], vamp.data[maxDataKey]);
      vamp.updateRemote();      
    });

    rootNode.on("click", "." + className, function(event) {
      var container = $(event.currentTarget).parent().parent();
      var filledContainer = container.children("." + className + "-filled-container");
      var unfilledContainer = container.children("." + className + "-unfilled-container");
      var vamp = getVampire(event.currentTarget);
      var newCount = Number($(event.currentTarget).attr('num'));
      var oldCount = vamp.data[dataKey];
      if (newCount > oldCount) {
        //Remove unfilled, add them to filled
        vamp.data[dataKey] = newCount;
        var n = newCount - oldCount;
        var cs = unfilledContainer.children().slice(0, n).detach();
        cs.removeClass(className + "-unfilled").addClass(className + "-filled");
        filledContainer.append(cs);
      } else {
        vamp.data[dataKey] = newCount - 1;
        var n = oldCount - newCount + 1; //Because if we select an already-filled element, we want to get rid of it.
        var cs = filledContainer.children().slice(-n).detach();
        cs.removeClass(className + "-filled").addClass(className + "-unfilled");
        unfilledContainer.prepend(cs);
      } 
      //Save to DB
      vamp.updateRemote();
    });
  };

  var saveEditedName = function(event) {
    var vamp = getVampire(event.target);
    var newName = $(event.target).val();
    $(event.target).replaceWith("<span class='name name-normal'>" + newName + "</span>");
    vamp.data.name = newName;
    vamp.updateRemote();
  }

  var makeEditableNames = function() {
    rootNode.on("click", ".name-normal", function(event) {
      var vamp = getVampire(event.target);
      console.log(vamp);
      var container = $(event.target).parent();
      $(event.target).replaceWith("<input type='text' class='name name-edit' />");
      container.children().val(vamp.data.name);
      container.children().focus();
    });

    rootNode.on("keydown", ".name-edit", function(event) {
      if (event.which == 13) { //enter 
        saveEditedName(event);
      }
    });

    rootNode.on("blur", ".name-edit", function(event) {
      saveEditedName(event);
    });
  };

  var makeDeleteEvents = function() {
    rootNode.on("click", ".delete", function(event) {
      if (confirm("Are you sure you want to delete this vampire?")) {
        var vamp = getVampire(event.target);
        vamp.deleteRemote();
        vamp.updateRemote = function() { }; //Disable in case of clicks while animating
        $(event.target).closest("tr").addClass("removed"); //For animation purposes
        $(event.target).find("*").attr("disabled", "disabled");
        setTimeout(function() { $(event.target).closest("tr").remove(); }, 1000);
      }
    });
  };

  var makeCreateEvent = function() {
    $(".create-button").on("click", function(event) {
      rootNode.append("<tr><td></td><td><div class='text-center'><span class='glyphicon glyphicon-refresh spinning' /></div></td></tr>");
      var vn = rootNode.children().last();
      $.getJSON("vampire", function(data) {
        new Vampire(data, vn);
      });
    });
  }
  
  //Init the DOM and get data
  $.getJSON("vampires", function(data) {
    rootNode.html("");
    $.each(data, function(key, value) {
      rootNode.append("<tr />");
      var vn = rootNode.children().last();
      new Vampire(value, vn); //Handles the DOM too
    });
  });

  makeClickableCounterEvents('blood', 'blood', 'maxBlood');
  makeClickableCounterEvents('willpower', 'willpower', 'maxWillpower');
  makeEditableNames();
  makeDeleteEvents();
  makeCreateEvent();
  rootNode.on('change', '.vampireDay', function (event) {
    var vamp = getVampire(event.target);
    if (vamp.data.day != $(event.target).val()) {
      var oldDate = moment(vamp.data.day, "YYYY-MM-DD");
      var newDate = moment($(event.target).val(), "YYYY-MM-DD");
      dd = newDate.diff(oldDate, 'days');
      vamp.data.day = $(event.target).val();

      if (!isNaN(dd) && dd > 0 && vamp.data.blood > 0 && confirm("Take away " + dd + " blood point(s)?")) {
        var toFind = Math.max(0, vamp.data.blood - dd + 1);
        $(event.target).closest("td").find(".blood").each(function() {
          if ($(this).attr('num') == toFind) {
            $(this).trigger('click');
          }
        });
      } else {
        vamp.updateRemote();
      }
    }
  });

  //Set up auto-resizing textarea
  rootNode.on('input', '.vampireNotes', function (event) {
    $(this).outerHeight(12).outerHeight(this.scrollHeight + 6);
    var vamp = getVampire(event.target);
    if (vamp.data.notes != $(event.target).val()) { //In case we call .trigger('input') and no change
      vamp.data.notes = $(event.target).val();
      vamp.updateRemote();
    }
  }); 
})();
