(function () {
  var blankVampireHtml = "" +
    "<tr>" +
      "<td><span class='name name-normal'></span></td>" +
      "<td>" + 
        "<div class='bloodContainer'></div>" +
        "<div class='willpowerContainer'></div>" +
        "<div><strong>Day: </strong><input type='date' class='vampireDay' /></div>" +
        "<div><strong>Notes: </strong><textarea class='vampireNotes' /></div>" + 
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
    makeClickableCounterElements(node.find(".bloodContainer"), 'Blood', 'blood', 'blood');
    makeClickableCounterElements(node.find(".willpowerContainer"), 'Willpower', 'willpower', 'willpower');
    node.find('.vampireDay').val(data.day);
    node.find('.vampireNotes').val(data.notes);
    node.find('.vampireNotes').trigger("input"); //Resize

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
            v.savingNode.html("Error saving! Trying again.");
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

  var makeClickableCounterElements = function(container, label, className, dataKey) {
    container.html("<strong>" + label + ": </strong><span class='" + className + "-filled-container'></span><span class='" + className + "-unfilled-container'></span>");
    var filledContainer = container.children("." + className + "-filled-container");
    var unfilledContainer = container.children("." + className + "-unfilled-container");
    var vamp = getVampire(container);
    var counter = vamp.data[dataKey];
    var MAX = 10;
    for (i = 1; i <= counter; i++) {
      filledContainer.append("<span class='" + className + " " + className + "-filled' num='" + i + "'></span>");
    }
    for (i = counter + 1; i <= MAX; i++) {
      unfilledContainer.append("<span class='" + className + " " + className + "-unfilled' num='" + i + "'></span>");
    }
  };

  var makeClickableCounterEvents = function(className, dataKey) {
    rootNode.on("click", "." + className, function(event) {
      var container = $(event.target).parent().parent();
      var filledContainer = container.children("." + className + "-filled-container");
      var unfilledContainer = container.children("." + className + "-unfilled-container");
      var vamp = getVampire(event.target);
      var newCount = Number($(event.target).attr('num'));
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

  makeClickableCounterEvents('blood', 'blood');
  makeClickableCounterEvents('willpower', 'willpower');
  makeEditableNames();
  makeDeleteEvents();
  makeCreateEvent();
  rootNode.on('change', '.vampireDay', function (event) {
    var vamp = getVampire(event.target);
    if (vamp.data.day != $(event.target).val()) {
      vamp.data.day = $(event.target).val();
      vamp.updateRemote();
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
