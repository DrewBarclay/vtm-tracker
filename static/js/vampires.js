(function () {
  var blankVampireHtml = "" +
    "<li class='list-group-item'>" +
      "<div class='row'>" +
        "<div class='col-md-3'><h4 class='name'></h4></div>" +
        "<div class='col-md-8'>" + 
          "<div class='bloodContainer'></div>" +
          "<div class='willpowerContainer'></div>" +
        "</div>" + 
        "<div class='col-md-1'>" +
          "<div class='saving'></div>" + 
          "<button type='button' class='btn btn-xs btn-danger delete'>Delete</button>" + 
        "</div>" +
      "</div>" +
    "</li>";

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
        this.savingNode.html("Saving...");
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
              v.savingNode.html("Saved!");
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
    return vampires[$(element).closest('li').attr('vampireId')];
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
    $("#vampireList").on("click", "." + className, function(event) {
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
    $(event.target).replaceWith("<h4 class='name'>" + newName + "</h4>");
    vamp.data.name = newName;
    vamp.updateRemote();
  }

  var makeEditableNames = function() {
    $("#vampireList").on("click", ".name", function(event) {
      var vamp = getVampire(event.target);
      console.log(vamp);
      var container = $(event.target).parent();
      $(event.target).replaceWith("<input type='text' class='name-edit' />");
      container.children().val(vamp.data.name);
      container.children().focus();
    });

    $("#vampireList").on("keydown", ".name-edit", function(event) {
      if (event.which == 13) { //enter 
        saveEditedName(event);
      }
    });

    $("#vampireList").on("blur", ".name-edit", function(event) {
      saveEditedName(event);
    });
  };

  var makeDeleteEvents = function() {
    $("#vampireList").on("click", ".delete", function(event) {
      if (confirm("Are you sure you want to delete this vampire?")) {
        var vamp = getVampire(event.target);
        vamp.deleteRemote();
        vamp.updateRemote = function() { }; //Disable in case of clicks while animating
        $(event.target).closest("li").addClass("removed"); //For animation purposes
        setTimeout(function() { $(event.target).closest("li").remove(); }, 500);
      }
    });
  };

  var makeCreateEvent = function() {
    $(".create-button").on("click", function(event) {
      $("#vampireList").append("<li class='list-group-item'>Creating...</li>");
      var vn = $("#vampireList").children().last();
      $.getJSON("vampire", function(data) {
        new Vampire(data, vn);
      });
    });
  }
  
  //Init the DOM and get data
  $.getJSON("vampires", function(data) {
    $("#vampireList").html("");
    $.each(data, function(key, value) {
      $("#vampireList").append("<li />");
      var vn = $("#vampireList").children().last();
      new Vampire(value, vn); //Handles the DOM too
    });
  });

  makeClickableCounterEvents('blood', 'blood');
  makeClickableCounterEvents('willpower', 'willpower');
  makeEditableNames();
  makeDeleteEvents();
  makeCreateEvent();
})();
