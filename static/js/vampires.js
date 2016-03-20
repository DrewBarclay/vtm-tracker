(function () {
  var blankVampireHtml = "" +
    "<li class='list-group-item'>" +
      "<h4 class='name'></h4>" +
      "<div class='bloodContainer'></div>" +
      "<div class='willpowerContainer'></div>" +
      "<div class='saving'></div>" +
    "</li>";

  var vampires = {};

  var Vampire = function(data, node) {
    this.data = data;
    this.node = node;
    this.savingNode = node.children(".saving");
    this.needsUpdate = false;
    this.updating = false;
    this.updateRemote = function() {
      v = this;
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
        cs = unfilledContainer.children().slice(0, n).detach();
        cs.removeClass(className + "-unfilled").addClass(className + "-filled");
        filledContainer.append(cs);
      } else {
        vamp.data[dataKey] = newCount - 1;
        var n = oldCount - newCount + 1; //Because if we select an already-filled element, we want to get rid of it.
        cs = filledContainer.children().slice(-n).detach();
        cs.removeClass(className + "-filled").addClass(className + "-unfilled");
        unfilledContainer.prepend(cs);
      } 
      //Save to DB
      vamp.updateRemote();
    });
  };
  
  //Init the DOM and get data
  $.getJSON("vampires", function(data) {
    $("#vampireList").html("");
    $.each(data, function(key, value) {
      $("#vampireList").append(blankVampireHtml);
      var vn = $("#vampireList").children().last();
      vampires[key] = new Vampire(value, vn);
      vn.attr('vampireId', key);
      vn.children(".name").html(value.name);
      makeClickableCounterElements(vn.children(".bloodContainer"), 'Blood', 'blood', 'blood');
      makeClickableCounterElements(vn.children(".willpowerContainer"), 'Willpower', 'willpower', 'willpower');
    });

    makeClickableCounterEvents('blood', 'blood');
    makeClickableCounterEvents('willpower', 'willpower');
  });
})();
