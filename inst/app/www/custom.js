// App-level accessibility helper (cross-cutting task). Auto-bundled by golem.
// Gives the untheme::create_field_set search-selection widgets an accessible
// name: the visible label is rendered as a styled pill (an <a>/<label> with the
// "main label" class) that is not programmatically associated with the input, so
// screen readers announce an unlabeled text box. Here we copy each field's label
// text onto its input/dropdown via aria-label. Additive and idempotent.
(function () {
  function labelizeFields(root) {
    var scope = root || document;
    var fields = scope.querySelectorAll(".field");
    for (var i = 0; i < fields.length; i++) {
      var field = fields[i];
      var lab = field.querySelector(".main.label");
      if (!lab) continue;
      var txt = (lab.textContent || "").trim();
      if (!txt) continue;
      var inputs = field.querySelectorAll("input");
      for (var j = 0; j < inputs.length; j++) {
        if (!inputs[j].getAttribute("aria-label")) {
          inputs[j].setAttribute("aria-label", txt);
        }
      }
      var dd = field.querySelector(".ui.dropdown");
      if (dd && !dd.getAttribute("aria-label")) {
        dd.setAttribute("aria-label", txt);
      }
    }
  }

  var pending;
  function schedule() {
    clearTimeout(pending);
    pending = setTimeout(function () { labelizeFields(document); }, 150);
  }

  function start() {
    labelizeFields(document);
    try {
      var mo = new MutationObserver(function (muts) {
        for (var k = 0; k < muts.length; k++) {
          if (muts[k].addedNodes && muts[k].addedNodes.length) { schedule(); return; }
        }
      });
      mo.observe(document.body, { childList: true, subtree: true });
    } catch (e) { /* MutationObserver unsupported: one-shot labelize is fine */ }
  }

  if (window.jQuery) {
    jQuery(document).on("shiny:connected", start);
    jQuery(document).on("shiny:value shiny:recalculated", schedule);
  } else {
    document.addEventListener("DOMContentLoaded", start);
  }
})();
