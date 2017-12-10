//!javascript

bind("^", function() {
    var adjustment = tabs.current.scrolledWindow.hadjustment;
    adjustment.value = adjustment.lower;
});
bind("$", function() {
    var adjustment = tabs.current.scrolledWindow.hadjustment;
    adjustment.value = adjustment.upper - adjustment.pageSize;
});
