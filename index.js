require("./exports.js");
function ShareData() {
    this.encoder = null;
}

window.doKeyEvent = function(code) {
    console.log('keyevent',code);
};

ShareData.prototype.setMsgEncoder = function(f) {
    this.encoder = f;
};
ShareData.prototype.pokeProgram = function(id,c) {
    var iframe = document.getElementById(id);
    if (iframe) {
        iframe.contentWindow.postMessage({"message":"runme","code":c},"*");
    }
};
ShareData.prototype.resizeWindow = function(id) {
    var self = this;
    var checkSize = function() {
        var editor = document.getElementById(id);
        if (editor) {
            var clientRect = null;
            var parentRect = null;
            if (editor) {
                var parent = editor.parentNode;
                parentRect = editor.getBoundingClientRect();
                var firstRow = editor.childNodes[0];
                var firstRowRect = firstRow.getBoundingClientRect();
                var width = 0;
                for (var i = 0; i < firstRow.childNodes.length; i++) {
                    var eltRect = firstRow.childNodes[i].getBoundingClientRect();
                    width += eltRect.width;
                }
                clientRect = {
                    width: width,
                    height: firstRowRect.height
                };
            }
            var msg = {
                message: "size",
                windowX: parentRect ? parentRect.width : 0,
                windowY: parentRect ? parentRect.height : 0,
                editorX: clientRect ? clientRect.width : 0,
                editorY: clientRect ? clientRect.height : 0
            };
            app.pushMsg(shareData.encoder(msg));
        } else {
            window.setTimeout(checkSize, 200);
        }
    };
    window.setTimeout(checkSize, 200);
};

window.shareData = new ShareData();
window.addEventListener('message', function(m) {
    app.pushMsg(shareData.encoder(m.data));
});

window.addEventListener('resize', function(evt) {
    shareData.resizeWindow('editor');
});

var app = require("./src/demo.bs.js").main(document.getElementById("app"));
