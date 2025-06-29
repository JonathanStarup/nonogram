mod Examples.ListenerEvents {

use Graphics.Color
use Graphics.drawRect
use Graphics.drawTextCenter
use Graphics.Key
use Graphics.KeyListener
use Graphics.MouseButton
use Graphics.MouseListener
use Graphics.runApp

pub def main(): Unit \ IO = {
    let height = 200;
    let width = 800;
    let font = {
        size = 20f32,
        color = Color.White,
        italic = false,
        bold = false
    };
    def drawText(s, d) = {
        drawRect({x = 0, y = 0}, {height = height, width = width}, Color.Black, d);
        drawTextCenter({x = width/2, y = height/2}, s, font, d)
    };
    def init(d) = {
        drawText("No Event Yet", d)
    };
    runApp({
        title = "Event Demo",
        width = width,
        height = height,
        init = init,
        mouseListener = MouseListener.mk({
            onPress = event -> Some(d -> drawText(mousePosEventToString("mouse press", event), d)),
            onRelease = event -> Some(d -> drawText(mousePosEventToString("mouse release", event), d)),
            onDrag = event -> Some(d -> drawText("mouse drag: " + pointLikeToString(event), d)),
            onScroll = amount -> Some(d -> drawText("mouse scroll: ${amount#amount}", d))
        }),
        keyListener = KeyListener.mk({
            onPress = event -> Some(d -> drawText("key press: ${event}", d))
        })
    })
}

def mousePosEventToString(prefix: String, event: {but = MouseButton, x = Int32, y = Int32}): String = {
    "${prefix}: ${event#but} ${pointLikeToString(event)}"
}

def pointLikeToString(point: {x = Int32, y = Int32 | r}): String = {
    "(x=${point#x}, y=${point#y})"
}

}
