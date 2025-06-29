mod Examples.Asteroids {

use Graphics.Action.Draw
use Graphics.Action.Nothing
use Graphics.App
use Graphics.Color
use Graphics.drawRect
use Graphics.drawTextCenter
use Graphics.Key
use Graphics.KeyListener
use Graphics.MouseListener
use Graphics.runApp

pub def main(): Unit \ IO = {
    let height = 800;
    let width = 1200;
    let textColor = Color.Orange;
    let titleFont = {
        size = 90f32,
        color = textColor,
        italic = false,
        bold = true
    };
    let subtitleFont = {
        size = 30f32,
        color = textColor,
        italic = true,
        bold = false
    };
    def init(d) = {
        drawTextCenter({x = width/2, y = 1*height/4}, "~ ~ Astroids ~ ~", titleFont, d);
        drawTextCenter({x = width/2, y = 2*height/4}, "press enter to play", subtitleFont, d)
    };
    runApp(App.mk({
        title = "Asteroids",
        width = width,
        height = height,
        init = init,
        mouseListener = MouseListener.empty(),
        keyListener = KeyListener.mk({
            onPress = event -> match event {
                case Key.Enter => Draw(d -> drawRect({x = 0, y = 0}, {width = width, height = height}, Color.Orange, d))
                case _ => Nothing
            }
        })
    }))
}

}
